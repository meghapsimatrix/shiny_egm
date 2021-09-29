library(robumeta)
library(clubSandwich)
library(tidyverse)
library(metafor)

tidy_meta <- function(dat, 
                      model, 
                      rho_val){
  
  summary <- dat %>% 
    summarize(n_studies = n_distinct(study_id),
              n_es = n())
  
  m <- summary %>% pull(n_studies)
  n <- summary %>% pull(n_es)
  
  if(m > 2){
    
    if(model == "ce"){
      
      mod <- robu(formula = es ~ 1,
                  data = dat,
                  var.eff.size = var,
                  modelweights = "CORR",
                  rho = rho_val,
                  studynum = study_id)
      
      
      res <- conf_int(mod, vcov = "CR2", tidy = TRUE) %>%
        rename(estimate = beta) %>%
        as_tibble() %>%
        mutate(method = "CE")
      
    } else if(model == "he"){
      
      mod <- robu(formula = es ~ 1,
                  data = dat,
                  var.eff.size = var,
                  modelweights = "HIER",
                  rho = rho_val,
                  studynum = study_id)
      
      
      res <- conf_int(mod, vcov = "CR2", tidy = TRUE) %>%
        rename(estimate = beta) %>%
        as_tibble()  %>%
        mutate(method = "HE")
      
      
    } else if(model == "che"){
    
    # constant sampling correlation working model
    V_mat <- impute_covariance_matrix(dat$var,
                                      cluster = dat$study_id,
                                      r = rho_val, 
                                      smooth_vi = TRUE)
    
    # fit random effects working model in metafor
    mod <- rma.mv(es ~ 1,
                  V = V_mat, 
                  random = ~ 1 | study_id,
                  data = dat,
                  sparse = TRUE)
    
    
    # run RVE
    res <- conf_int(mod, vcov = "CR2", tidy = TRUE) %>%
      as_tibble() %>%
      mutate(method = "CHE") %>%
      rename(estimate = beta)
    
  }
    
  } else{
      
    suppressWarnings(mod <- lm_robust(es ~ 1, data = dat, se_type = "classical"))
    
    res <- tidy(mod) %>%
      select(estimate, SE = std.error, df = df, CI_L = conf.low, CI_U = conf.high) %>%
      mutate(method = "Simple Average")
    
  } 
  
  # figure out how to output if combination has only one effect size  

  
  output <- bind_cols(res, summary) %>%
    mutate_if(is.numeric, round, 3) 
  
  return(output)
}
