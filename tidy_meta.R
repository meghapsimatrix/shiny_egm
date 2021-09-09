library(robumeta)
library(clubSandwich)
library(tidyverse)
library(metafor)

tidy_meta <- function(dat, rho = 0.6){
  
  summary <- dat %>% 
    summarize(n_studies = n_distinct(study_id),
              n_es = n())
  
  m <- summary %>% pull(n_studies)
  
  if(m > 2){
    
    # constant sampling correlation working model
    V_mat <- impute_covariance_matrix(dat$var,
                                      cluster = dat$study_id,
                                      r = rho, 
                                      smooth_vi = TRUE)
    
    # fit random effects working model in metafor
    mod <- rma.mv(es ~ 1,
                  V = V_mat, 
                  random = ~ 1 | study_id,
                  data = dat,
                  sparse = TRUE)
    
    
    # run RVE
    res <- conf_int(mod, vcov = "CR2", tidy = TRUE) %>%
      as_tibble()
    
  } else {
    
    beta <- mean(dat$es, na.rm = TRUE)
    
    res <- tibble(beta = beta, SE = NA, df = NA, CI_L = NA, CI_U = NA)
    
  }
  
  output <- bind_cols(res, summary) %>%
    mutate_if(is.numeric, round, 3) #%>%
   # mutate(text_res = paste0(beta, " 95% CI[", CI_L, ", ", CI_U, "]"))
  
  return(output)
}
