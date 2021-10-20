tidy_meta <- function(dat, 
                      rho_val){

  
  summary <- dat %>% 
    summarize(n_studies = n_distinct(study_id),
              n_es = n())
  
  m <- summary %>% pull(n_studies)
  n <- summary %>% pull(n_es)
  
  if(m > 2){
    

      mod <- robu(formula = es ~ 1,
                  data = dat,
                  var.eff.size = var,
                  modelweights = "CORR",
                  rho = rho_val,
                  studynum = study_id)
      
      
      res <- conf_int(mod, vcov = "CR2", tidy = TRUE) %>%
        rename(estimate = beta) %>%
        as_tibble() %>%
        mutate(method = "Correlated Effects") 
      
   
    
  } else if(n == 1){
    
    res <- tibble(estimate = dat$es,
                  SE = sqrt(dat$var), 
                  df = NA, 
                  CI_L = NA, 
                  CI_U = NA, 
                  method = "Raw Effect Size")
    
  } else{
    
    suppressWarnings(mod <- rma.uni(yi = es,
                                    vi = var,
                                    data = dat)) # REML univariate random effects by default
    
    res <- tidy(mod) %>%
      dplyr::select(estimate, SE = std.error) %>%
      mutate(df = NA,
             CI_L = mod$ci.lb,
             CI_U = mod$ci.ub) %>%
      mutate(method = "Univariate Random Effects")
    
  } 
  
  
  output <- bind_cols(res, summary) %>%
    mutate_if(is.numeric, round, 3) %>%
    select(method, estimate, n_studies, n_es)
  
  return(output)
}
