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
        mutate(method = "CE")
      
   
    
  } else if(n == 1){
    
    res <- tibble(estimate = dat$es,
                  SE = sqrt(dat$var), 
                  df = NA, 
                  CI_L = NA, 
                  CI_U = NA, 
                  method = "Raw ES")
    
  } else{
    
    suppressWarnings(mod <- lm_robust(es ~ 1, 
                                      data = dat, 
                                      weights = 1 /var,
                                      se_type = "classical"))
    
    res <- tidy(mod) %>%
      select(estimate, SE = std.error, df = df, CI_L = conf.low, CI_U = conf.high) %>%
      mutate(method = "Simple Weighted Average")
    
  } 
  
  
  output <- bind_cols(res, summary) %>%
    mutate_if(is.numeric, round, 3) 
  
  return(output)
}
