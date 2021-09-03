library(robumeta)
library(clubSandwich)
library(tidyverse)

tidy_meta <- function(dat){
  
  summary <- dat %>% 
    summarize(n_studies = n_distinct(study),
              n_es = n())
  
  m <- summary %>% pull(n_studies)
  
  if(m > 1){
    
    mod <- robu(formula = es ~ 1,
                data = dat,
                var.eff.size = var,
                studynum = study)
    
    
    res <- conf_int(mod, vcov = "CR2", tidy = TRUE) %>%
      as_tibble()
  } else {
    
    res <- tibble(beta = NA, SE = NA, df = NA, CI_L = NA, CI_U = NA)
    
  }
  
  output <- bind_cols(res, summary)
  
  return(output)
}