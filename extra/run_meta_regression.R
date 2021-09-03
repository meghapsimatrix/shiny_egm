library(robumeta)
library(clubSandwich)
library(tidyverse)

tidy_meta <- function(dat){
  
  
  mod <- robu(formula = y ~ 1,
            data = dat,
            var.eff.size = var,
            studynum = study_id)
  
  
  res <- conf_int(mod, vcov = "CR2", tidy = TRUE) %>%
    as_tibble()

  summary <- dat %>% 
    summarize(n_studies = n_distinct(study_id),
              n_es = n_distinct(es_id))
  
  output <- bind_cols(res, summary)
  
  return(output)
}






