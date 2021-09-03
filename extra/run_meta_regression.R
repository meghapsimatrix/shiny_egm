library(robumeta)
library(clubSandwich)
library(tidyverse)

tidy_meta <- function(dat){
  
  summary <- dat %>% 
    summarize(n_studies = n_distinct(study_id),
              n_es = n())
  
  m <- summary %>% pull(n_studies)
  
  if(m > 1){
  
  mod <- robu(formula = es ~ 1,
            data = dat,
            var.eff.size = var,
            studynum = study_id)
  
  
  res <- conf_int(mod, vcov = "CR2", tidy = TRUE) %>%
    as_tibble()
  } else {
    
    res <- tibble(beta = NA, SE = NA, df = NA, CI_L = NA, CI_U = NA)
    
  }

  output <- bind_cols(res, summary)
  
  return(output)
}

load("dat_egm_prg.RData")


dat_egm_prg <- 
  dat_egm_prg %>%
  rename(es = g_yi_f, study_id = StudyID, var = g_vi)



summary_dat <- 
  dat_egm_prg %>%
  group_by(program, outcome) %>%
  summarize(n = n_distinct(study_id), .groups = "drop") 


dat_egm_prg <- 
  dat_egm_prg %>%
  left_join(summary_dat, by = c("program", "outcome")) %>%
  filter(n > 2)


dat_egm_prg %>%
  group_by(program, outcome) %>%
  do(tidy_meta(.)) %>%
  View()



