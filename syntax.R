library(robumeta)
library(clubSandwich)
library(tidyverse)
library(plotly)


# Data set up -------------------------------------------------------------

# on the right hand side of the = input the corresponding names
# of the variables in your dataset


dat <- input_data %>% # name of your data 
  rename(es = `Effect Size`, # name of the effect size variable
         var = `Variance of Effect Size`, # name of the variance of the es
         study = `Study Identifier`, # name of the study id variable
         factor_1 = `First Factor`, # name of the first factor variable
         factor_2 = `Second Factor`) # name of the second factor variable



# Function to run meta-regression -----------------------------------------


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



# Evidence Map Gap --------------------------------------------------------

dat_summary <- dat %>%
  group_by(factor_1, factor_2) %>%
  summarize(n_studies = n_distinct(study), .groups = "drop")


p <- ggplot(dat_summary, aes(x = factor_1, y = factor_2, size = n_studies)) + 
  geom_point(color = "dark blue") + 
  labs(x = "", y = "") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_minimal()


ggplotly(p)


# Run meta-analysis -------------------------------------------------------

dat %>%
  group_by(factor_1, factor_2) %>%
  group_modify(~ tidy_meta(.x)) %>%
  kable(digits = 3) %>%
  kable_styling(
    font_size = 15,
    bootstrap_options = c("striped", "hover", "condensed")
  )