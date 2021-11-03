# Create Summary Data -----------------------------------------------------


# this loads a function that calculates number of studies and estimates average effects
source('https://raw.githubusercontent.com/meghapsimatrix/shiny_egm/main/tidy_meta.R')

# group by factors and then run tidy_meta function through each combination 
summary_dat <- 
  dat %>%
  group_by(factor_1, factor_2, factor_3) %>%
  group_modify(~ tidy_meta(.x, 
                           rho_val = {user_rho})) %>%
  ungroup()


