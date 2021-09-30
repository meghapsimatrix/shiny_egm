dat <- dat[, c("{user_params}")]

source('tidy_meta.R')

summary_dat <- 
  dat %>%
  group_by(factor_1, factor_2) %>%
  group_modify(~ tidy_meta(.x, 
                           model = "{user_mod}", 
                           rho_val = {user_rho})) %>%
  ungroup()