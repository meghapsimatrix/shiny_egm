dat <- dat[, c("{user_params}")]
names(dat) <- c("factor_1", "factor_2", "factor_3", "es", "var", "studyid")

source('tidy_meta.R')

summary_dat <- 
  dat %>%
  group_by(factor_1, factor_2, factor_3) %>%
  group_modify(~ tidy_meta(.x, 
                           model = "{user_mod}", 
                           rho_val = {user_rho})) %>%
  ungroup()