dat <- read_csv("example/example_dat_clean.csv")


summary_dat <- 
  dat %>%
  group_by(factor_1, factor_2, factor_3) %>%
  group_modify(~ tidy_meta(.x, 
                           model = "che", 
                           rho_val = .8)) %>%
  ungroup()


