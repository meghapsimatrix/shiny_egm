dat <- dat_egm_prg %>%
  rename(es = g_yi_f,
         var = g_vi,
         es_id = ES_ID,
         study_id = StudyID,
         factor_1 = program,
         factor_2 = outcome)


tidy_meta(dat, rho = 0.6)


sum_dat <- 
  dat %>%
  group_by(factor_1, factor_2) %>%
  group_modify(~ tidy_meta(.x))


write_csv(sum_dat, "sum_dat_comma.csv")


