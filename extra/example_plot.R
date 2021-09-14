library(tidyverse)
library(ggstance)


source("tidy_meta.R")



dat <- dat_egm_prg %>%
  rename(es = g_yi_f,
         var = g_vi,
         study_id = StudyID,
         factor_1 = program,
         factor_2 = outcome, 
         factor_3 = Cn_aid_EGM_typ)


dat_sum <-
  dat %>%
  group_by(factor_1, factor_2, factor_3) %>%
  group_modify(~ tidy_meta(.x)) %>%
  ungroup()


write_csv(dat_sum, "dat_sum_comma_3.csv")

ggplot(dat_sum, aes(x = factor_1, y = factor_2, color = factor_3, size = n_studies)) +
  geom_jitter(alpha = 0.5, position = position_dodge(width = 1)) + 
  scale_size_identity() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_minimal() +
  theme(legend.position = "bottom")


dat <- dropoutPrevention



dat <- dat %>%
  rename(es = g_yi_f,
         var = g_vi,
         es_id = ES_ID,
         study_id = StudyID,
         factor_1 = program,
         factor_2 = outcome, 
         factor_3 = Cn_aid_EGM_typ)


glimpse(dat)
