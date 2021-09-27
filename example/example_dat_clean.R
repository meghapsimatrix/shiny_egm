library(tidyverse)


dat <- read_csv("example/cyber_bullying_clean.csv")


dat <- dat %>%
  rename(assignment = He_asgn_type,
         outcome = Co02_macro.1,
         school_setting = Sa22_scl_type) %>%
  mutate(assignment = case_when(assignment == "non_cls" ~ "Non-Randomized Class",
                              assignment == "non_ind" ~ "Non-Randomized Individual",
                              assignment == "non_scl" ~ "Non-Randomized School",
                              assignment == "ran_cls" ~ "Randomized Class",
                              assignment == "ran_ind" ~ "Randomized Individual",
                              assignment == "ran_scl" ~ "Randomized School"),
         outcome = str_sub(outcome, 5),
         school_setting = str_sub(school_setting, 4))


dat <- dat %>%
  select(es = yi_flp,
         var = vi,
         study_id = StudyID,
         factor_1 = assignment,
         factor_2 = outcome, 
         factor_3 = school_setting)

write_csv(dat, "example/example_dat_clean.csv")



dat_sum_3 <-
  dat %>%
  group_by(factor_1, factor_2, factor_3) %>%
  group_modify(~ tidy_meta(.x)) %>%
  ungroup()

# dat_sum_3 <-
#   dat_sum_3 %>%
#   rename(assignment = factor_1,
#          outcome = factor_2,
#          school_setting = factor_3)

write_csv(dat_sum_3, "example/dat_sum_3.csv")

dat_sum_2 <-
  dat %>%
  group_by(factor_1, factor_2) %>%
  group_modify(~ tidy_meta(.x)) %>%
  ungroup()

# dat_sum_2 <-
#   dat_sum_2 %>%
#   rename(assignment = factor_1,
#          outcome = factor_2)


write_csv(dat_sum_2, "example/dat_sum_2.csv")




ggplot(dat_sum, aes(x = factor_1, y = factor_2, color = factor_3, size = n_studies)) +
  #geom_jitter(alpha = 0.5, position = position_dodge(width = 0.5)) + 
  geom_point(alpha = 0.5, aes(group = factor_3), 
             position = position_dodge(width= 0.5)) + 
  scale_size_identity() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  geom_text(aes(label = as.character(n_studies), group = factor_3), 
            size = 2.5, color = "black",
            position = position_dodge(width = .5)) +
  labs(caption = "Number of studies per combination of factors are overlaid.") +
  theme_minimal() +
  theme(legend.position = "bottom")


dat_sum <-
  dat %>%
  group_by(factor_1, factor_2, factor_3) %>%
  group_modify(~ tidy_meta(.x)) %>%
  ungroup()