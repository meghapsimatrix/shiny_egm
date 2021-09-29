library(tidyverse)


dat <- read_csv("example/example_dat_clean.csv")


glimpse(dat)
count(study_id)


dat_sum <- dat %>%
  group_by(factor_1, factor_2) %>%
  summarize(n_studies = n_distinct(study_id))


dat_wt_count <- dat %>%
  group_by(factor_1, factor_2, study_id) %>%
  summarize(var_m = mean(var), .groups = "drop") %>%
  count(factor_1, factor_2, wt = var_m)


dat_wt_count %>%
  ggplot(aes(x = factor_1, y = factor_2, size = n)) +
  geom_point(alpha = .5, color = "skyblue") + 
  theme_minimal()


dat_sum %>%
  ggplot(aes(x = factor_1, y = factor_2, size = n_studies)) +
  geom_point(alpha = .5, color = "skyblue") + 
  theme_minimal()
