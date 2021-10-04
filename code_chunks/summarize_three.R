source('tidy_meta.R')

summary_dat <- 
  dat %>%
  group_by(factor_1, factor_2, factor_3) %>%
  group_modify(~ tidy_meta(.x, 
                           model = "{user_mod}", 
                           rho_val = {user_rho})) %>%
  ungroup()


ggplot(summary_dat, aes(x = factor_1, y = factor_2, 
                        size = n_studies, color = factor_3)) + 
  geom_point(alpha = 0.6, aes(group = factor_3), 
             position = position_dodge(width= 0.5)) + 
  labs(x = "", y = "", color = "") +
  scale_size_identity() +
  scale_color_manual(values = palette_OkabeIto) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_minimal() +
  theme(legend.position = "bottom")