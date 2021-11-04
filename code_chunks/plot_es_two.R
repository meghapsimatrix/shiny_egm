# Create EGM plot ---------------------------------------------------------

p <- ggplot(summary_dat, aes(x = factor_1, y = factor_2, size = n_studies, color = estimate)) +
  geom_point(alpha = 0.6) + 
  scale_color_viridis_c() +
  labs(x = "{user_x}", y = "{user_y}", color = "Average Effect Size") +
  scale_size_identity() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_minimal() + 
  theme(legend.position = "none")