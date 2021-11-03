# Create EGM plot ---------------------------------------------------------


p <- ggplot(dat, aes(x = factor_1, y = factor_2, 
                      size = n_studies, shape = factor_3, color = estimate)) +
  geom_point(alpha = 0.6, aes(group = factor_3), 
             position = position_dodge(width= 0.5))  +
  scale_color_viridis_c() +
  scale_shape_manual(values = c(16, 17, 15, 18, 3, 4, 7, 8)) +
  labs(x = "{user_x}", y = "{user_y}",
       color = "Average Effect Size", shape = "{user_shape}") +
  scale_size_identity() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_minimal() +
  theme(legend.position = "bottom")