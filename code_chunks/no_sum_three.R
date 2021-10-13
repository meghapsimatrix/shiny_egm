# Create EGM plot ---------------------------------------------------------

# from colorblindr package https://github.com/clauswilke/colorblindr
palette_OkabeIto <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

p <- ggplot(dat, aes(x = factor_1, y = factor_2, 
                             size = n_studies, color = factor_3)) + 
  geom_point(alpha = 0.6, aes(group = factor_3), 
             position = position_dodge(width= 0.5)) + 
  labs(x = "{user_x}", y = "{user_y}", color = "{user_color}") +
  scale_size_identity() +
  scale_color_manual(values = palette_OkabeIto) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_minimal() +
  theme(legend.position = "bottom")