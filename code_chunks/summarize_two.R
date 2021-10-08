# Create Summary Data -----------------------------------------------------


# this loads a function that calculates number of studies and estimates average effects
source('https://raw.githubusercontent.com/meghapsimatrix/shiny_egm/main/tidy_meta.R')

# group by factors and then run tidy_meta function through each combination 
summary_dat <- 
  dat %>%
  group_by(factor_1, factor_2) %>%
  group_modify(~ tidy_meta(.x, 
                           model = "{user_mod}", 
                           rho_val = {user_rho})) %>%
  ungroup()



# Create EGM plot ---------------------------------------------------------

p <- ggplot(summary_dat, aes(x = factor_1, y = factor_2, size = n_studies)) + 
  geom_point(alpha = 0.6, color = "skyblue") + 
  labs(x = "{user_x}", y = "{user_y}") +
  scale_size_identity() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_minimal() + 
  theme(legend.position = "none")