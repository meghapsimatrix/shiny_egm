p <- p  + 
  geom_text(aes(label = as.character(round(estimate, 1))), 
            size = 2.5, color = "black") +
  labs(caption = "Average effect sizes per combination of factors are overlaid.")