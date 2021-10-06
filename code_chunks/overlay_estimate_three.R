p <- p + 
  geom_text(aes(label = as.character(round(estimate, 1)), group = factor_3), 
            size = 2.5, color = "black",
            position = position_dodge(width = .5)) +
  labs(caption = "Average effect sizes per combination of factors are overlaid.")