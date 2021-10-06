p <- p + 
  geom_text(aes(label = as.character(n_studies), group = factor_3), 
            size = 2.5, color = "black",
            position = position_dodge(width = .5)) +
  labs(caption = "Number of studies per combination of factors are overlaid.")