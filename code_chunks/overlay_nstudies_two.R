p <- p + 
  geom_text(aes(label = as.character(n_studies)), 
            size = 2.5, color = "black") +
  labs(caption = "Number of studies per combination of factors are overlaid.")