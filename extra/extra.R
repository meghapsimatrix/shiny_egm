output$meta <- renderText({
  
  dat <- datClean()
  
  dat %>%
    group_by(factor_1, factor_2) %>%
    group_modify(~ tidy_meta(.x)) %>%
    kable(digits = 3) %>%
    kable_styling(
      font_size = 15,
      bootstrap_options = c("striped", "hover", "condensed")
    )
  
  
  
})


tabPanel("Meta Analysis Results",
         htmlOutput("meta")),