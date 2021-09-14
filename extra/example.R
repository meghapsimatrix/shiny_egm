load("dat_egm_prg.RData")

exampleChoices <-  c("Data EGM Program" = "dat_egm_prg")


exampleMapping <- list(
  dat_egm_org = list(x = "program",
                     y = "outcome")
)


# output$downloadPlot <- downloadHandler(
#   
#   filename = function(){
#      "plot.png"
#     },
#   
#   
#   content = function(file){
#     file.copy("plot.png", file, overwrite = TRUE)
#   }
# )
# 

# output$info <- renderTable({
#   
#   dat <- datClean()
#   
#   if (is.null(input$plot1_click$x)) return("")
#   else{
#     
#   fct_1 <- levels(as.factor(dat$factor_1))
#   fct_2 <- levels(as.factor(dat$factor_2))
#   fct_click_1 <- fct_1[round(input$plot_click$x)]
#   fct_click_2 <- fct_2[round(input$plot_click$y)]
#   
#   
# 
#   dat %>%
#     filter(factor_1 == fct_click_1, 
#            factor_2 == fct_click_2) %>%
#     kable(digits = 3) %>%
#     kable_styling(
#       font_size = 15,
#       bootstrap_options = c("striped", "hover", "condensed")
#     )
#   }
# 
# })