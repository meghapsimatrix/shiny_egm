tabPanel("Evidence Gap Map",
         
         br(),
         sidebarPanel("",
                      selectInput("overlay", label = "What do you want to overlay on the dots?",
                                  choices = c("Number of Studies" = "nstudy", 
                                              "Average Effect Size" = "aves", 
                                              "Nothing" = "nothing"), 
                                  selected = "nothing")
         ), 
         mainPanel("",
                   #downloadButton('downloadPlot', 'Download Plot'),
                   plotOutput("egmPlot")
                   
                   # htmlOutput("info")
                   
         )
)




output$egmPlot <- renderPlot({
  
  dat <- datClean()
  
  if(input$ex_upload == "example"){
    
    if(input$num_factors == "two"){
      
      p <- make_egm_plot(data = dat, factors_n = "two")
      
    } else if(input$num_factors == "three"){
      
      p <-  make_egm_plot(data = dat, factors_n = "three")
    }
    
  } else if(input$ex_upload == "up"){
    
    if(input$summary_raw == "esdat"){
      
      if(input$z == "None"){
        
        p <-  make_egm_plot(data = dat, factors_n = "two")
        
      } else{
        
        p <-  make_egm_plot(data = dat, factors_n = "three")
      }
      
    } else if(input$summary_raw == "sumdat"){
      
      if(input$zsum == "None"){
        
        p <-  make_egm_plot(data = dat, factors_n = "two")
        
      } else{
        
        p <-  make_egm_plot(data = dat, factors_n = "three")
      }
    }
    
  }
  
  p
  
})


