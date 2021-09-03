datClean <- reactive({
    
    y <- datFile()[,input$outcome]
    var <- datFile()[,input$variance]
    study <- as.numeric(datFile()[,input$study])
    x <- as.numeric(datFile()[,input$x])
    y <- as.numeric(datFile()[,input$y])
    
    dat <- data.frame(y = y,
                      var = var,
                      study = study,
                      x = x, 
                      y = y)
    
})