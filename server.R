library(shiny)
library(markdown)
library(ggplot2)
library(readxl)
library(janitor)
library(readr)
library(knitr)
library(kableExtra)
library(plotly)

source("tidy_meta.R")
options(knitr.kable.NA = '')


server <- 
  
  shinyServer(function(input, output, session) {
    
    if (exists("dataset", where = environment(server))) if (!is.null(dataset)) {
      
      updateRadioButtons(
        session, 
        inputId = 'summary_raw', 
        label = 'Do you want to use summary level data or effect size level data?',
        choices = c("Use effect size level data" = "esdat",
                    "Use summary level data" = "sumdat"),
        selected = "esdat"
      )
      
      
      updateRadioButtons(
        session, 
        inputId = 'dat_type', 
        label = 'What data do you want to use?',
        choices = c("Use an example" = "example",
                    "Upload data from a .csv or .txt file" = "dat",
                    "Upload data from a .xlsx file" = "xlsx",
                    "Use the dataset specified at initialization" = "loaded"),
        selected = "loaded"
      )
    }
    
    updateSelectInput(
      session,
      inputId = "overlay",
      label = "What do you want to overlay on the dots?",
      choices = c("Number of Studies" = "nstudy", 
                  "Average Effect Size" = "aves", 
                  "Nothing" = "nothing"), 
      selected = "nothing"
    )
    
    sheetname <- reactive({
      if (input$dat_type == "xlsx") {
        inFile <- input$xlsx
        if (is.null(inFile)) return(NULL)
        sheetnames <- excel_sheets(inFile$datapath)
      } 
    })
    
    observe({
      sheets <- sheetname()
      updateSelectInput(session, "inSelect", label = "Select a sheet",
                        choices = sheets,
                        selected = sheets[1])
    })
    
    # Read in data
    
    datFile <- reactive({ 
      
      if (input$summary_raw == "esdat" & input$dat_type == "dat" || input$summary_raw == "sumdat" & input$dat_type == "dat") {
        
        inFile <- input$dat
        
        if (is.null(inFile)) return(NULL)
        
        read.table(inFile$datapath, header=input$header, 
                   sep=input$sep, quote=input$quote,
                   stringsAsFactors = FALSE) %>% 
          clean_names(case = "parsed")
        
      } else if (input$summary_raw == "esdat" & input$dat_type == "xlsx" || input$summary_raw == "sumdat" & input$dat_type == "xlsx") {
        
        inFile <- input$xlsx
        
        if (is.null(inFile) || is.null(input$inSelect) || nchar(input$inSelect) == 0) return(NULL)
        
        readxl::read_xlsx(inFile$datapath, col_names = input$col_names,
                          sheet = input$inSelect) %>% 
          clean_names(case = "parsed") %>%
          as.data.frame()
        
      } else if (input$dat_type == "loaded") {
        
        dataset %>% 
          clean_names(case = "parsed")
        
      } 
    })
    
    # Check that file is uploaded
    
    output$fileUploaded <- reactive({
      
      return(!is.null(datFile()))
    
      })
    
    

    # es level data mapping ---------------------------------------------------

    
    output$esMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("effectsize", label = "Effect Size: Please specify the variable in the dataset containing the effect sizes.", choices = var_names, selected = var_names[n_var])
    })
    
    output$varMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("variance", label = "Variance: Please specify the variable in the dataset containting the variance of the effect sizes.", choices = var_names, selected = var_names[n_var])
    })
    
    output$studyMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("studyid", label = "Study ID: Please specify the variable with the study identifier.", choices = var_names, selected = var_names[n_var])
    })
    
    output$esidMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("esid", label = "Effect Size ID: Please specify the variable with the effect size identifier.", choices = var_names, selected = var_names[n_var])
    })
    
    output$xMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("x", label = "Factor 1: Please specify the first factor for the EGM.", choices = var_names, selected = var_names[n_var])
    })
    
    output$yMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("y", label = "Factor 2: Please specify the second factor for the EGM.", choices = var_names, selected = var_names[n_var])
    })
    

    # summary data mapping ----------------------------------------------------
    
    
    output$nstudyMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("nstudy", label = "Number of Studies: Please specify the variable containing the number of effect sizes per combination of factors.", choices = var_names, selected = var_names[n_var])
    })
    
    output$xsumMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("xsum", label = "Factor 1: Please specify the first factor for the EGM.", choices = var_names, selected = var_names[n_var])
    })
    
    output$ysumMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("ysum", label = "Factor 2: Please specify the second factor for the EGM.", choices = var_names, selected = var_names[n_var])
    })
    
    
    output$avesMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("aves", label = "Average effect size: Please specify variable containing the average effect size per combination of factors.", choices = var_names, selected = var_names[n_var])
    })
    
    
    

    # clean the data ----------------------------------------------------------
    
    
    datClean <- reactive({
      
      if(input$summary_raw == "esdat"){
      
      es <- datFile()[,input$effectsize]
      var <- datFile()[,input$variance]
      studyid <- datFile()[,input$studyid]
      esid <- datFile()[,input$esid]
      x <- datFile()[,input$x]
      y <- datFile()[,input$y]
      
      dat <- data.frame(es = es,
                        var = var,
                        study_id = studyid,
                        es_id = esid,
                        factor_1 = x, 
                        factor_2 = y)
      
      dat <- 
        dat %>%
        group_by(factor_1, factor_2) %>%
        group_modify(~ tidy_meta(.x)) %>%
        ungroup()
      
      
      
      } else if(input$summary_raw == "sumdat"){
        
        x <- datFile()[,input$xsum]
        y <- datFile()[,input$ysum]
        n_studies <- datFile()[,input$nstudy]
        avg_es <- datFile()[,input$aves]
        
        dat <- data.frame(factor_1 = x, 
                          factor_2 = y,
                          n_studies = n_studies,
                          beta = avg_es)
        
      }
      
      return(dat)
      
    })
    
    

   # output ------------------------------------------------------------------
    
    
    output$contents <- renderDataTable({
      
      DT::datatable(datClean())
      
    })
    
    
    output$egmPlot <- renderPlotly({
      
      dat <- datClean()
      
      
      p <- ggplot(dat, aes(x = factor_1, y = factor_2, size = n_studies)) + 
        geom_point(color = "skyblue") + 
        labs(x = "", y = "") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        theme_minimal()
      
      if(input$overlay == "nstudy"){
        
        p <- p + geom_text(aes(label = as.character(n_studies), size =5))
        
      } else if(input$overlay == "aves"){
        
        p <- p + geom_text(aes(label = as.character(beta), size =5))
        
      }
      
      ggplotly(p, height = 800, width = 900)
      
    })
    

    

    
    
    output$syntax <- renderPrint({
      "R syntax"
    })
    
    output$clip <- renderUI({
      rclipButton("clipbtn", "Copy", "R syntax", icon("clipboard"))
    })
    

  })    