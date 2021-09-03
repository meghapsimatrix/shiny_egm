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
        inputId = 'dat_type', 
        label = 'What data do you want to use?',
        choices = c("Use an example" = "example",
                    "Upload data from a .csv or .txt file" = "dat",
                    "Upload data from a .xlsx file" = "xlsx",
                    "Use the dataset specified at initialization" = "loaded"),
        selected = "loaded"
      )
    }
    
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
      
      if (input$dat_type == "dat") {
        
        inFile <- input$dat
        
        if (is.null(inFile)) return(NULL)
        
        read.table(inFile$datapath, header=input$header, 
                   sep=input$sep, quote=input$quote,
                   stringsAsFactors = FALSE) %>% 
          clean_names(case = "parsed")
        
      } else if (input$dat_type == "xlsx") {
        
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
    
    
    output$esMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("effectsize", label = "Effect Size", choices = var_names, selected = var_names[n_var])
    })
    
    output$varMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("variance", label = "Variance", choices = var_names, selected = var_names[n_var])
    })
    
    output$studyMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("study", label = "StudyID", choices = var_names, selected = var_names[n_var])
    })
    
    output$xMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("x", label = "Factor 1", choices = var_names, selected = var_names[n_var])
    })
    
    output$yMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("y", label = "Factor 2", choices = var_names, selected = var_names[n_var])
    })
    
    
    datClean <- reactive({
      
      es <- datFile()[,input$effectsize]
      var <- datFile()[,input$variance]
      study <- datFile()[,input$study]
      x <- datFile()[,input$x]
      y <- datFile()[,input$y]
      
      dat <- data.frame(es = es,
                        var = var,
                        study = study,
                        factor_1 = x, 
                        factor_2 = y)
      
      return(dat)
      
    })
    
    output$contents <- renderDataTable({
      
      DT::datatable(datClean())
      
    })
    
    
    output$egmPlot <- renderPlotly({
      
      dat <- datClean()
      
      dat <- dat %>%
        group_by(factor_1, factor_2) %>%
        summarize(n_studies = n_distinct(study), .groups = "drop")
      
      
      p <- ggplot(dat, aes(x = factor_1, y = factor_2, size = n_studies)) + 
        geom_point(color = "dark blue") + 
        labs(x = "", y = "") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        theme_minimal()
      
      ggplotly(p)
      
    })
    
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
    
    output$syntax <- renderPrint({
     
      "syntax"

      
      
    })

  })    