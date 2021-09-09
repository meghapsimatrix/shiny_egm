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
        choices = c("Effect size level data" = "esdat",
                    "Summary level data" = "sumdat"),
        selected = "esdat"
      )
      
      
      updateRadioButtons(
        session, 
        inputId = 'dat_type', 
        label = 'What data do you want to use?',
        choices = c("Use example data" = "example",
                    "Upload a .csv or .txt file" = "dat",
                    "Upload an .xlsx file" = "xlsx"),
        selected = "example"
      )
      
      updateRadioButtons(
        session, 
        inputId = 'sevar', 
        label = 'Do you want to input variance or standard error of the effect sizes?',
        choices = c("Variance" = "var",
                    "Standard Error" = "se"),
        selected = "var"
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
        
      } else if (input$dat_type == "example") {
        
        dataset %>% 
          clean_names(case = "parsed")
        
      } 
    })
    
    # Check that file is uploaded
    
    output$fileUploaded <- reactive({
      
      return(!is.null(datFile()))
    
      })
    
    

    # es level data mapping ---------------------------------------------------
    
    output$xMapping <- renderUI({
      var_names <- names(datFile())
      selectInput("x", label = "Factor 1: Please specify the first factor for the EGM.", choices = var_names, selected = var_names[1])
    })
    
    output$yMapping <- renderUI({
      var_names <- names(datFile())
      selectInput("y", label = "Factor 2: Please specify the second factor for the EGM.", choices = var_names, selected = var_names[1])
    })
    
    output$zMapping <- renderUI({
      var_names <- names(datFile())
      var_names <- c("None", var_names)
      selectInput("z", label = "Factor 3: Please specify the third factor for the EGM. If you don't have a third factor, select 'None'", choices = var_names, selected = var_names[1])
    })

    
    output$esMapping <- renderUI({
      var_names <- names(datFile())
      selectInput("effectsize", label = "Effect Size: Please specify the variable in the dataset containing the effect sizes.", choices = var_names, selected = var_names[1])
    })
    
    output$varMapping <- renderUI({
      var_names <- names(datFile())
      selectInput("variance", label = "Variance: Please specify the variable in the dataset containting the variance of the effect sizes.", choices = var_names, selected = var_names[1])
    })
    
    output$studyMapping <- renderUI({
      var_names <- names(datFile())
      selectInput("studyid", label = "Study ID: Please specify the variable with the study identifier.", choices = var_names, selected = var_names[1])
    })
    
    output$esidMapping <- renderUI({
      var_names <- names(datFile())
      selectInput("esid", label = "Effect Size ID: Please specify the variable with the effect size identifier.", choices = var_names, selected = var_names[1])
    })
    

    

    # summary data mapping ----------------------------------------------------
    
    
    output$xsumMapping <- renderUI({
      var_names <- names(datFile())
      selectInput("xsum", label = "Factor 1: Please specify the first factor for the EGM.", choices = var_names, selected = var_names[1])
    })
    
    output$ysumMapping <- renderUI({
      var_names <- names(datFile())
      selectInput("ysum", label = "Factor 2: Please specify the second factor for the EGM.", choices = var_names, selected = var_names[1])
    })
    
    output$zsumMapping <- renderUI({
      var_names <- names(datFile())
      var_names <- c("None", var_names)
      selectInput("zsum", label = "Factor 3: Please specify the third factor for the EGM. If you don't have a third factor, select 'None'.", choices = var_names, selected = var_names[1])
    })
    
    output$nstudyMapping <- renderUI({
      var_names <- names(datFile())
      selectInput("nstudy", label = "Number of Studies: Please specify the variable containing the number of studies per combination of factors.", choices = var_names, selected = var_names[1])
    })
    
    
    output$avesMapping <- renderUI({
      var_names <- names(datFile())
      var_names <- c("None", var_names)
      selectInput("aves", label = "Average effect size: Please specify variable containing the average effect sizes per combination of factors. If your data does not have average effect sizes, select 'None'.", choices = var_names, selected = var_names[1])
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
      
        if(input$sevar == "se"){
          
          var <- var^2
          
        }
        
        if(input$z == "None"){
        
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
        
        } else {
          
        
        z <- datFile()[,input$z]
          
        dat <- data.frame(es = es,
                          var = var,
                          study_id = studyid,
                          es_id = esid,
                          factor_1 = x, 
                          factor_2 = y,
                          factor_3 = z)
          
        dat <- 
          dat %>%
          group_by(factor_1, factor_2, factor_3) %>%
          group_modify(~ tidy_meta(.x)) %>%
          ungroup()
          
          
        }
        
      
      
      
      } 
      
      else if(input$summary_raw == "sumdat"){
        
        x <- datFile()[,input$xsum]
        y <- datFile()[,input$ysum]
        n_studies <- datFile()[,input$nstudy]
        
        
        if(input$zsum == "None"){
          
          dat <- data.frame(factor_1 = x, 
                            factor_2 = y,
                            n_studies = n_studies)
        
        } else{
          
          z <- datFile()[,input$zsum]
          
          dat <- data.frame(factor_1 = x, 
                            factor_2 = y,
                            factor_3 = z,
                            n_studies = n_studies)
            
            
        }
        
        
        
        if(input$aves != "None"){
          
          avg_es <- datFile()[,input$aves]
          
          dat <- dat %>%
            mutate(beta = avg_es)
          
          
        }
      
      }
        

      
      
      return(dat)
      
    })
    
    

    # output ------------------------------------------------------------------
    
    
    output$contents <- renderDataTable({

      DT::datatable(datClean())

    })

    
    output$egmPlot <- renderPlot({
      
      dat <- datClean()
      
      
      p <- ggplot(dat, aes(x = factor_1, y = factor_2, size = n_studies)) + 
        geom_point(color = "skyblue") + 
        labs(x = "", y = "") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        theme_minimal() + 
        theme(legend.position = "none")
      
      
      
      if(input$overlay == "nstudy"){
        
        p <- p + 
          geom_text(aes(label = as.character(n_studies), size = 4)) +
          labs(caption = "Number of studies per combination of factors are overlaid.")
        
      } 
      
      else if(input$overlay == "aves"){
        
        if(input$summary_raw == "esdat"){
        
        p <- p + 
          geom_text(aes(label = as.character(beta), size = 4)) +
          labs(caption = "Average effect size per combination of factors are overlaid.")
        
        } 
        
        else if(input$summary_raw == "sumdat"){
         
         if(input$aves != "None"){
           
           p <- p + geom_text(aes(label = as.character(beta), size = 4)) +
             labs(caption = "Average effect size per combination of factors are overlaid.")
           
           
         } else if(input$aves == "None"){
           
           p <- p
           
         }
      
      }
      
        
    }
      
      p
      
   
      
    })
    
    output$downloadPlot <- downloadHandler(
      
      filename = function(){
         "plot.png"
        },
      
      
      content = function(file){
        file.copy("plot.png", file, overwrite = TRUE)
      }
    )
    
    
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

    

    
    
    output$syntax <- renderPrint({
      "R syntax"
    })
    
    output$clip <- renderUI({
      rclipButton("clipbtn", "Copy", "R syntax", icon("clipboard"))
    })
    

  })    