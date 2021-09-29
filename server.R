
server <- 
  
  shinyServer(function(input, output, session) {
    
    updateRadioButtons(
      session,
      inputId = 'ex_upload',
      label = 'Do you want to use an example or upload your own data?',
      choices = c("Use an example" = "example",
                  "Upload my own data" = "up"),
      selected = "example")
    
    
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
      label = 'What data do you want to upload?',
      choices = c("Upload a .csv or .txt file" = "dat",
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
    
    updateRadioButtons(
      session, 
      inputId = 'model', 
      label = 'Which model do you want to use to calculate the average effect sizes?',
      choices = c("Correlated and Hierarchical Effects Model" = "che",
                  "Correlated Effects Model" = "ce",
                  "Hiearchical Effects Model" = "he"),
      selected = "che"
    )
    
    updateSliderInput(
      session,
      inputId = 'rho',
      label = "What value would you like to use for the within-study correlation between effect sizes?",
      min = 0, max = 1, value = 0.8
    )
    
    updateSliderInput(
      session,
      inputId = 'height',
      label = "Download Plot: Please specify the height (in).",
      min = 1, max = 15, value = 7
    )
    
    updateSliderInput(
      session,
      inputId = 'width',
      label = "Download Plot: Please specify the width (in).",
      min = 1, max = 15, value = 7
    )
    
    updateTextInput(
      session,
      inputId = "pname",
      label = "Download Plot: Download Plot: Please specify the name of the plot.",
      value = "egm_plot.png"
    )

    
    updateSelectInput(
      session,
      inputId = "overlay",
      label = "What do you want to overlay on the dots?",
      choices = c("Number of Studies" = "nstudy", 
                  "Average Effect Size" = "aves", 
                  "Nothing" = "nothing"), 
      selected = "nothing"
    )
  
    
    # get an error saying argument of length 0
    # sheetname <- reactive({
    #   if (input$dat_type == "xlsx") {
    #     inFile <- input$xlsx
    #     if (is.null(inFile)) return(NULL)
    #     sheetnames <- excel_sheets(inFile$datapath)
    #   } 
    # })
    # 
    # observe({
    #   sheets <- sheetname()
    #   updateSelectInput(session, "inSelect", label = "Select a sheet",
    #                     choices = sheets,
    #                     selected = sheets[1])
    # })
  
    
    
    
    # Read in data
    
    datFile <- reactive({ 
      
      if (input$ex_upload == "example") {
        
        read_csv("example/example_dat_clean.csv")
        
        # if(input$num_factors == "two"){
        #   
        #   read_csv("example/dat_sum_2.csv") 
        # 
        #   
        # } else if(input$num_factors == "three"){
        #   
        #   read_csv("example/dat_sum_3.csv") 
        #   
        # }
      }
        
        
      else if(input$ex_upload == "up"){
      
          if (input$summary_raw == "esdat" & input$dat_type == "dat" || input$summary_raw == "sumdat" & input$dat_type == "dat") {
            
            inFile <- input$dat
            
            if (is.null(inFile)) return(NULL)
            
            read.table(inFile$datapath, header = input$header, 
                       sep = input$sep, #quote=input$quote,
                       stringsAsFactors = FALSE) %>% 
              clean_names(case = "parsed")
            
          } else if (input$summary_raw == "esdat" & input$dat_type == "xlsx" || input$summary_raw == "sumdat" & input$dat_type == "xlsx") {
            
            inFile <- input$xlsx
            
            if (is.null(inFile)) return(NULL)
            
            readxl::read_xlsx(inFile$datapath, col_names = input$col_names,
                              sheet = 1) %>% 
              clean_names(case = "parsed") %>%
              as.data.frame()
            
          } 
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
      selectInput("variance", label = "Variance or SE: Please specify the variable in the dataset containting the variance or the standard error of the effect sizes.", choices = var_names, selected = var_names[1])
    })
    
    output$studyMapping <- renderUI({
      var_names <- names(datFile())
      selectInput("studyid", label = "Study ID: Please specify the variable with the study identifier.", choices = var_names, selected = var_names[1])
    })
    
    # output$esidMapping <- renderUI({
    #   var_names <- names(datFile())
    #   selectInput("esid", label = "Effect Size ID: Please specify the variable with the effect size identifier.", choices = var_names, selected = var_names[1])
    # })
    # 
    
    
    
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
    

    # Parameters --------------------------------------------------------------
    
    modType <- eventReactive(input$go, {
      
      input$model
      
    })
    

    rho <- eventReactive(input$go, {
      
      input$rho
      
    })    
    
    ht <- reactive({
      
      input$height
      
    })
    
    wd <- reactive({
      
      input$width
      
    })
    
    plotname <- reactive({
      
      input$pname
    })
    
    
    output$noparam <- renderText({ "Because you want to use summary data, no need to set the parameters." })
    
    # clean the data ----------------------------------------------------------
    
    
    datClean <- eventReactive(input$go, {
      
      if(input$ex_upload == "example"){
        
        dat <- datFile()
        
        if(input$num_factors == "two"){
          
          shinybusy::show_modal_spinner(text = "Estimating...") 
          
            dat <- 
              dat %>%
              group_by(factor_1, factor_2) %>%
              group_modify(~ tidy_meta(.x, 
                                       model = modType(), 
                                       rho_val = rho())) %>%
              ungroup()
          
            shinybusy::remove_modal_spinner() 

        } else if(input$num_factors == "three"){
          
          shinybusy::show_modal_spinner(text = "Estimating...") 
          
          dat <- 
            dat %>%
            group_by(factor_1, factor_2, factor_3) %>%
            group_modify(~ tidy_meta(.x, 
                                     model = modType(), 
                                     rho_val = rho())) %>%
            ungroup()
          
          shinybusy::remove_modal_spinner()
          
        }
        
        
        
        
      } else if(input$ex_upload == "up"){
        
          
          if(input$summary_raw == "esdat"){
            
            es <- datFile()[,input$effectsize]
            var <- datFile()[,input$variance]
            studyid <- datFile()[,input$studyid]
            x <- datFile()[,input$x]
            y <- datFile()[,input$y]
            
            if(input$sevar == "se"){
              
              var <- var^2
              
            }
            
            if(input$z == "None"){
              
              dat <- data.frame(es = es,
                                var = var,
                                study_id = studyid,
                                factor_1 = x, 
                                factor_2 = y)
              
              shinybusy::show_modal_spinner(text = "Estimating...") 
              
              
              dat <- 
                dat %>%
                group_by(factor_1, factor_2) %>%
                group_modify(~ tidy_meta(.x, 
                                         model = modType(), 
                                         rho_val = rho())) %>%
                ungroup()
              
              shinybusy::remove_modal_spinner()
              
            } else {
              
              
              z <- datFile()[,input$z]
              
              dat <- data.frame(es = es,
                                var = var,
                                study_id = studyid,
                                # es_id = esid,
                                factor_1 = x, 
                                factor_2 = y,
                                factor_3 = z)
              
              shinybusy::show_modal_spinner(text = "Estimating...") 
              
              
              dat <- 
                dat %>%
                group_by(factor_1, factor_2, factor_3) %>%
                group_modify(~ tidy_meta(.x, 
                                         model = modType(), 
                                         rho_val = rho())) %>%
                ungroup()
              
              shinybusy::remove_modal_spinner()
              
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
                mutate(estimate = avg_es)
              
              
            }
            
          } 
        
        
      }  
      
      
      
      return(dat)
      
    })
    
    
    # output ------------------------------------------------------------------
    
    
    output$contents <- renderDataTable({
      
      DT::datatable(datClean())
      
    })
    
    
   plot_obj <- reactive({
      
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
      
      
      
      if(input$overlay == "nstudy"){
        
        if(input$ex_upload == "example"){
          
          if(input$num_factors == "two"){
            
          p <- add_text_egm(p, what_over = "n_studies", factors_n = "two")
            
          } else if(input$num_factors == "three"){
            
            p <- add_text_egm(p, what_over = "n_studies", factors_n = "three")
            
            
          }
          
        } else if(input$ex_upload == "up"){
          
          
          if(input$summary_raw == "esdat"){
            
            if(input$z == "None"){
              
              p <- add_text_egm(p, what_over = "n_studies", factors_n = "two")
              
            } else if(input$z != "None"){
              
              p <- add_text_egm(p, what_over = "n_studies", factors_n = "three")
              
            } 
          }
          else if(input$summary_raw == "sumdat"){
            
            if(input$zsum == "None"){
              
              p <- add_text_egm(p, what_over = "n_studies", factors_n = "two")
              
            } else if(input$zsum != "None"){
              
              p <- add_text_egm(p, what_over = "n_studies", factors_n = "three")
              
            } 
          }
          
        }
      }
      
      
      
      else if(input$overlay == "aves"){
        
        
        if(input$ex_upload == "example"){
          
          if(input$num_factors == "two"){
            
            p <- add_text_egm(p, what_over = "aves", factors_n = "two")
            
            
          } else if(input$num_factors == "three"){
            
            p <- add_text_egm(p, what_over = "aves", factors_n = "three")
            
            
          }
          
        } else if(input$ex_upload == "up"){
          
          
          if(input$summary_raw == "esdat"){
            
            if(input$z == "None"){
              
              p <- add_text_egm(p, what_over = "aves", factors_n = "two")
              
            } else if(input$z != "None"){
              
              p <- add_text_egm(p, what_over = "aves", factors_n = "three")
              
            } 
          }
          
          
          else if(input$summary_raw == "sumdat"){
            
            if(input$aves != "None"){
              
              if(input$zsum == "None"){
                
                p <- add_text_egm(p, what_over = "aves", factors_n = "two")
                
              } else if(input$zsum != "None"){
                
                p <- add_text_egm(p, what_over = "aves", factors_n = "three")
                
              }
            } else if(input$aves == "None"){
              
              p <- p
              
            }
            
          }
          
          
        }
      }
      
      p
    
      
    })
   
   output$egmPlot <- renderPlot({
     plot_obj()
   })
    
    
    output$dndPlot <- downloadHandler(
      filename = function() {
        plotname()
      },
      content = function(file) {
        ggsave(file, plot_obj(), device = "png", units = "in", height = ht(), width = wd())
      }
    )
    
    
    # This part doesn't work yet!
    
    egm_syntax <- reactive({
      
      header_res <- c(
        '# Load packages',
        'library(tidyverse)',
        'library(metafor)',
        ''
      )
      
      # read in file code
      
      if(input$ex_upload == "example") {
        
        read_res <- c(
          parse_code_chunk("load_example", args = list(path = "example/example_dat_clean.csv")),
          ''
        )

      } else if(input$ex_upload == "up"){
        
        if(input$dat_type == "dat"){
          
        inFile <- input$dat
        
        read_res <- c(
          parse_code_chunk("load_dat", 
                           args = list(user_path = inFile$name, user_header = input$header, 
                                       user_sep = input$sep)),
          ''
        )
      } else if (input$dat_type == "xlsx") {
        
        inFile <- input$xlsx
        
        read_res <- c(
          parse_code_chunk("load_excel", args = list(user_path = inFile$name)),
          ''
        )
      }
        
      }
      
      
      # 
      # if(input$ex_upload == "example"){
      #   
      #   
      #   if(input$num_factors == "two"){
      #     
      #     clean_dat <- c(
      #       parse_code_chunk("dat_example",
      #                             args = list(user_params = c("factor_1", "factor_2", "es", "var", "studyid"))),
      #       ''
      #     )
      #     
      #   } else if(input$num_factors == "three"){
      #     
      #     clean_dat <- c(
      #       parse_code_chunk("dat_example",
      #                        args = list(user_params = c("factor_1", "factor_2", "factor_3", "es", "var", "studyid"))),
      #       ''
      #     )
      #     
      #   }
      #   
      # }
      
    })
    
    
    output$syntax <- renderPrint({
      cat(egm_syntax(), sep = "\n")
    })
    
    output$clip <- renderUI({
      rclipButton("clipbtn", "Copy", egm_syntax(), icon("clipboard"))
    })
    
  })
    
  
