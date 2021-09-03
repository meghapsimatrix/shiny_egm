# syntax for rep ----------------------------------------------------------


metaSyntax <- reactive({
  
  header_res <- c(
    '# Load packages',
    'library(tidyverse)',
    'library(robumeta)',
    'library(clubSandwich)',
    'library(plotly)'
  )
  
  # read in file code
  
  if(input$dat_type == "dat"){
    inFile <- input$dat
    read_res <- c(
      parse_code_chunk("load-dat", 
                       args = list(user_path = inFile$name, user_header = input$header, 
                                   user_sep = input$sep, user_quote = input$quote)),
      ''
    )
  } else if (input$dat_type == "xlsx") {
    inFile <- input$xlsx
    read_res <- c(
      parse_code_chunk("load-excel", args = list(user_path = inFile$name, user_sheet = input$inSelect)),
      ''
    )
  }
  
  
  
})
