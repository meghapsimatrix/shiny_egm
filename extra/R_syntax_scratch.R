if(input$ex_upload == "example"){
  
  
  if(input$num_factors == "two"){
    
    dat <- parse_code_chunk("dat_example",
                            args = list(user_params = c("factor_1", "factor_2", "es", "var", "studyid")))
    
  } else if(input$num_factors == "three"){
    
    dat <- parse_code_chunk("dat_example",
                            args = list(user_params = c("factor_1", "factor_2", "factor_3", "es", "var", "studyid")))
    
  }
  
}


if(input$ex_upload == "up"){
  
  
  
  #user_params <- 
  
  
}