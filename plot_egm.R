make_egm_plot <- function(data, factors_n){

  if(factors_n == "two"){
    
  p <- ggplot(data, aes(x = factor_1, y = factor_2, size = n_studies)) + 
    geom_point(alpha = 0.5, color = "skyblue") + 
    labs(x = "", y = "") +
    scale_size_identity() +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
    theme_minimal() + 
    theme(legend.position = "none")
  }
  
  else if(factors_n == "three"){
    
    p <- ggplot(data, aes(x = factor_1, y = factor_2, 
                         size = n_studies, color = factor_3)) + 
      geom_point(alpha = 0.5, aes(group = factor_3), 
                 position = position_dodge(width= 0.5)) + 
      labs(x = "", y = "", color = "") +
      scale_size_identity() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
    
  return(p)
    
}


add_text_egm <- function(pl, what_over = "n_studies", factors_n){
  
  if(what_over == "n_studies" & factors_n == "two"){
  
      pl <- 
        pl + 
        geom_text(aes(label = as.character(n_studies)), 
                size = 2.5, color = "black") +
        labs(caption = "Number of studies per combination of factors are overlaid.")
      
  } else if(what_over == "n_studies" & factors_n == "three"){
    
    pl <- 
      pl + 
      geom_text(aes(label = as.character(n_studies), group = factor_3), 
                size = 2.5, color = "black",
                position = position_dodge(width = .5)) +
      labs(caption = "Number of studies per combination of factors are overlaid.")
    
    
  } else if(what_over == "aves" & factors_n == "two"){
    
    
    pl <- pl  + 
      geom_text(aes(label = as.character(beta)), 
                size = 2.5, color = "black") +
      labs(caption = "Average effect sizes per combination of factors are overlaid.")
    
    
  } else if(what_over == "aves" & factors_n == "three"){
    
    pl <- pl + 
      geom_text(aes(label = as.character(beta), group = factor_3), 
                size = 2.5, color = "black",
                position = position_dodge(width = .5)) +
      labs(caption = "Average effect sizes per combination of factors are overlaid.")
    
    
  }
  
  return(pl)
  
  
}
