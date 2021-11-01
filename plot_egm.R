palette_OkabeIto <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

make_egm_plot <- function(data, 
                          factors_n, 
                          es, 
                          xlabel, 
                          ylabel, 
                          colorlabel = "", 
                          shapelabel = ""){

  if(factors_n == "two"){
    
    if(es == TRUE){
      
      p <- ggplot(data, aes(x = factor_1, y = factor_2, size = n_studies, color = estimate)) +
        geom_point(alpha = 0.6) + 
        scale_color_viridis_c() +
        labs(color = "Average Effect Size")
      
    } else if (es == FALSE){
      
      p <- ggplot(data, aes(x = factor_1, y = factor_2, size = n_studies)) +
        geom_point(alpha = 0.6, color = "skyblue") 
      
    }
  
  }
  
  else if(factors_n == "three"){
    
    if(es == TRUE){
      
      p <- ggplot(data, aes(x = factor_1, y = factor_2, 
                            size = n_studies, shape = factor_3, color = estimate)) +
        geom_point(alpha = 0.6, aes(group = factor_3), 
                   position = position_dodge(width= 0.5))  +
        scale_color_viridis_c() +
        scale_shape_manual(values = c(16, 17, 15, 18, 3, 4, 7, 8)) +
        labs(color = "Average Effect Size", shape = shapelabel)
        
      
    } else if(es == FALSE) {
      
      p <- ggplot(data, aes(x = factor_1, y = factor_2, 
                            size = n_studies, color = factor_3)) +
        geom_point(alpha = 0.6, aes(group = factor_3), 
                   position = position_dodge(width= 0.5))  +
        scale_color_manual(values = palette_OkabeIto) +
        labs(color = colorlabel)
    }
    

  }
  
  p <- p + 
    labs(x = xlabel, y = ylabel) +
    scale_size_identity() +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
    theme_minimal() + 
    guides(size = "none") +
    theme(legend.position = "bottom")
    
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
      geom_text(aes(label = as.character(round(estimate, 1))), 
                size = 2.5, color = "black") +
      labs(caption = "Average effect sizes per combination of factors are overlaid.")
    
    
  } else if(what_over == "aves" & factors_n == "three"){
    
    pl <- pl + 
      geom_text(aes(label = as.character(round(estimate, 1)), group = factor_3), 
                size = 2.5, color = "black",
                position = position_dodge(width = .5)) +
      labs(caption = "Average effect sizes per combination of factors are overlaid.")
    
    
  }
  
  return(pl)
  
  
}
