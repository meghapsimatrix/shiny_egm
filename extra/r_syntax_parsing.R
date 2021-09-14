egm_syntax <- reactive({
  
  header_res <- c(
    '# Load packages',
    'library(tidyverse)',
    'library(metafor)',
    'library(clubSandwich)'
    ''
  )
  
  # read in file code
  
  } else if(input$dat_type == "dat"){
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
  
  # Clean the data
  
  if (input$dat_type == "example") {
    
    example_parms <- exampleMapping[[input$example]]
    filter_vars <- example_parms$filters
    filter_vals <- if(length(filter_vars) > 0) lapply(paste0("filter_", filter_vars), 
                                                      function(x) paste0('"', input[[x]], '"', collapse = ",")) else NULL
    filter_vals <- paste0("%in% c(", filter_vals, ")")
    filter_string <- paste(example_parms$filters, filter_vals, collapse = " & ")
    
    if (!is.null(example_parms$filters)) {
      clean_dat_A <- c(
        parse_code_chunk("clean-example-filter", 
                         args = list(user_filterString = filter_string))
      )
    } else {
      clean_dat_A <- c()
    }
    
    case <- "case"
    session <- "session"
    phase <- "phase"
    outcome <- "outcome"
    
    if (studyDesign() == "TR") {
      clean_dat <- c(clean_dat_A,
                     '',
                     parse_code_chunk("clean-example-nofilter-TR",
                                      args = list(user_parms = paste(example_parms$vars, collapse='", "'),
                                                  user_design = studyDesign()))
      )
    } else {
      clean_dat <- c(clean_dat_A,
                     '',
                     parse_code_chunk("clean-example-nofilter",
                                      args = list(user_parms = paste(example_parms$vars, collapse='", "'),
                                                  user_design = studyDesign(),
                                                  user_model_center = input$model_center))
      )
    }
    
  } else {
    
    case <- input$caseID
    session <- input$session
    phase <- input$phaseID
    outcome <- input$outcome
    round_session <- if (!is.null(input$round_session)) TRUE else FALSE
    
    filter_vars <- input$filters
    filter_vals <- if(length(filter_vars) > 0) lapply(paste0("filter_", filter_vars), 
                                                      function(x) paste0('"', input[[x]], '"', collapse = ",")) else NULL
    filter_vals <- paste0("%in% c(", filter_vals, ")")
    filter_string <- paste(input$filters, filter_vals, collapse = " & ")
    
    if (!is.null(input$filters)) {
      clean_dat_B <- c(
        '',
        parse_code_chunk("clean-inputdata-filter", args = list(user_filterString = filter_string))
      )
    } else {
      clean_dat_B <- c()
    }
    
    if (studyDesign() == "TR") {
      clean_dat <- c(clean_dat_B,
                     '',
                     parse_code_chunk("clean-inputdata-nofilter-TR", 
                                      args = list(user_caseID = case, 
                                                  user_session = session,
                                                  user_phaseID = phase,
                                                  user_outcome = outcome,
                                                  user_design = studyDesign(),
                                                  user_treatment = input$treatment,
                                                  user_round = round_session))
      )
    } else {
      clean_dat <- c(clean_dat_B,
                     '',
                     parse_code_chunk("clean-inputdata-nofilter", 
                                      args = list(user_caseID = case,
                                                  user_session = session,
                                                  user_phaseID = phase,
                                                  user_outcome = outcome,
                                                  user_design = studyDesign(),
                                                  user_model_center = input$model_center,
                                                  user_treatment = input$treatment,
                                                  user_round = round_session))
      )
    }
  }
  
  # Fit the model
  
  if (input$method=="RML") {
    
    if (studyDesign() == "MB") {
      session_FE <- write_formula(input$FE_base, c("0","1", session))
      trt_FE <- write_formula(input$FE_trt, c("NULL", "trt", paste0(session, "_trt")))
      session_RE <- write_formula(input$RE_base, c("0","1", session))
      trt_RE <- write_formula(input$RE_trt, c("NULL","trt", paste0(session, "_trt")))
    } else {
      session_FE <- if (is.null(input$FE_base) | !(0 %in% input$FE_base)) "0" else "1"
      trt_FE <- if (is.null(input$FE_trt) | !(0 %in% input$FE_trt)) NULL else "trt"
      session_RE <- if (is.null(input$RE_base) | !(0 %in% input$RE_base)) "0" else "1"
      trt_RE <- if (is.null(input$RE_trt) | !(0 %in% input$RE_trt)) NULL else "trt"
    }
    
    fixed <- paste(outcome, "~", paste(c(session_FE, trt_FE), collapse = " + "))
    random <- paste("~", paste(c(session_RE, trt_RE), collapse = " + "), "|", case)
    
    fit_mod <- parse_code_chunk("fit-RML", args = list(user_case = case,
                                                       user_session = session,
                                                       user_fixed = fixed, 
                                                       user_random = random))
  } else {
    fit_mod <- c()
  }
  
  # Calculate effect size
  
  if (input$method == "RML") {
    A <- effect_size()$`Initial treatment time`
    B <- effect_size()$`Follow-up time`
    p_const <- c(rep(0L, length(input$FE_base)), (B - A)^as.integer(input$FE_trt))
    
    # get r_const when centering at an arbitrary time instead of B
    r_dim <- length(input$RE_base) + length(input$RE_trt)
    r_const_dim <- r_dim * (r_dim + 1) / 2
    bc_vec <- (input$B_time - input$model_center)^as.integer(input$RE_base)
    bc_mat <- 2 * tcrossprod(bc_vec) - diag(bc_vec^2)
    r_const_base <- bc_mat[upper.tri(bc_mat, diag = TRUE)]
    r_const_trt <- rep(0, (r_const_dim - length(r_const_base)))
    r_const_cor <- rep(0, length(model_fit()$fit$modelStruct$corStruct))
    r_const_var <- rep(0, length(model_fit()$fit$modelStruct$varStruct))
    r_const <- c(r_const_base, r_const_trt, r_const_cor, r_const_var, 1L)
    
    if (studyDesign() == "MB") {
      calc_ES <- parse_code_chunk("es-RML-MB", args = list(user_A = A,
                                                           user_B = B,
                                                           user_FE_base = paste_object(input$FE_base), 
                                                           user_FE_trt = paste_object(input$FE_trt),
                                                           user_rconst_base = paste_object(r_const_base),
                                                           user_rconst_trt = paste_object(r_const_trt),
                                                           user_rconst_cor = paste_object(r_const_cor),
                                                           user_rconst_var = paste_object(r_const_var),
                                                           user_pconstant = paste_object(p_const),
                                                           user_rconstant= paste_object(r_const)))
    } else {
      calc_ES <- parse_code_chunk("es-RML-TR", args = list(user_rconst_base = paste_object(r_const_base),
                                                           user_rconst_trt = paste_object(r_const_trt),
                                                           user_rconst_cor = paste_object(r_const_cor),
                                                           user_rconst_var = paste_object(r_const_var),
                                                           user_pconstant = paste_object(p_const),
                                                           user_rconstant= paste_object(r_const)))
    }
    
  } else {
    if (studyDesign() == "MB") {
      calc_ES <- parse_code_chunk("es-MB", args = list(user_case = case,
                                                       user_session = session,
                                                       user_outcome = outcome))
    } else {
      phase_pair <- paste0(phase, "_pair")
      calc_ES <- parse_code_chunk("es-ABK", args = list(user_case = case,
                                                        user_session = session,
                                                        user_outcome = outcome,
                                                        user_phase_pair = phase_pair))
    }
  }
  
  SCD_graph <- parse_code_chunk("graph-scd", args = list(user_case = case,
                                                         user_phase = phase,
                                                         user_session = session,
                                                         user_outcome = outcome,
                                                         user_design = studyDesign()))
  
  res <- c(header_res, read_res, clean_dat, '', fit_mod, '', calc_ES, '', SCD_graph, '')
  paste(res, collapse = "\n")
})
