# Load data and clean -----------------------------------------------------

dat <- 
  read_excel(path = "{user_path}", sheet = 1) %>% # Modify the path to the full location of your file
  clean_names(case = "parsed")