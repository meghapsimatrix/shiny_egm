# Load data and clean -----------------------------------------------------

# Load data
library(janitor)

dat <- 
  read.table("{user_path}", header = {user_header}, sep = "{user_sep}", 
             stringsAsFactors = FALSE) %>% # Modify the path to the full location of your file
  clean_names(case = "parsed")