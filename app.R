library(shiny)
library(shinythemes)
library(robumeta)
library(DT)
library(knitr)
library(tidyverse)
library(kableExtra)
library(rclipboard)
library(metafor)
library(clubSandwich)
library(janitor)
library(estimatr)
library(readxl)
library(shinybusy)


source("ui.R")
source("server.R")
source("tidy_meta.R")
source("plot_egm.R")
source("parse_syntax.R")
# Run the application 
shinyApp(ui = ui, server = server)



