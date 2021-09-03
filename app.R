library(shiny)
library(shinythemes)
library(robumeta)
library(dplyr)
library(ggplot2)
library(DT)

source("ui.R")
source("server.R")
source("extra/example.R")


shinyApp(ui = ui, server = server)
