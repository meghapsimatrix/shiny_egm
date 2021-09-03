library(shiny)
library(shinythemes)
library(robumeta)
library(DT)
library(knitr)
library(tidyverse)
library(kableExtra)
library(plotly)

source("ui.R")
source("server.R")
source("extra/example.R")
source("tidy_meta.R")


shinyApp(ui = ui, server = server)
