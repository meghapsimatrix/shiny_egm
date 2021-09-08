
column(8,
       conditionalPanel(
         
         condition = "input.summary_raw == 'sumdat' & input.dat_type == 'dat' || input.summary_raw == 'sumdat' & input.dat_type == 'xlsx'",
         column(12, br()),
         strong("Please specify the first factor for the EGM."),
         uiOutput("xMapping"),
         column(12, br()),
         strong("Please specify the second factor for the EGM."),
         uiOutput("yMapping"),
         column(12, br()),
         strong("Please specify the variable in the dataset containing the number of effect sizes per cell."),
         uiOutput("nstudyMapping"),
         
       )
       



source("extra/example.R")

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  # App title
  titlePanel("Evidence Gap Maps"),
  
  tabsetPanel(type = "tabs",
              
              tabPanel("About"),
              
              tabPanel("Load Data",
                       br(),
                       fluidRow(
                         column(4,
                                radioButtons('summary_raw',
                                             'Do you want to use summary level data or effect size level data?',
                                             c("Use summary level data" = "summary",
                                               "Use effect size level data" = "esdata")),
                                radioButtons('dat_type', 
                                             'What data do you want to use?',
                                             c("Use an example" = "example",
                                               "Upload data from a .csv or .txt file" = "dat",
                                               "Upload data from a .xlsx file" = "xlsx")),
                                conditionalPanel(
                                  condition = "input.summary_raw == 'summary' & input.dat_type == 'example' || input.summary_raw == 'esdata' & input.dat_type == 'example'",
                                  selectInput("example", label = "Choose an example", 
                                              choices = exampleChoices)
                                ),
                                conditionalPanel(
                                  condition = "input.summary_raw == 'summary' & input.dat_type == 'dat' || input.summary_raw == 'esdata' & input.dat_type == 'dat'",
                                  fileInput('dat', 'Upload a .csv or .txt file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.txt')),
                                  checkboxInput('header', 'File has a header?', TRUE),
                                  radioButtons('sep', 'Data seperator', c(Commas=',', Semicolons=';', Tabs='\t', Spaces=' ')),
                                  radioButtons('quote', 'Include quotes?', c('No'='', 'Double Quotes'='"', 'Single Quotes'="'"))
                                ),
                                conditionalPanel(
                                  condition = "input.summary_raw == 'summary' & input.dat_type == 'xlsx' || input.summary_raw == 'esdata' & input.dat_type == 'xlsx'",
                                  fileInput('xlsx', 'Upload a .xlsx file', accept = c('.xlsx')),
                                  checkboxInput('col_names', 'File has a header?', TRUE),
                                  selectInput("inSelect", "Select a sheet", "")
                                ),
                         ),
                         column(8)
                         conditionalPanel(
                           condition = "input.summary_raw == 'esdata' & input.dat_type == 'dat' || input.summary_raw == 'esdata' & input.dat_type == 'xlsx'",
                           column(12, br()),
                           strong("Please specify the variable in the dataset containing the effect sizes."),
                           uiOutput("esMapping"),
                           column(12, br()),
                           strong("Please specify the variable in the dataset containting the variance of the effect sizes."),
                           uiOutput("varMapping"),
                           column(12, br()),
                           strong("Please specify the variable with the study identifier."),
                           uiOutput("studyMapping"),
                           column(12, br()),
                           strong("Please specify the variable with the effect size identifier."),
                           uiOutput("esidMapping"),
                           column(12, br()),
                           strong("Please specify the first factor for the EGM."),
                           uiOutput("xMapping"),
                           column(12, br()),
                           strong("Please specify the second factor for the EGM."),
                           uiOutput("yMapping"),
                         ),
                         conditionalPanel(
                           condition = "input.summary_raw == 'summary' & input.dat_type == 'dat' || input.summary_raw == 'summary' & input.dat_type == 'xlsx'",
                           column(12, br()),
                           strong("Please specify the first factor for the EGM."),
                           uiOutput("xMapping"),
                           column(12, br()),
                           strong("Please specify the second factor for the EGM."),
                           uiOutput("yMapping"),
                           column(12, br()),
                           strong("Please specify the variable containing the number of studies per cell."),
                           uiOutput("nstudyMapping")
                           
                         ),
                         
                         
                       ),
                       fluidRow(br(),br(),br())
              ),