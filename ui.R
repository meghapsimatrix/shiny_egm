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
                                 c("Use effect size level data" = "esdat",
                                   "Use summary level data" = "sumdat")),
                    
                    radioButtons('dat_type', 
                                 'What data do you want to use?',
                                 c("Use an example" = "example",
                                   "Upload data from a .csv or .txt file" = "dat",
                                   "Upload data from a .xlsx file" = "xlsx")),
                    
                    
                    conditionalPanel(
                      condition = "input.dat_type == 'example'",
                      selectInput("example", label = "Choose an example", 
                                  choices = exampleChoices)
                    ),
                    
                    conditionalPanel(
                      condition = "input.dat_type == 'dat'",
                      fileInput('dat', 'Upload a .csv or .txt file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.txt')),
                      checkboxInput('header', 'File has a header?', TRUE),
                      radioButtons('sep', 'Data seperator', c(Commas=',', Semicolons=';', Tabs='\t', Spaces=' ')),
                      radioButtons('quote', 'Include quotes?', c('No'='', 'Double Quotes'='"', 'Single Quotes'="'"))
                    ),
                    
                    
                    conditionalPanel(
                      condition = "input.dat_type == 'xlsx'",
                      fileInput('xlsx', 'Upload a .xlsx file', accept = c('.xlsx')),
                      checkboxInput('col_names', 'File has a header?', TRUE),
                      selectInput("inSelect", "Select a sheet", "")
                    ),
                    
                    
                    
             ),
             
             
             
             column(8,
                    
                    conditionalPanel(
                      
                      condition = "input.summary_raw == 'esdat' & input.dat_type == 'dat' || input.summary_raw == 'esdat' & input.dat_type == 'xlsx'",
                      
                      uiOutput("esMapping"),
                      
                      uiOutput("varMapping"),
                      
                      uiOutput("studyMapping"),
                      
                      uiOutput("esidMapping"),

                      uiOutput("xMapping"),

                      uiOutput("yMapping")
                      
                    ),
                    
                    conditionalPanel(

                      condition = "input.summary_raw == 'sumdat' & input.dat_type == 'dat' || input.summary_raw == 'sumdat' & input.dat_type == 'xlsx'",
                      
                      uiOutput("nstudyMapping"),
                      
                      uiOutput("xsumMapping"),
                      
                      uiOutput("ysumMapping"),
                      
                      uiOutput("avesMapping")
                    ),
             ),
      
             
           ),
           fluidRow(br(),br(),br())
  ),
  
  tabPanel("Examine Dataset",
           dataTableOutput("contents")),
  
  tabPanel("Evidence Gap Map",
           
           br(),
           sidebarPanel("",
                    selectInput("overlay", label = "What do you want to overlay on the dots?",
                                choices = c("Number of Studies" = "nstudy", 
                                            "Average Effect Size" = "aves", 
                                            "Nothing" = "nothing"), 
                                selected = "nothing")
             ), 
           mainPanel("",
                    plotlyOutput("egmPlot")
           
             )
           ),
  

  
  tabPanel("R Syntax",
           rclipboardSetup(),
           uiOutput("clip"),
           verbatimTextOutput("syntax"))
  
)
)  
  
        