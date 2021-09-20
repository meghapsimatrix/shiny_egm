
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
                                
                                radioButtons('ex_upload',
                                             'Do you want to use an example or upload your own data?',
                                             c("Use an example" = "example",
                                               "Upload my own data" = "up")),
                                
                                conditionalPanel(
                                  condition = "input.ex_upload == 'up'",
                                  
                                  radioButtons('summary_raw',
                                               'Do you want to use summary level data or effect size level data?',
                                               c("Effect size level data" = "esdat",
                                                 "Summary level data" = "sumdat")),
                                  
                                  radioButtons('dat_type', 
                                               'What data do you want to use?',
                                               c("Upload a .csv or .text file" = "dat",
                                                 "Upload an .xlsx file" = "xlsx"))
                                ),
                                
                                
                                
                                conditionalPanel(
                                  condition = "input.ex_upload == 'up' & input.dat_type == 'dat'",
                                  fileInput('dat', 'Upload a .csv or .txt file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.txt')),
                                  checkboxInput('header', 'File has a header?', TRUE),
                                  radioButtons('sep', 'Seperator', c(Commas=',', Semicolons=';', Tabs='\t', Spaces=' ')),
                                  radioButtons('quote', 'Include quotes?', c('No'='', 'Double Quotes'='"', 'Single Quotes'="'"))
                                ),
                                
                                
                                conditionalPanel(
                                  condition = "input.ex_upload == 'up' & input.dat_type == 'xlsx'",
                                  fileInput('xlsx', 'Upload an .xlsx file', accept = c('.xlsx')),
                                  checkboxInput('col_names', 'File has a header?', TRUE)
                                  #selectInput("inSelect", "Select a sheet", "")
                                ),
                         ),
                                
  
                       
                       
                       column(8,
                              
                              conditionalPanel(
                                
                                condition = "input.ex_upload == 'example'",
                                
                                radioButtons("num_factors", label = "Do you want to use two factors or three factors for the EGM Plot?", 
                                             c("Two Factors" = "two",
                                               "Three Factors" = "three"))
                              ),
                              
                              conditionalPanel(
                                
                                condition = "input.ex_upload == 'up' & input.summary_raw == 'esdat' & input.dat_type == 'dat' || 
                      input.ex_upload == 'up' & input.summary_raw == 'esdat' & input.dat_type == 'xlsx'",
                                
                                uiOutput("xMapping"),
                                
                                uiOutput("yMapping"),
                                
                                uiOutput("zMapping"),
                                
                                uiOutput("esMapping"),
                                
                                radioButtons("sevar", label = "Do you want to input variance or standard error of the effect sizes?", 
                                             c("Variance" = "var",
                                               "Standard Error" = "se")),
                                
                                uiOutput("varMapping"),
                                
                                uiOutput("studyMapping")
                                
                                #uiOutput("esidMapping")
                                
                                
                              ),
                              
                              conditionalPanel(
                                
                                condition = "input.ex_upload == 'up' & input.summary_raw == 'sumdat' & input.dat_type == 'dat' || 
                      input.ex_upload == 'up' & input.summary_raw == 'sumdat' & input.dat_type == 'xlsx'",
                                
                                uiOutput("xsumMapping"),
                                
                                uiOutput("ysumMapping"),
                                
                                uiOutput("zsumMapping"),
                                
                                uiOutput("nstudyMapping"),
                                
                                uiOutput("avesMapping")
                              ),
                              
                              
                       ),
                       
                       
              ),
              fluidRow(br(),br(),br())
              ),
  
              tabPanel("Examine Summary Data",
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
                                 downloadButton('dndPlot','Download Plot'),
                                 plotOutput("egmPlot")
                                 
                       )
              ),
              
              
              tabPanel("R Syntax",
                       rclipboardSetup(),
                       uiOutput("clip"),
                       verbatimTextOutput("syntax"))
              
  )
)  
                       

 