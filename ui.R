
ui <- fluidPage(
  
  theme = shinytheme("yeti"),
  
  # App title
  titlePanel("Evidence Gap Maps"),
  
  tabsetPanel(type = "tabs",
              
              tabPanel("About",
                       br(),
                       navlistPanel(widths = c(3, 9),
                                    tabPanel("Evidence Gap Maps", includeMarkdown("markdown_chunks/about.md")),
                                    tabPanel("Instructions to Use the App", includeMarkdown("markdown_chunks/instructions.md"))
                       )),
              
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
                                  radioButtons('sep', 'Seperator', c(Commas=',', Semicolons=';', Tabs='\t', Spaces=' '))
                                  #radioButtons('quote', 'Include quotes?', c('No'='', 'Double Quotes'='"', 'Single Quotes'="'"))
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
              
              tabPanel("Create Summary Data",
                       
                       br(),
                       sidebarPanel("",
                                  
                                
                              conditionalPanel(
                                  
                                  condition = "input.ex_upload == 'up' & input.summary_raw == 'sumdat'",
                                  
                                  textOutput("noparam")
                                  
                                ),
                                
                                
                              conditionalPanel(

                                  condition = "input.ex_upload == 'example' || 
                                               input.ex_upload == 'up' & input.summary_raw == 'esdat'",
                                  
                                  # radioButtons('model',
                                  #              'Which model do you want to use to calculate the average effect sizes?',
                                  #              c("Correlated Effects Model" = "ce",
                                  #                "Hierarchical Effects Model" = "he")),
                                  
                                  
                                  textOutput("explain"),

                                  br(),
                                  
                                  sliderInput("rho",
                                              "For the correlated effects model, what value would you like to use for the within-study correlation between effect sizes?",
                                              min = 0, max = 1, value = 0.8),
                                ),
                                
                                br(),
                                actionButton("go", "Create Summary Data"),
                                
                                
                         ),
                       
                       mainPanel("",
                                 fluidRow(br(),br()),
                                 dataTableOutput("contents")
                                )
                         

              ),


  
              # tabPanel("Run Analyses and Examine",
              #          br(),
              #          actionButton("go", "Create Summary Data"),
              #          fluidRow(br(),br()),
              #          dataTableOutput("contents")),
              
              tabPanel("Create Evidence Gap Map",
                       
                       br(),
                       sidebarPanel("",
                                    tabsetPanel(
                                      tabPanel("Create Plot",
                                               
                                               conditionalPanel(
                                                 condition = "input.ex_upload == 'example'||
                                                               input.ex_upload == 'up' & input.summary_raw == 'esdat' ||
                                                               input.ex_upload == 'up' & input.summary_raw == 'sumdat' & input.aves != 'None'",
                                                 
                                                 selectInput("escolor", 
                                                             label = "Do you want to map average effect size onto a continous color?", 
                                                             choices = c("Yes" = "yes",
                                                                         "No" = "no"),
                                                             selected = "yes"),
                                                 
                                               ),
                                    
                                                selectInput("overlay", 
                                                            label = "What do you want to overlay on the dots?",
                                                            choices = c("Number of Studies" = "nstudy", 
                                                                        "Average Effect Size" = "aves", 
                                                                        "Nothing" = "nothing"), 
                                                            selected = "nothing"),
                                               
                                                textInput("xlabel", label = "Please specify the label for the x-axis. Please leave the box blank if you do not want to label the axis.", value = ""),
                                                textInput("ylabel", label = "Please specify the label for the y-axis. Please leave the box blank if you do not want to label the axis.", value = ""),
                                                
                                                conditionalPanel(
                                                  condition = "input.ex_upload == 'example' & input.num_factors == 'three' & input.escolor == 'no'||
                                                               input.ex_upload == 'up' & input.summary_raw == 'esdat' & input.z !=  'None' & input.escolor == 'no' ||
                                                               input.ex_upload == 'up' & input.summary_raw == 'sumdat' & input.zsum != 'None' & input.escolor == 'no'",
                                                  
                                                  textInput("colorlabel", label = "Please specify the label for the colors. Please leave the box blank if you do not want to label the colors.", value = "")
                                                  
                                                ),
                                               
                                               conditionalPanel(
                                                 condition = "input.ex_upload == 'example' & input.num_factors == 'three' & input.escolor == 'yes' ||
                                                              input.ex_upload == 'up' & input.summary_raw == 'esdat' & input.z !=  'None' & input.escolor == 'yes' ||
                                                              input.ex_upload == 'up' & input.summary_raw == 'sumdat' & input.zsum != 'None' & input.escolor == 'yes'",
                                                 
                                                 textInput("shapelabel", label = "Please specify the label for the shapes. Please leave the box blank if you do not want to label the colors.", value = "")
                                                 
                                               ),
                                               
                                               
                                                actionButton("plot", "Create Plot")
                                                
                                      ),
                                      
                                      tabPanel("Download Plot",
                                               textInput("pname", label = "Download Plot: Please specify the name of the plot.", value = "egm_plot.png"),
                                               sliderInput("width", "Download Plot: Please specify the width (in).",
                                                           min = 1, max = 15, value = 7),
                                               sliderInput("height", "Download Plot: Please specify the height (in).",
                                                           min = 1, max = 15, value = 7),
                                               downloadButton('dndPlot','Download Plot')
                                               
                                      )
                                      
                                    ),


                       ), 
                       
                       mainPanel("",
                                 
                                 plotOutput("egmPlot")
                                 
                       )
              ),
              
              
              tabPanel("R Syntax",
                       rclipboardSetup(),
                       uiOutput("clip"),
                       verbatimTextOutput("syntax"))
              
  )
)  
                       

 