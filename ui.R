library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  titlePanel(h4("Cyclotella salinity experiments")),
  tabsetPanel(id = "tabsets",
    tabPanel("Data", value = "tab0",
      sidebarLayout(fluid = TRUE,
        sidebarPanel(width = 3,
          radioButtons("Experiment0",
            label = h5("Experiment"),
              choices = list("C. wallercreekii tubes salinity" = 1,
                             "C. wallercreekii flasks salinity" = 2,
                             "C. wallercreekii flasks 2 salinity" = 3,
                             "C. cryptica tubes salinity" = 4,
                             "C. cryptica flasks salinity" = 5,
                             "C. wallercreekii tubes temperature" = 6),
                selected = 1),
        wellPanel(
          HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#newData'>Upload new data</button>"),
          div(id = "newData", class = "collapse",
#               tags$hr(),
#               HTML("Enter the number of strains and treatments based on the selected experiment and select the tab-delimited file containing in vivo relative fluorescence."),
#               tags$hr(),
#             checkboxInput('header', 'Header', FALSE),
#             numericInput('skip_lines', label="How many lines to skip", value=0),
#             radioButtons('sep', 'Separator',
#               c(Comma=',',
#               Semicolon=';',
#               Tab='\t'),
#               selected='\t'),
#             radioButtons('quote', 'Quote',
#               c(None='',
#               'Double Quote'='"',
#               'Single Quote'="'"),
#               selected=''),
            numericInput("numTreat", "Number of treatments", value = 5),
            numericInput("numStrain", "Number of strains", value = 5),
            tags$hr(),
            fileInput('file1', 'Choose file to upload',
                      accept = c(
                        #'text/csv',
                        #'text/comma-separated-values',
                        'text/tab-separated-values',
                        #'text/plain',
                        #'.csv',
                        '.tsv') ),
             strong("Caution: Clicking 'Submit new data' will add a day's worth of measurements to the dataset. 
                    Make sure the correct file is loaded before commiting the changes."),
             tags$hr(),
             actionButton("submitNewData", "Submit new data", class='btn btn-danger'),
             tags$hr(),
             h4(textOutput("uploadSubmitted")),
             strong("Clicking 'Clear' and refreshing the browser will purge the newly uploaded data in case of problems."),
             tags$hr(),
             actionButton("clearUpload", label="Clear", class='btn btn-success', icon("trash"))
            ) # end collapsable div
          ), # end wellPanel
        wellPanel(
          HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#Transfer'>Prepare transfer</button>"),
          tags$hr(),
          div(id = "Transfer", class = "collapse",
          numericInput("finalVolume", min=1, max=1000,
            label="Final volume (mL) in new transfer vessel", value=4),
          numericInput("desiredRFU",                       
            label="Fluorescence (RFU/mL) at start of next transfer", value=500),
          actionButton("calculateTransfer", label="Calculate", class='btn btn-warning'),
          downloadButton("downloadTransferSheet", label="Download", class='btn btn-success'),
          actionButton("clearTransfer", label="Clear", class='btn btn-success', icon("trash")),
          hr(),
          strong("Caution: Clicking 'Submit Transfer' will add a day's worth of measurements to the dataset. 
                  Use only when Transfer is actually performed."),
          hr(),
          actionButton("submitTransfer", label="Submit Transfer", class='btn btn-danger'),
          tags$hr(),
          h4(textOutput("transferSubmitted"))
          ) # end collapsible div
        ) # end wellPanel
      ), # end sidebarPanel

          mainPanel(width = 9,
            HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#allData'>All data</button>"),
            div(id = "allData", class = "collapse in",
              dataTableOutput("Table0a")),
            HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#newUpload'>Newly uploaded</button>"),
            div(id = "newUpload", class = "collapse",
              dataTableOutput("Table0b")),
            HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#transferData'>Transfer sheet</button>"),
            div(id = "transferData", class = "collapse",
              dataTableOutput("Table0c")),
            HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#RNA'>RNA</button>"),
            div(id = "RNA", class = "collapse",
                dataTableOutput("Table0d"))
            ) # end main panel
        ) # end sidebarLayout
    ), # end tab0
                                                  
    tabPanel("Growth curves", value = "tab1",
      sidebarLayout(fluid = TRUE,
        sidebarPanel(width = 3,
          radioButtons("Experiment1",
            label = h5("Experiment"),
            choices = list("C. wallercreekii tubes salinity" = 1,
                           "C. wallercreekii flasks salinity" = 2,
                           "C. wallercreekii flasks 2 salinity" = 3,
                           "C. cryptica tubes salinity" = 4,
                           "C. cryptica flasks salinity" = 5,
                           "C. wallercreekii tubes temperature" = 6),
                      selected = 1),
          HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#PlotControls'>Customize</button>"),
          uiOutput("whichSelectInput1")
          ), # end sidebarPanel
        mainPanel(width = 9,
          plotOutput('Plot1', height = 800)
        ) # end mainPanel
      ) # end sidebarLayot
    ), # end tab
  
  tabPanel("Slopes", value = "tab2",
    sidebarLayout(fluid = TRUE,
      sidebarPanel(width = 3,
        radioButtons("Experiment10",
          label = h5("Experiment"),
            choices = list("C. wallercreekii tubes salinity" = 1,
                           "C. cryptica tubes salinity" = 2),
                      selected = 1),
            HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#PlotControls10'>Customize</button>"),
            uiOutput("whichSelectInput10")
            ), # end sidebarPanel
      mainPanel(width = 9,
        plotOutput('Plot10', height = 800)
      ) # end mainPanel
    ) # end sidebarLayot
  ), # end tab

  tabPanel("Experiments log",
           includeMarkdown(path = "./Experiments_log.Rmd")
           )
  ) # end tabsetPanel

)) # end shinyUI and fluidPage
