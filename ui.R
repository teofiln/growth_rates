library(shiny)
library(shinythemes)

# get a named list of the active experiments 
# not used
#EXPS <- as.list(dir(pattern="*.csv"))
#names(EXPS) <- EXPS  # to be used in selectInput("Experiment"...) bellow

# update the datasets 
# creates symbolic links from original files in different folder
# will work only localy
# not used
#system("./update_datasets.sh")
#system("./copy_datasets.sh")

shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  titlePanel(h4("Cyclotella salinity experiments")),
  tabsetPanel(id = "tabsets",
              tabPanel("Data", value = "tab0",
                       sidebarLayout(fluid = TRUE,
                                     sidebarPanel(width = 2.5,
                                                  fluidRow( column(width=3,
                                                  radioButtons("Experiment0",
                                                               label = h5("Experiment"),
                                                               choices = list("C. wallercreekii tubes salinity" = 1, 
                                                                              "C. wallercreekii tubes temperature" = 2, 
                                                                              "C. cryptica tubes salinity" = 3, 
                                                                              "C. wallercreekii flasks salinity" = 4, 
                                                                              "C. wallercreekii flasks 2 salinity" = 5,
                                                                              "C. cryptica flasks salinity" = 6),
                                                               selected = 1)),
                                                  column( width = 3,
                                                          HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#newData'>Upload new data</button>"),
                                                          div(id = "newData", class = "collapse",
                                                              wellPanel(
                                                                actionButton("uploadNewData",
                                                                             label="Upload tab delimited file"),
                                                                hr(),
                                                                actionButton("displayNewData",
                                                                             label="Display new data"),
                                                                hr(),
                                                                actionButton("submitNewData",
                                                                             label="Submit new data")
                                                                ) # end wellPanel
                                                              ) # end collapsable div
                                                          ),
                                                  
                                                  column( width= 3,
                                                  HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#Transfer'>Prepare transfer</button>"),
                                                  div(id = "Transfer", class = "collapse",
                                                      wellPanel(
                                                        numericInput("finalVolume", 
                                                                     label=h5("Final volume (mL) in new transfer vessel"),
                                                                     value=4),
                                                        numericInput("desiredRFU", 
                                                                     label=h5("Desired fluorescence (RFU/mL) at start of next transfer"),
                                                                     value=500),
                                                        actionButton("calculateTransferVolumes",
                                                                     label="Calculate transfer volumes"),
                                                        downloadButton("downloadTransferSheet",
                                                                       label="Download")#,
#                                                 hr(),
#                                                 strong("Caution: Clicking 'Submit Transfer' will add a day's worth of measurements to the dataset. 
#                                                            Use only when Transfer is actually performed!"),
#                                                 hr(),
#                                                 actionButton("submitTransfer", label="Submit Transfer")
                                                      ) # end wellPanel

                                            )))#, # end collapsable div

                                     ), # end sidebarPanel
                                     mainPanel(width = 9.5,
                                       dataTableOutput("Table0")
                                       ) # end main panel
                       ) # end sidebarPanel
              ), # end tab0
                                                  
              tabPanel("Growth curves", value = "tab1",
                       sidebarLayout(fluid = TRUE,
                         sidebarPanel(width = 3,
                                      radioButtons("Experiment1",
                                                   label = h5("Experiment"),
                                                   choices = list("C. wallercreekii tubes salinity" = 1, 
                                                                  "C. wallercreekii tubes temperature" = 2, 
                                                                  "C. cryptica tubes salinity" = 3, 
                                                                  "C. wallercreekii flasks salinity" = 4, 
                                                                  "C. wallercreekii flasks 2 salinity" = 5,
                                                                  "C. cryptica flasks salinity" = 6),
                                                   selected = 1),
                                      HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#PlotControls'>Customize</button>"),
                                      uiOutput("whichSelectInput1")
                                      ), # end sidebarPanel
                         mainPanel(width = 9,
                                       plotOutput('Plot1', height = 800)
                                   ) # end mainPanel
                          ) # end first sidebarLayot
                       ), # end first tab
            
            tabPanel("Compare slopes", value = "tab2",
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                    radioButtons("Experiment2",
                                                 label = h5("Experiment"),
                                                 choices = list("C. wallercreekii tubes salinity" = 1, 
                                                                "C. wallercreekii tubes temperature" = 2, 
                                                                "C. cryptica tubes salinity" = 3),
                                                 selected = 1),
                                    HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#PlotControls2'>Customize</button>"),
                                    uiOutput("whichSelectInput")
                                    ),
                       mainPanel(width = 10,
                                 plotOutput('Plot2', height = 500),
                                 tableOutput('TableAOV')
                                 ) # end mainPanel
                       ) # end second sidebarPanel
                     ), # end second tab

            tabPanel("Growth rates through time", value = "tab4",
                     sidebarLayout(
                       sidebarPanel(width = 3,
                                    radioButtons("Experiment4",
                                                 label = h5("Experiment"),
                                                 choices = list("C. wallercreekii tubes salinity" = 1, 
                                                                "C. wallercreekii tubes temperature" = 2, 
                                                                "C. cryptica tubes salinity" = 3),
                                                 selected = 1),
                                    HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#PlotControls4'>Customize</button>"),
                                    uiOutput("whichCheckBoxInput4")#,
                       ),
                       mainPanel(width = 9,
                                 plotOutput('Plot4', height = 800)
                       ) # end mainPanel
                     ) # end third sidebarPanel
            ), # end third tab

            tabPanel("Compare growth rates", value = "tab3",
                     sidebarLayout(
                       sidebarPanel(width = 3,
                                    radioButtons("Experiment3",
                                                 label = h5("Experiment"),
                                                 choices = list("C. wallercreekii tubes salinity" = 1, 
                                                                "C. wallercreekii tubes temperature" = 2, 
                                                                "C. cryptica tubes salinity" = 3),
                                                 selected = 1),
                                    HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#PlotControls3'>Customize</button>"),
                                    uiOutput("whichCheckBoxInput3")#,
                       ),
                       mainPanel(width = 9,
                                 plotOutput('Plot3', height = 800)#,
                       ) # end mainPanel
                     ) # end fourth sidebarPanel
                  ) # end fourth tab
        ) # end tabsetPanel

)) # end shinyUI and fluidPage
