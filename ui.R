library(shiny)
library(shinythemes)
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)
library(HH)

# get a named list of the active experiments 
# not used
#EXPS <- as.list(dir(pattern="*.csv"))
#names(EXPS) <- EXPS  # to be used in selectInput("Experiment"...) bellow

# update the datasets 
# creates symbolic links from original files in different folder
# will work only localy
#system("./update_datasets.sh")
#system("./copy_datasets.sh")

source("./load_prep_data.R")
ALLDATA <- load_prep_data()

DAT <- ALLDATA[[1]]
WTEMPsplit <- ALLDATA[[2]]
WTEMP <- ALLDATA[[3]]
WSALTsplit <- ALLDATA[[4]]
WSALT <- rbind(ALLDATA[[13]], ALLDATA[[5]])
WTEMPslopes <- ALLDATA[[6]]
WSALTslopes <- ALLDATA[[7]]
CSALT <- ALLDATA[[8]]
CSALTsplit <- ALLDATA[[9]]
CSALTslopes <- ALLDATA[[10]]
WFLAS <- ALLDATA[[11]]
WFLAS2 <- ALLDATA[[12]]

shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  titlePanel(h4("Salinity experiments")),
  tabsetPanel(position = "above", id = "tabsets",
              
              tabPanel("Growth curves", value = "tab1",
                       sidebarLayout(
                         sidebarPanel(width = 2,
                                      radioButtons("Experiment1",
                                                   label = h4("Experiment"),
                                                   choices = list("salinity" = 1, 
                                                                  "temperature" = 2, 
                                                                  "cryptica" = 3, 
                                                                  "flask" = 4, 
                                                                  "flask2" = 5),
                                                   selected = 1),
                                      
                                      uiOutput("whichSelectInput1")
                                      ), # end sidebarPanel
                         mainPanel(width = 10,
                                   plotOutput('Plot1', height = 800)
                                   ) # end mainPanel
                         ) # end first sidebarLayot
                       ), # end first tab
            
            tabPanel("Compare slopes", value = "tab2",
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                    radioButtons("Experiment2",
                                                 label = h4("Experiment"),
                                                 choices = list("salinity" = 1, "temperature" = 2, "cryptica" = 3),
                                                 selected = 1),
                                    
                                    uiOutput("whichSelectInput")
                                    ),
                       mainPanel(width = 10,
                                 plotOutput('Plot2', height = 500),
                                 tableOutput('TableAOV')
#                                  fluidRow(
#                                    column(4, plotOutput('Plot2.2', height = 400)),
#                                    column(8, tableOutput('TableAOV'))
#                                  )
                                 ) # end mainPanel
                       ) # end second sidebarPanel
                     ), # end second tab

            tabPanel("Growth rates through time", value = "tab4",
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                    radioButtons("Experiment4",
                                                 label = h4("Experiment"),
                                                 choices = list("salinity" = 1, "temperature" = 2, "cryptica" = 3),
                                                 selected = 1),
                                    uiOutput("whichCheckBoxInput4")#,
                                    # actionButton("actionPlot3", "Plot")
                       ),
                       mainPanel(width = 10,
                                 plotOutput('Plot4', height = 800)
                       ) # end mainPanel
                     ) # end fourth sidebarPanel
            ), # end fourt tab

            tabPanel("Compare growth rates", value = "tab3",
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                    radioButtons("Experiment3",
                                                 label = h4("Experiment"),
                                                 choices = list("salinity" = 1, "temperature" = 2, "cryptica" = 3),
                                                 selected = 1),
                                    uiOutput("whichCheckBoxInput3")#,
                                   # actionButton("actionPlot3", "Plot")
                       ),
                       mainPanel(width = 10,
                                 plotOutput('Plot3', height = 800)#,
                                 #plotOutput('Plot3.2', height = 400)#,
                                 #tableOutput('TableAOV3')
                       ) # end mainPanel
                     ) # end third sidebarPanel
                  ) # end third tab
        ) # end tabsetPanel

)) # end shinyUI and fluidPage
