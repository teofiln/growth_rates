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
WSALT <- ALLDATA[[5]]
WTEMPslope <- ALLDATA[[6]]
WSALTslope <- ALLDATA[[7]]

shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  titlePanel(h4("Salinity experiments")),
  tabsetPanel(position = "above", id = "tabsets",
              
              tabPanel("Growth curves", value = "tab1",
                       sidebarLayout(
                         sidebarPanel(width = 2,
                                      radioButtons("Experiment",
                                                   label = h4("Experiment"),
                                                   choices = list("salinity" = 1, "temperature" = 2, "flask" = 3, "flask2" = 4),
                                                   selected = 1),
                                      
                                      uiOutput("whichCondPanel")
                                      ), # end sidebarPanel
                         mainPanel(width = 10,
                                   plotOutput('Plot1', height = 800)
                                   ) # end mainPanel
                         ) # end first sidebarLayot
                       ), # end first tab
            
            tabPanel("Slopes though time", value = "tab2",
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                    radioButtons("Experiment2",
                                                 label = h4("Experiment"),
                                                 choices = list("salinity" = 1, "temperature" = 2),
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
            
            tabPanel("Compare growth rates", value = "tab3",
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                    radioButtons("Experiment3",
                                                 label = h4("Experiment"),
                                                 choices = list("salinity" = 1, "temperature" = 2),
                                                 selected = 1),
                                    
                                    uiOutput("whichCheckBoxInput3")#,
                                   # actionButton("actionPlot3", "Plot")
                       ),
                       mainPanel(width = 10,
                                 plotOutput('Plot3.1', height = 400),
                                 plotOutput('Plot3.2', height = 400)#,
                                 #tableOutput('TableAOV3')
                       ) # end mainPanel
                     ) # end second sidebarPanel
                  ) # end second tab
            
            ) # end tabsetPanel

)) # end shinyUI and fluidPage
