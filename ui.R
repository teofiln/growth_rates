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

shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  titlePanel(h4("Salinity experiments")),
  tabsetPanel(position = "above", id = "tabsets",
              
              tabPanel("Growth curves", value = "tab1",
                       sidebarLayout(
                         sidebarPanel(width = 2,
                                      radioButtons("Experiment",
                                                   label = h4("Experiment"),
                                                   choices = list("salinity" = 1, "temperature" = 2, "flask" = 3),
                                                   selected = 2),
                                      
                                      uiOutput("whichCondPanel")
                                      ), # end sidebarPanel
                         mainPanel(width = 10,
                                   plotOutput('Plot1', height = 700)
                                   ) # end mainPanel
                         ) # end first sidebarLayot
                       ), # end first tab
            
            tabPanel("Slopes though time", value = "tab2",
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                    radioButtons("Experiment2",
                                                 label = h4("Experiment"),
                                                 choices = list("salinity" = 1, "temperature" = 2),
                                                 selected = 2),
                                    
                                    uiOutput("whichSelectInput")
                                                                  
                                    #actionButton("nextCase", "Next case", icon = NULL )
                                    ),
                       mainPanel(width = 10,
                                 plotOutput('Plot2', height = 500),
                                 tableOutput('TableAOV')
                                 ) # end mainPanel
                       ) # end second sidebarPanel
                     ) # end second tab
            ) # end tabsetPanel

)) # end shinyUI and fluidPage
