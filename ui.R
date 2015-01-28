library(shiny)
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
system("./update_datasets.sh")

# read all experiments: waller_salinity, waller_temperature, waller_flasks, cryptica_salinity
Wfami <- read.csv("./WFAMI.csv", header=TRUE)
Wsalt <- read.csv("./WSALT.csv", header=TRUE)
Wtemp <- read.csv("./WTEMP.csv", header=TRUE)
Wflas <- read.csv("./WFLAS.csv", header=TRUE)
Csalt <- read.csv("./CSALT.csv", header=TRUE)

DAT <- rbind(Wfami, Wsalt, Wtemp, Wflas, Csalt)
DAT <- mutate(.data=DAT, Rep=factor(Replicate), 
                        Transfer=as.numeric(Transfer), 
                        Treatment=factor(Treatment),
                        Temperature=factor(Temperature),
                        Media=factor(Media),
                        Experiment=factor(Experiment),
                        Strain=factor(Strain))

shinyUI(fluidPage(#theme = shinytheme("flatly"),
  
  titlePanel(h3("Salinity experiments")),
  
  tabPanel("Growth curves",
           sidebarLayout(
             sidebarPanel(width = 2,
               radioButtons("Experiment", 
                           label = h4("Experiment"),
                           choices = list("salinity" = 1, "temperature" = 2, "flask" = 3), 
                           selected = 1 ),
             
             conditionalPanel(condition= "input.Experiment == 1",
                                     checkboxGroupInput("Strain1", 
                                                        label = h4("Strain"),
                                                        choices = levels(droplevels(DAT[DAT$Experiment=="salinity",]$Strain)), 
                                                        selected = levels(droplevels(DAT[DAT$Experiment=="salinity",]$Strain))[1:5]),
                                     checkboxGroupInput("Treatment1", 
                                                        label = h4("Treatment"),
                                                        choices = levels(droplevels(DAT[DAT$Experiment=="salinity",]$Treatment)), 
                                                        selected = levels(droplevels(DAT[DAT$Experiment=="salinity",]$Treatment))[1:5]),
                                     sliderInput("Transfer1", 
                                                 label = h4("Transfer Range"), 
                                                 min = min(DAT[DAT$Experiment=="salinity",]$Transfer), 
                                                 max = max(DAT[DAT$Experiment=="salinity",]$Transfer), 
                                                 value = c(min(DAT[DAT$Experiment=="salinity",]$Transfer), 
                                                           max(DAT[DAT$Experiment=="salinity",]$Transfer)), ticks=TRUE)
                ),
             
             conditionalPanel(condition= "input.Experiment == 2",
                                     checkboxGroupInput("Strain2", 
                                                        label = h4("Strain"),
                                                        choices = levels(droplevels(DAT[DAT$Experiment=="temperature",]$Strain)), 
                                                        selected = levels(droplevels(DAT[DAT$Experiment=="temperature",]$Strain))[1:2]),
                                     checkboxGroupInput("Treatment2", 
                                                        label = h4("Treatment"),
                                                        choices = levels(droplevels(DAT[DAT$Experiment=="temperature",]$Treatment)), 
                                                        selected = levels(droplevels(DAT[DAT$Experiment=="temperature",]$Treatment))[1:4]),
                                     sliderInput("Transfer2", 
                                                 label = h4("Transfer Range"), 
                                                 min = min(DAT[DAT$Experiment=="temperature",]$Transfer), 
                                                 max = max(DAT[DAT$Experiment=="temperature",]$Transfer), 
                                                 value = c(min(DAT[DAT$Experiment=="temperature",]$Transfer), 
                                                           max(DAT[DAT$Experiment=="temperature",]$Transfer)), ticks=TRUE)
                ),
             
             conditionalPanel(condition= "input.Experiment == 3",
                              checkboxGroupInput("Strain3", 
                                                 label = h4("Strain"),
                                                 choices = levels(droplevels(DAT[DAT$Experiment=="flask",]$Strain)), 
                                                 selected = levels(droplevels(DAT[DAT$Experiment=="flask",]$Strain))[1:5]),
                              checkboxGroupInput("Treatment3", 
                                                 label = h4("Treatment"),
                                                 choices = levels(droplevels(DAT[DAT$Experiment=="flask",]$Treatment)), 
                                                 selected = levels(droplevels(DAT[DAT$Experiment=="flask",]$Treatment))[1:4]),
                              sliderInput("Transfer3", 
                                          label = h4("Transfer Range"), 
                                          min = min(DAT[DAT$Experiment=="flask",]$Transfer), 
                                          max = max(DAT[DAT$Experiment=="flask",]$Transfer), 
                                          value = c(min(DAT[DAT$Experiment=="flask",]$Transfer), 
                                                    max(DAT[DAT$Experiment=="flask",]$Transfer)), ticks=TRUE)
                )
              ), # end sidePanel 
             
             mainPanel(width = 10,
                      plotOutput('Plot', height = 950)
             ) # end mainPanel
  )) # end first tab

  )) # end shinyUI and fluidPage
