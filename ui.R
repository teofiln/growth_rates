library(shiny)
library(ggplot2)
library(shinythemes)

#get a named list of the active experiments 
EXPS <- as.list(dir(pattern="MAIN.csv"))
names(EXPS) <- EXPS  # to be used in selectInput("Experiment"...) bellow
data <- read.csv(as.character(EXPS), header=TRUE)
data <- mutate(data, Rep=factor(Replicate), Transfer=as.numeric(Transfer), Treatment=factor(Treatment))

shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  title="View growth curves and calculate growth rate",
  plotOutput('Plot', height = 750), #width = 1400
  hr(),
  
  fluidRow(
    column(2,
      selectInput("Experiment", 
                  label = h3("Experiment"),
                  choices = EXPS, selected = EXPS[1] ) ),
    column(2,
           checkboxGroupInput("Strain", 
                       label = h3("Strain"),
                       choices = levels(data$Strain), selected = levels(data$Strain)[1:5]) ),
    column(2,
           checkboxGroupInput("Treatment", 
                              label = h3("Treatment"),
                              choices = levels(data$Treatment), selected = levels(data$Treatment)[1:5]) ),
  #  column(3,
  #         sliderInput("Day", label = h3("Day Range"), 
  #                     min = min(data$Day), 
  #                     max = max(data$Day), 
  #                     value = c(min(data$Day), max(data$Day)),
  #                     ticks=TRUE, animate=TRUE)),
   column(3,
          sliderInput("Transfer", label = h3("Transfer Range"), 
                      min = min(data$Transfer), 
                      max = max(data$Transfer), 
                      value = c(min(data$Transfer), max(data$Transfer)),
                      ticks=TRUE, animate=TRUE))
  )

  ))
