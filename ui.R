library(shiny)
library(ggplot2)
library(shinythemes)

#get a named list of the active experiments 
EXPS <- as.list(dir(pattern="MAIN.csv"))
names(EXPS) <- EXPS  # to be used in selectInput("Experiment"...) bellow
dataset <- read.csv(as.character(EXPS), header=TRUE)

shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  title="View growth curves and calculate growth rate",
  plotOutput('Plot', height = 750), #width = 1400
  hr(),
  
  fluidRow(
    column(3,
      selectInput("Experiment", 
                  label = h3("Choose experiment"),
                  choices = EXPS, selected = EXPS[1] ) ),
    column(3,
           checkboxGroupInput("Strain", 
                       label = h3("Choose Strain"),
                       choices = levels(dataset$Strain), selected = levels(dataset$Strain)[1]) ),
   # column(3,
    #       checkboxGroupInput("Treatment", 
     #                         label = h3("Choose Treatment"),
      #                        choices = levels(dataset$Treatment), selected = levels(dataset$Treatment)[1]) ),
    column(3,
           sliderInput("Day", label = h3("Day Range"), 
                       min = min(dataset$Day), 
                       max = max(dataset$Day), 
                       value = c(30, 45),
                       ticks=TRUE, animate=TRUE))
  )

  ))
