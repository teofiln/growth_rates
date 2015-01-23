library(shiny)
library(ggplot2)

#get a named list of the active experiments 
EXPS <- as.list(dir(pattern="*.csv"))
names(EXPS) <- EXPS  # to be used in selectInput("Experiment"...) bellow

shinyUI(fluidPage(
  
  title="View growth curves and calculate growth rate",
  plotOutput('Plot', height = 800),
  hr(),
  
  fluidRow(
    column(3,
      selectInput("Experiment", 
                  label = h6("Choose experiment", style="color:#0066FF"),
                  choices = EXPS, selected = EXPS[1] ) )
  )
  
))
