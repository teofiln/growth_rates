library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)

shinyServer(function(input, output, session) {

  # read the experiment data and subset based on input
  # clunky still
  whatToPlot <- reactive({
    data <- read.csv(input$Experiment, header=T, sep=',', quote='"')
    data <- mutate(data, Rep=factor(Replicate), Transfer=as.numeric(Transfer), Treatment=factor(Treatment))
    
    # subset by Strain based on the group check box input
    df <- data[data$Strain==input$Strain[1] |
                     data$Strain==input$Strain[2] |
                     data$Strain==input$Strain[3] |
                     data$Strain==input$Strain[4] |
                     data$Strain==input$Strain[5] ,]
    
    df <- df[df$Treatment==input$Treatment[1] |
               df$Treatment==input$Treatment[2] |
               df$Treatment==input$Treatment[3] |
               df$Treatment==input$Treatment[4] |
               df$Treatment==input$Treatment[5] ,]
  
    # subset by day based on inputs from the Day slider
#    df <- df[which(df$Day >= input$Day[1] & df$Day <= input$Day[2]),]
    
    # subset by transfer (sequential replicate) based on inputs from the Transfer slider
    df <- df[which(df$Transfer >= input$Transfer[1] & df$Transfer <= input$Transfer[2]),]

    return(df) 
  })
  
  # plot the entire data frame as RF by Day
  pl1 <- reactive ({ 
    plGrowCurv <- suppressWarnings(
                  ggplot(data=whatToPlot(),
                         aes(x=Day, y=RF-Rfctrl, ymax=max(RF)*1.05,
                         group=Rep, shape=Rep, 
                         colour=Rep, linetype=Rep)) +
                    geom_line(size=0.5) +
                    geom_point(size=2) +
                    xlab("Day") +  ylab("log10 RFU") +
                    facet_grid(Strain ~ Treatment) +
                    theme_bw() +
                    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                  labels = trans_format("log10", math_format(10^.x))) #+
                  #  coord_fixed() #+
  )
  return(plGrowCurv)
  
  })
  
  output$Plot <- renderPlot({  
    pl1()
  })

  output$textAbout <- renderUI({
    HTML("<p>Tool to view and calculate growth rates.</p>")
  })
  
})