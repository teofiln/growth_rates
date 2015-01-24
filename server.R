library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)

shinyServer(function(input, output, session) {
  
  # read the experiment data sheet and make data frame
  which.experiment <- reactive({
    data <- read.csv(input$Experiment, header=T, sep=',', quote='"')
    df <- data.frame(data)
    return(df) 
  })
    
  # plot the entire data frame as RF by Day
  pl1 <- reactive ({
    DAT <- mutate(which.experiment(),
                  Rep=as.factor(Replicate))
    
    plGrowCurv <- ggplot(data=DAT,
                         aes(x=Day, y=RF-Rfctrl, ymax=max(RF)*1.05,
                         group=Rep, shape=Rep, 
                         colour=Rep, linetype=Rep)) +
      geom_line(size=0.3) +
      geom_point(size=1) + #position=position_dodge(width=0.5, height=0)) +
      #   scale_x_discrete(breaks=MIN:MAX, labels=MIN:MAX) +
      xlab("Day") +  ylab("log10 RFU") +
      xlim(min(DAT$Day), max(DAT$Day)) +
      facet_grid(Strain ~ Treatment) +
      theme_bw() +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) #+
    #  coord_fixed() #+
      #theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  
  return(plGrowCurv)
  })
  
  output$Plot <- renderPlot({  
    pl1()
  })

  output$textAbout <- renderUI({
    HTML("<p>Tool to view and calculate growth rates.</p>")
  })
  
})