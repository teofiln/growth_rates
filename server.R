library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)
library(HH)

# update the datasets 
# creates symbolic links from original files in different folder
# will work only localy
system("./update_datasets.sh")

# read all experiments: waller_salinity, waller_temperature, waller_flasks, cryptica_salinity
Wsalt <- read.csv("./WSALT.csv", header=TRUE)
Wtemp <- read.csv("./WTEMP.csv", header=TRUE)
Wflas <- read.csv("./WFLAS.csv", header=TRUE)
#Csalt <- read.csv("./CSALT.csv", header=TRUE)

DAT <- rbind(Wsalt, Wtemp, Wflas)
DAT <- mutate(.data=DAT, Rep=factor(Replicate), 
              Transfer=as.numeric(Transfer), 
              Treatment=factor(Treatment),
              Temperature=factor(Temperature),
              Media=factor(Media),
              Experiment=factor(Experiment))

shinyServer(function(input, output, session) {
  
  # read the experiment data and subset based on input
  # clunky still
  whichExperiment <- reactive({
    # subset by Experiment based on select input
    DF <- switch(input$Experiment,
                  "1" = droplevels.data.frame(DAT[which(DAT$Experiment=="salinity"), ]),
                  "2" = droplevels.data.frame(DAT[which(DAT$Experiment=="temperature"), ]),
                  "3" = droplevels.data.frame(DAT[which(DAT$Experiment=="flask"), ])
    )
    return(DF)
  })
  
  whichSubset <- reactive({
    DF <- whichExperiment()
    DF <- switch(input$Experiment,
                  "1" = DF[which(DF$Strain %in% input$Strain1 & DF$Treatment %in% input$Treatment1
                                   & DF$Transfer >= input$Transfer1[1] & DF$Transfer <= input$Transfer1[2]), ],
                  "2" = DF[which(DF$Strain %in% input$Strain2 & DF$Treatment %in% input$Treatment2
                                   & DF$Transfer >= input$Transfer2[1] & DF$Transfer <= input$Transfer2[2]), ],
                  "3" = DF[which(DF$Strain %in% input$Strain3 & DF$Treatment %in% input$Treatment3
                                   & DF$Transfer >= input$Transfer3[1] & DF$Transfer <= input$Transfer3[2]), ]
                  )
    return(DF) 
  })
  
  # plot the entire data frame as RF by Day
  pl1 <- reactive ({ 
    DF <- whichSubset()
    ENV <- environment()
    plGrowCurv <- ggplot(data=DF, environment = ENV,
                         aes(x=Day, y=RF-Rfctrl, ymax=max(RF)*1.05,
                         group=Rep, shape=Rep, 
                         colour=Rep, linetype=Rep)) +
                    geom_line(size=0.5) +
                    geom_point(size=2) +
                    xlab("Day") +  ylab("log10 RFU") +
                    facet_grid(Strain ~ Treatment) +
                    theme_bw() +
                    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                  labels = trans_format("log10", math_format(10^.x))) 
  return(plGrowCurv)
  })
  
  output$Plot <- renderPlot({  
    pl1()
  })

  output$textAbout <- renderUI({
    HTML("<p>Tool to view and calculate growth rates.</p>")
  })
  
})