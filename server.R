library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)
library(HH)
library(shinythemes)

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

# prepare the data
DAT <- rbind(Wfami, Wsalt, Wtemp, Wflas, Csalt)
DAT <- mutate(.data=DAT, 
              Rep=factor(Replicate),
              seqRep=factor(Transfer),
              Transfer=as.numeric(Transfer), 
              Treatment=factor(Treatment),
              Temperature=factor(Temperature),
              Media=factor(Media),
              Experiment=factor(Experiment),
              Strain=factor(Strain),
              lnRF=log(RF-Rfctrl),
              trDay=(Hour/24)+1)
              

shinyServer(function(input, output, session) {
  
  # helper to get the name of the experiment
  # for creating the conditional panels
  getExpName <- reactive({
    COND <- switch(input$Experiment,
                 "1" = "salinity",
                 "2" = "temperature",
                 "3" = "flask")
    return(COND)
  })
  
  # helper to create a string for the 
  # condition in the conditional panels
  makeCondition <- reactive({
    COND_TEXT <- paste("input.Experiment == ", input$Experiment, sep="")
    return(COND_TEXT)
  })
  
  # function to create the conditional panels
  # based on the choice of experiment
  # shortens the code in ui.R
  output$whichCondPanel <- renderUI({ 
    COND <- getExpName()
    COND_TEXT <- makeCondition()
    conditionalPanel(condition= COND_TEXT,
                     checkboxGroupInput("Strain", 
                                        label = h4("Strain"),
                                        choices = levels(droplevels(DAT[DAT$Experiment==COND,]$Strain)), 
                                        selected = levels(droplevels(DAT[DAT$Experiment==COND,]$Strain))[1:4]),
                     checkboxGroupInput("Treatment", 
                                        label = h4("Treatment"),
                                        choices = levels(droplevels(DAT[DAT$Experiment==COND,]$Treatment)), 
                                        selected = levels(droplevels(DAT[DAT$Experiment==COND,]$Treatment))[1:4]),
                     sliderInput("Transfer", 
                                 label = h4("Transfer Range"), 
                                 min = min(DAT[DAT$Experiment==COND,]$Transfer), 
                                 max = max(DAT[DAT$Experiment==COND,]$Transfer), 
                                 value = c(min(DAT[DAT$Experiment==COND,]$Transfer), 
                                           max(DAT[DAT$Experiment==COND,]$Transfer)), ticks=TRUE)
    )
  })

  # subset the data
  # based on choice of experiment
  whichExperiment <- reactive({
    EXPER <- getExpName()
    # subset by Experiment based on select input
    DF <- droplevels.data.frame(DAT[which(DAT$Experiment==EXPER), ])
    return(DF)
  })
  
  # subset the data based on choice of
  # strain, treatment and transfer range
  # in conditional panels
  whichSubset <- reactive({
    DF <- whichExperiment()
    DF <- DF[which(DF$Strain %in% input$Strain & DF$Treatment %in% input$Treatment
                   & DF$Transfer >= input$Transfer[1] & DF$Transfer <= input$Transfer[2]), ]
    return(DF) 
  })
  
  # plot the subset as RF by Day
  pl1 <- reactive ({ 
    DF <- whichSubset()
    ENV <- environment()
    labs <- log10((10^3)*(2^(0:6)))
    plGrowCurv <- ggplot(data=DF, environment = ENV,
                         aes(x=Day, y=RF-Rfctrl, ymax=max(RF)*1.05,
                             group=Rep, shape=Rep, 
                             colour=Rep, linetype=Rep)) +
      geom_line(size=0.6) +
      geom_point(size=3) +
      geom_hline(aes(yintercept=10000), size=0.3, linetype=3, colour="firebrick4") +
      xlab("Day") +  ylab("log10 RFU") +
      facet_grid(Strain ~ Treatment) +
      theme_bw() +
      #coord_trans(y="log10")
      scale_y_continuous(trans=log10_trans(), 
                         breaks = trans_breaks("log10", function(x) 10^x, n=3),
                         labels = trans_format("log10", math_format(10^.x))) 
    return(plGrowCurv)
  })
  
  output$Plot1 <- renderPlot({  
    pl1()
  })
    
  ####
  ## end of first tab
  
  ####
  ## start of second tab
  
  # helper to get the name of the experiment
  # for creating the conditional panels
  getExpName2 <- reactive({
    COND <- switch(input$Experiment2,
                   "1" = "salinity",
                   "2" = "temperature")
    return(COND)
  })
  
  # helper to create a string for the 
  # condition in the conditional panels
  makeCondition2 <- reactive({
    COND_TEXT <- paste("input.Experiment2 == ", input$Experiment2, sep="")
    return(COND_TEXT)
  })
  
  output$whichCondPanel2 <- renderUI({ 
    COND <- getExpName2()
    COND_TEXT <- makeCondition2()
    conditionalPanel(condition= COND_TEXT,
                     checkboxGroupInput("Strain2", 
                                        label = h4("Strain"),
                                        choices = levels(droplevels(DAT[DAT$Experiment==COND,]$Strain)), 
                                        selected = levels(droplevels(DAT[DAT$Experiment==COND,]$Strain))[1]),
                     checkboxGroupInput("Treatment2", 
                                        label = h4("Treatment"),
                                        choices = levels(droplevels(DAT[DAT$Experiment==COND,]$Treatment)), 
                                        selected = levels(droplevels(DAT[DAT$Experiment==COND,]$Treatment))[1]),
                     sliderInput("Transfer2", 
                                 label = h4("Transfer Range"), 
                                 min = min(DAT[DAT$Experiment==COND,]$Transfer), 
                                 max = max(DAT[DAT$Experiment==COND,]$Transfer), 
                                 value = c(max(DAT[DAT$Experiment==COND,]$Transfer)-4, 
                                           max(DAT[DAT$Experiment==COND,]$Transfer)), ticks=TRUE)
    )
  })
  
  # subset the data
  # based on choice of experiment
  whichExperiment2 <- reactive({
    EXPER <- getExpName2()
    DF <- droplevels.data.frame(DAT[which(DAT$Experiment==EXPER), ])
    return(DF)
  })
  
  # subset the data based on choice of
  # strain, treatment and transfer range
  # in conditional panels
  whichSubset2 <- reactive({
    DF <- whichExperiment2()
    DF <- DF[which(DF$Strain %in% input$Strain2 & DF$Treatment %in% input$Treatment2
                   & DF$Transfer >= input$Transfer2[1] & DF$Transfer <= input$Transfer2[2]), ]
    return(droplevels.data.frame(DF))
  })
  
  # fit ancova based on choice of
  # strain, treatment and range of transfers
  ANCOV <- reactive({
    DF <- whichSubset2()
    mod <- aov(data = DF, 
               formula = lnRF ~ trDay*seqRep*Rep, 
               na.action=na.omit)
    pred <- predict(mod)
    return(pred)
  })
  
  # plot the subset as RF by Day
  pl2 <- reactive ({
    DF <- cbind(whichSubset2(), ANCOV())
    ENV <- environment()
    plotANCOVA <- ggplot(data = DF, environment=ENV,
                         aes(y=lnRF, x=trDay, color=Rep)) + 
                         geom_point() +
                         facet_grid(. ~ seqRep) +
                         geom_line(aes(y=pred), data = DF) +
                         theme_bw()
    return(plotANCOVA)
  })
  
  output$Plot2 <- renderPlot({  
    pl2()
  })


####
## end of second tab
  
  output$textAbout <- renderUI({
    HTML("<p>Tool to view and calculate growth rates.</p>")
  })
  
})