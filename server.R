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
#system("./update_datasets.sh")
system("./copy_datasets.sh")

source("./load_prep_data.R")
ALLDATA <- load_prep_data()

DAT <- ALLDATA[[1]]
WTEMPsplit <- ALLDATA[[2]]
WSALTsplit <- ALLDATA[[3]]

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
    condPan1 <- conditionalPanel(condition= COND_TEXT,
                     checkboxGroupInput("Strain",
                                        label = h4("Strain"),
                                        choices = levels(droplevels(DAT[DAT$Experiment==COND,]$Strain)), 
                                        selected = levels(droplevels(DAT[DAT$Experiment==COND,]$Strain))[1:2]),
                     checkboxGroupInput("Treatment",
                                        label = h4("Treatment"),
                                        choices = levels(droplevels(DAT[DAT$Experiment==COND,]$Treatment)), 
                                        selected = levels(droplevels(DAT[DAT$Experiment==COND,]$Treatment))[1:2]),
                     sliderInput("Transfer", 
                                 label = h4("Transfer Range"), 
                                 min = min(DAT[DAT$Experiment==COND,]$Transfer), 
                                 max = max(DAT[DAT$Experiment==COND,]$Transfer), 
                                 value = c(min(DAT[DAT$Experiment==COND,]$Transfer), 
                                           max(DAT[DAT$Experiment==COND,]$Transfer)), ticks=TRUE) )
    return(condPan1)
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
      #ggtitle(paste(input$Strain, "at", input$Treatment, sep=" ")) +
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
  
  # choose experiment
  whichExperiment2 <- reactive({
    EXPER <- getExpName2()
    if (EXPER=="salinity") {
      DF <- WSALTsplit
    } else {
      DF <- WTEMPsplit
    }
    return(DF)
  })
  
  # create conditional panel
  # for choosing the case within an experiment
  output$whichSelectInput <- renderUI({
    AA <- do.call(rbind, strsplit(names(whichExperiment2()), "\\."))
    Strains <- unique(AA[,1])
    Treatments <- unique(AA[,2])
    DF <- whichExperiment2()
    out <- list(selectInput(inputId = "chooseStrain", label = h4("Strain"),
                       choices = Strains,
                         #names(whichExperiment2()),
                       selected = Strains[1]),
                         #names(whichExperiment2())[1]),
                selectInput(inputId = "chooseTreatment", label = h4("Treatment"),
                            choices = Treatments,
                            selected = Treatments[1]),
                sliderInput("Transfer2", 
                            label = h4("Transfer Range"), 
                            min = min(DF[[1]]$Transfer), 
                            max = max(DF[[1]]$Transfer), 
                            value = c(min(DF[[1]]$Transfer), 
                                      max(DF[[1]]$Transfer)), ticks=TRUE)
    )
  return(out)
  })
  
  
  # choose the case (combination of strain and treatment)
  whichSubset2 <- reactive({
    DF <- whichExperiment2()[[which(names(whichExperiment2()) 
                                    ==  paste(input$chooseStrain, ".", input$chooseTreatment, sep=''))]]
    DF <- DF[which(DF$Transfer >= input$Transfer2[1] & DF$Transfer <= input$Transfer2[2]), ]
    return(DF)
  })

  # plot the subset as RF by Day
  pl2 <- reactive({
    DF2 <- whichSubset2()
    ENV2 <- environment()
    plotANCOVA <- ggplot(data = DF2, environment=ENV2,
                         aes(y=lnRF, x=trDay, color=Rep)) + 
                         geom_point() +
                         facet_grid(. ~ seqRep) +
                         geom_line(aes(y=pred), data = DF2) +
                         theme_bw() +
                         ggtitle(paste(input$chooseStrain, "at", input$chooseTreatment, sep=' '))
    return(plotANCOVA)
  })
  
  output$Plot2 <- renderPlot({
    input$nextCase
    pl2()
  })

####
## end of second tab
  
  output$textAbout <- renderUI({
    HTML("<p>Tool to view and calculate growth rates.</p>")
  })
  
})
