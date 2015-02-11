library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)
library(HH)
library(nlme)
library(car)
library(shinythemes)

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
WTEMPslopes <- ALLDATA[[6]]
WSALTslopes <- ALLDATA[[7]]

# get mean, SD
# helper
meanNsd <- function(x) { c(Mean=mean(x$trDay, na.rm=TRUE), SD=sd(x$trDay, na.rm=TRUE)) }

shinyServer(function(input, output, session) {
  
  # helper to get the name of the experiment
  # for creating the conditional panels
  getExpName <- reactive({
    COND <- switch(input$Experiment,
                 "1" = "salinity",
                 "2" = "temperature",
                 "3" = "flask",
                 "4" = "flask2")
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
                                           max(DAT[DAT$Experiment==COND,]$Transfer)), ticks=TRUE),
                     sliderInput("aspect_ratio", 
                                 label = h4("Aspect ratio"), 
                                 min = 0, 
                                 max = 10, 
                                 value = 1, 
                                 ticks=TRUE)
                     )
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
    pl1() + coord_fixed(ratio=input$aspect_ratio)
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
      DF <- WSALT
    } else {
      DF <- WTEMP
    }
    return(DF)
  })
  
  # create conditional panel
  # for choosing the case within an experiment
  output$whichSelectInput <- renderUI({
#     AA <- do.call(rbind, strsplit(names(whichExperiment2()), "\\."))
#     Strains <- unique(AA[,1])
#     Treatments <- unique(AA[,2])
    DF <- whichExperiment2()
    out <- list(selectInput(inputId = "chooseStrain", label = h4("Strain"),
                            choices = levels(DF$Strain),
                            selected = levels(DF$Strain)[1]),
                selectInput(inputId = "chooseTreatment", label = h4("Treatment"),
                            choices = levels(DF$Treatment),
                            selected = levels(DF$Treatment)[1]),
                sliderInput(inputId = "Transfer2", label = h4("Transfer Range"), 
                            min = min(DF$Transfer), 
                            max = max(DF$Transfer), 
                            value = c(min(DF$Transfer), 
                                      max(DF$Transfer)), ticks=TRUE),
                selectInput(inputId = "switchPlot", label = h4("Faceted or superimposed"),
                            choices = list("Faceted" = 1, "Superimposed" = 2),
                            selected = 1)
    )
  return(out)
  })
  
  
  # choose the case (combination of strain and treatment)
  whichSubset2 <- reactive({
    DF <- whichExperiment2()
    DF <- DF[which(DF$Strain == input$chooseStrain & DF$Treatment == input$chooseTreatment), ]
    DF <- DF[which(DF$Transfer >= input$Transfer2[1] & DF$Transfer <= input$Transfer2[2]), ]
    DF <- DF[- which(DF$Hour == 0), ]
    return(DF)
  })
  
  # reactive ancova 
  runAOV <- reactive({
    DF <- whichSubset2()
    MOD <- lm(lnRF ~ trDay*seqRep*Rep, data = DF, na.action = na.exclude)
    PRED <- predict(MOD, se=TRUE)
    UCL <- PRED$fit + 1.96 * PRED$se.fit
    LCL <- PRED$fit - 1.96 * PRED$se.fit
    USE <- PRED$fit + PRED$se.fit
    LSE <- PRED$fit - PRED$se.fit
    res3 <- cbind(DF, PRED=PRED$fit, UCL, LCL, USE, LSE)
    result <- list(MOD, res3)
    return(result)
  })
  
  
  # plot the subset as RF by Day
  pl2.1 <- reactive({
    DF2 <- runAOV()[[2]]
    ENV2 <- environment()
    plotANCOVA <- ggplot(data = DF2, environment=ENV2,
                         aes(y=lnRF, x=trDay, color=Rep)) + 
                         geom_point() +
                         facet_grid(. ~ seqRep) +
                         geom_smooth(aes(y=PRED),#, ymin=LSE, ymax=USE, fill=Rep),
                                     data = DF2, stat="identity", se=FALSE) +
                         theme_bw() +
                         ggtitle(paste("Slopes of", input$chooseStrain, "at", input$chooseTreatment,
                                       "ppt. Shaded region corresponds to predicted value +- standard error.",
                                       sep=' '))
    return(plotANCOVA)
  })
  
  pl2.2 <- reactive({
    DF2 <- mutate(runAOV()[[2]], RepSeqRep = paste(Rep, seqRep, sep=""))
    ENV2 <- environment()
    plotANCOVA <- ggplot(data = DF2, environment=ENV2,
                         aes(y=lnRF, x=trDay, 
                             color=seqRep, 
                             shape=Rep, 
                             group=RepSeqRep
                             )) +
      geom_smooth(aes(y=PRED, linetype=Rep),#, ymin=LSE, ymax=USE, fill=seqRep), 
                  data = DF2, method="lm", stat="identity", fullrange=TRUE, se=FALSE) +
      #geom_smooth(method="lm", data=DF2, aes(linetype=Rep, ), se=TRUE, fullrange=TRUE) +
      geom_point() +
      #facet_grid(. ~ seqRep) +
      #geom_line(aes(y=pred, linetype=Rep), data = DF2) +
      theme_bw() +
      ggtitle(paste("Slopes of", input$chooseStrain, "at", input$chooseTreatment,
                    "ppt. Shaded region corresponds to predicted value +- standard error.",
                    sep=' '))
      return(plotANCOVA)
  })

  pl2 <- reactive({
    if (input$switchPlot == 1) {
      plot <- pl2.1()
    } else {
      plot <- pl2.2()
    }
    return(plot)
  })

  # render an analysis of variance table
  tb1 <- reactive({
    Model <- runAOV()[[1]]
    Table <- Anova(Model, type = "III")
    explanation <- c("Intercept of regression of log RF against Day",
                     "Slope of regression of log RF against Day",
                     "Comparison of intercepts across Transfers",
                     "Comparison of intercepts across Replicates",
                     "Comparison of slopes across Transfers",
                     "Comparison of slopes across Replicates",
                     "Comparison of intercepts of the interaction Transfer-by-Replicate",
                     "Comparison of slopes of the interaction Transfer-by-Replicate",
                     "Residuals")
    Table <- cbind(Table, explanation)
    return(Table)
  })

  output$Plot2 <- renderPlot({
    pl2()
  })

#   output$Plot2.1 <- renderPlot({
#     pl2.1()
#   })
# 
#   output$Plot2.2 <- renderPlot({
#     pl2.2()
#   })

  output$TableAOV <- renderTable({
    tb1()#[c(1,4,5,7,8),]
  })

####
## end of second tab

####
## start of third tab

# the logic is the same as the second tab
# with checkBox inputs instead of selectInputs
# and a different output

# helper to get the name of the experiment
# for creating the conditional panels
getExpName3 <- reactive({
  COND <- switch(input$Experiment3,
                 "1" = "salinity",
                 "2" = "temperature")
  return(COND)
})

# choose experiment
whichExperiment3 <- reactive({
  EXPER <- getExpName3()
  if (EXPER=="salinity") {
    DF <- WSALTslopes
  } else {
    DF <- WTEMPslopes
  }
  return(DF)
})

# create conditional panel
# for choosing the case within an experiment
output$whichCheckBoxInput3 <- renderUI({
  #     AA <- do.call(rbind, strsplit(names(whichExperiment2()), "\\."))
  #     Strains <- unique(AA[,1])
  #     Treatments <- unique(AA[,2])
  DF <- whichExperiment3()
  out <- list(checkboxGroupInput(inputId = "chooseStrain3", label = h4("Strain"),
                          choices = levels(DF$Strain),
                          selected = levels(DF$Strain)[1:5]),
              checkboxGroupInput(inputId = "chooseTreatment3", label = h4("Treatment"),
                          choices = levels(DF$Treatment),
                          selected = levels(DF$Treatment)[1:5])#,
#                sliderInput(inputId = "Transfer3", label = h4("Transfer Range"), 
#                            min = min(DF$Transfer), 
#                            max = max(DF$Transfer), 
#                            value = c(min(DF$Transfer), 
#                                      max(DF$Transfer)), ticks=TRUE)
  )
  return(out)
})

# choose the case (combination of strain and treatment)
whichSubset3 <- reactive({
  DF <- isolate( whichExperiment3() )
  DF <- DF[which(DF$Strain %in% input$chooseStrain3 & DF$Treatment %in% input$chooseTreatment3), ]
 # DF <- DF[which(DF$Transfer >= input$Transfer3[1] & DF$Transfer <= input$Transfer3[2]), ]
  return(DF)
}) 

getMean <- reactive({
  DF3 <- whichSubset3()
  meanSlopes <- ddply(.data=DF3,
                      .variables=.(Strain, Treatment),
                      .fun=meanNsd)
  return(meanSlopes)
})

# plot the mean slopes (growth rates)
pl3.1 <- reactive({
  DF3 <- getMean()
  ENV3 <- environment()
  plotMeanSlopes <- ggplot(data = DF3, environment=ENV3, aes(x=Treatment, y=Mean, fill=Treatment)) + 
    geom_bar(stat="identity", data=DF3, width=.8, colour="black") +
    geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2, data = DF3) +
    scale_fill_brewer(palette="Blues") + #, name="Salinity") +
    facet_grid(. ~ Strain) +
    theme_bw() +
    ggtitle("Mean growth rate across Transfer and Replicates over the last 3 Transfers at different salinities. Error bars are mean +- 1 standard deviation.") +
    ylab("Mean growth rate (slope)") #+
    #ylim(min(DF3$Mean)-0.1,1) 
  return(plotMeanSlopes)
  
})

# plot the coefficient of variation of the growth rate
pl3.2 <- reactive({
  DF3 <- getMean()
  ENV3 <- environment()
  plotCV <- ggplot(data = DF3, environment=ENV3, aes(x=Treatment, y=abs(SD/Mean), fill=Treatment)) + 
    geom_bar(stat="identity", data=DF3, width=.8, colour="black") +
    scale_fill_brewer(palette="Blues") + #, name="Salinity") +
    facet_grid(. ~ Strain) +
    theme_bw() +
    ggtitle("Coefficient of variation (|SD/Mean|) as a measure of the variability around the mean growth rate across Transfers and Replicates.") +
    ylab("|Coefficient of variation|") #+
    #ylim(0,max(abs(DF3$SD/DF3$Mean))+0.02) 
  return(plotCV)
  
}) 

output$Plot3.1 <- renderPlot({
  pl3.1()
})

output$Plot3.2 <- renderPlot({
  pl3.2()
})

####
## end of third tab

  output$textAbout <- renderUI({
    HTML("<p>Tool to view and calculate growth rates.</p>")
  })
  
})
