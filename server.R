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
WSALT <- rbind(ALLDATA[[13]], ALLDATA[[5]])
WTEMPslopes <- ALLDATA[[6]]
WSALTslopes <- ALLDATA[[7]]
CSALT <- ALLDATA[[8]]
CSALTsplit <- ALLDATA[[9]]
CSALTslopes <- ALLDATA[[10]]
WFLAS <- ALLDATA[[11]]
WFLAS2 <- ALLDATA[[12]]

# get mean, SD
# helper
meanNsd <- function(x) { c(Mean=mean(x$trDay, na.rm=TRUE), SD=sd(x$trDay, na.rm=TRUE)) }

shinyServer(function(input, output, session) {
  # helper to get the name of the experiment
  # for creating the conditional panels
  getExpName1 <- reactive({
    COND <- switch(input$Experiment1,
                   "1" = "salinity",
                   "2" = "temperature",
                   "3" = "cryptica",
                   "4" = "flasks",
                   "5" = "flasks2")
    return(COND)
  })
  
  # choose experiment
  whichExperiment1 <- reactive({
    EXPER <- getExpName1()
    DF <- switch(EXPER,
                 "salinity" = WSALT,
                 "temperature" = WTEMP,
                 "cryptica" = CSALT,
                 "flasks" = WFLAS,
                 "flasks2" = WFLAS2)
    return(DF)
  })

  # create conditional panel
  # for choosing the case within an experiment
  output$whichSelectInput1 <- renderUI({
    DF <- whichExperiment1()
    out <- list(checkboxGroupInput(inputId = "chooseStrain1", label = h4("Strain"),
                            choices = levels(DF$Strain),
                            selected = levels(DF$Strain)[1:5]),
                checkboxGroupInput(inputId = "chooseTreatment1", label = h4("Treatment"),
                            choices = levels(DF$Treatment),
                            selected = levels(DF$Treatment)[1:5]),
                sliderInput(inputId = "Transfer1", label = h4("Transfer Range"), 
                            min = min(DF$Transfer), 
                            max = max(DF$Transfer), 
                            value = c(min(DF$Transfer), 
                                      max(DF$Transfer)), ticks=TRUE),
                sliderInput("aspect_ratio1", 
                            label = h4("Aspect ratio"), 
                            min = 0, 
                            max = 10, 
                            value = 10, 
                            ticks=TRUE)
    )
    return(out)
  })
  
  # subset the data based on choice of
  # strain, treatment and transfer range
  # in conditional panels
  whichSubset1 <- reactive({
    DF <- isolate( whichExperiment1() )
    DF <- DF[which(DF$Strain %in% input$chooseStrain1 & DF$Treatment %in% input$chooseTreatment1), ]
    DF <- DF[which(DF$Transfer >= input$Transfer1[1] & DF$Transfer <= input$Transfer1[2]), ]
    return(DF)
  }) 
  
  # plot the subset as RF by Day
  pl1 <- reactive ({ 
    DF <- whichSubset1()
    ENV <- environment()
    labs <- log10((10^3)*(2^(0:6)))
    plGrowCurv <- ggplot(data=DF, environment = ENV,
                         aes(x=Day, y=RF-Rfctrl, ymax=max(RF)*1.05,
                             group=Rep, shape=Rep, 
                             colour=Rep, linetype=Rep)) +
      geom_line(size=0.6) +
      geom_point(size=3) +
      geom_hline(aes(yintercept=10000), size=0.3, linetype=3, colour="firebrick4") +
      xlab("Day") +  ylab("RFU (log10 scale)") +
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
    pl1() + coord_fixed(ratio=input$aspect_ratio1)
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
                   "2" = "temperature",
                   "3" = "cryptica")
    return(COND)
  })
  
  # choose experiment
  whichExperiment2 <- reactive({
    EXPER <- getExpName2()
    DF <- switch(EXPER,
                 "salinity" = WSALT,
                 "temperature" = WTEMP,
                 "cryptica" = CSALT )
    return(DF)
  })
  
  # create conditional panel
  # for choosing the case within an experiment
  output$whichSelectInput <- renderUI({
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
                            selected = 1),
                sliderInput("aspect_ratio2", 
                            label = h4("Aspect ratio"), 
                            min = 0, 
                            max = 10, 
                            value = 1, 
                            ticks=TRUE)
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
      plot <- pl2.1() + coord_fixed(ratio=input$aspect_ratio2) 
    } else {
      plot <- pl2.2() + coord_fixed(ratio=input$aspect_ratio2)
    }
    return(plot)
  })

  output$Plot2 <- renderPlot({  
    pl2() + coord_fixed(ratio=input$aspect_ratio2)
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
                 "2" = "temperature", 
                 "3" = "cryptica")
  return(COND)
})

# choose experiment
whichExperiment3 <- reactive({
  EXPER <- getExpName3()
  DF <- switch(EXPER,
               "salinity" = WSALTslopes, 
               "temperature" = WTEMPslopes, 
               "cryptica" = CSALTslopes )
  return(DF)
})

# create conditional panel
# for choosing the case within an experiment
output$whichCheckBoxInput3 <- renderUI({
  DF <- whichExperiment3()
  out <- list(checkboxGroupInput(inputId = "chooseStrain3", label = h4("Strain"),
                          choices = levels(DF$Strain),
                          selected = levels(DF$Strain)[1:5]),
              checkboxGroupInput(inputId = "chooseTreatment3", label = h4("Treatment"),
                          choices = levels(DF$Treatment),
                          selected = levels(DF$Treatment)[1:5]),
              sliderInput(inputId = "Transfer3", label = h3("Transfer Range"), 
                          min = min(as.numeric(DF$seqRep)), 
                          max = max(as.numeric(DF$seqRep)), 
                          value = c(min(as.numeric(DF$seqRep)), 
                                    max(as.numeric(DF$seqRep)) )),
              selectInput(inputId = "switchPlot3", label = h4("Facet by:"),
                          choices = list("Strain" = 1, "Treatment" = 2),
                          selected = 1)
  )
  return(out)
})

# choose the case (combination of strain and treatment)
whichSubset3 <- reactive({
  DF <- isolate( whichExperiment3() )
  DF <- DF[which(DF$Strain %in% input$chooseStrain3 & DF$Treatment %in% input$chooseTreatment3), ]
  DF <- DF[which(as.numeric(DF$seqRep) >= input$Transfer3[1] & as.numeric(DF$seqRep) <= input$Transfer3[2]), ]
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
pl3.a1 <- reactive({
  DF3 <- getMean()
  ENV3 <- environment()
  plotMeanSlopes <- ggplot(data = DF3, environment=ENV3, aes(x=Treatment, y=Mean, fill=Treatment)) + 
    geom_bar(stat="identity", data=DF3, width=.8, colour="black") +
    geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2, data = DF3) +
    scale_fill_brewer(palette="Blues") + #, name="Salinity") +
    facet_grid(. ~ Strain) +
    theme_bw() +
    ggtitle("Mean growth rate (+- SD) across Transfers and Replicates at different salinities.") +
    ylab("Mean growth rate (slope)") #+
   # ylim(0,1) 
  return(plotMeanSlopes)
  
})

# plot the coefficient of variation of the growth rate
pl3.a2 <- reactive({
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

# plot the mean slopes (growth rates)
pl3.b1 <- reactive({
  DF3 <- getMean()
  ENV3 <- environment()
  plotMeanSlopes <- ggplot(data = DF3, environment=ENV3, aes(x=Strain, y=Mean, fill=Treatment)) + 
    geom_bar(stat="identity", data=DF3, width=.8, colour="black") +
    geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2, data = DF3) +
    scale_fill_brewer(palette="Blues") + #, name="Salinity") +
    facet_grid(. ~ Treatment) +
    theme_bw() +
    ggtitle("Mean growth rate (+- SD) across Transfers and Replicates at different salinities.") +
    ylab("Mean growth rate (slope)") #+
  # ylim(0,1) 
  return(plotMeanSlopes)
  
})

# plot the coefficient of variation of the growth rate
pl3.b2 <- reactive({
  DF3 <- getMean()
  ENV3 <- environment()
  plotCV <- ggplot(data = DF3, environment=ENV3, aes(x=Strain, y=abs(SD/Mean), fill=Treatment)) + 
    geom_bar(stat="identity", data=DF3, width=.8, colour="black") +
    scale_fill_brewer(palette="Blues") + #, name="Salinity") +
    facet_grid(. ~ Treatment) +
    theme_bw() +
    ggtitle("Coefficient of variation (|SD/Mean|) as a measure of the variability around the mean growth rate across Transfers and Replicates.") +
    ylab("|Coefficient of variation|") #+
  #ylim(0,max(abs(DF3$SD/DF3$Mean))+0.02) 
  return(plotCV)
  
}) 

pl3 <- reactive({
  if (input$switchPlot3 == 1) {
    plot <- grid.arrange(pl3.a1(), pl3.a2(), nrow=2)
  } else {
    plot <- grid.arrange(pl3.b1(), pl3.b2(), nrow=2)
  }
  return(plot)
})

output$Plot3 <- renderPlot({
  pl3()
})

####
## end of third tab

####
## start of Fourth tab

# same as third tab
# except different plots

# helper to get the name of the experiment
# for creating the conditional panels
getExpName4 <- reactive({
  COND <- switch(input$Experiment4,
                 "1" = "salinity",
                 "2" = "temperature", 
                 "3" = "cryptica")
  return(COND)
})

# choose experiment
whichExperiment4 <- reactive({
  EXPER <- getExpName4()
  DF <- switch(EXPER,
               "salinity" = WSALTslopes,
               "temperature" = WTEMPslopes,
               "cryptica" = CSALTslopes )
  return(DF)
})

# create conditional panel
# for choosing the case within an experiment
output$whichCheckBoxInput4 <- renderUI({
  DF <- whichExperiment4()
  out <- list(checkboxGroupInput(inputId = "chooseStrain4", label = h4("Strain"),
                                 choices = levels(DF$Strain),
                                 selected = levels(DF$Strain)[1:5]),
              checkboxGroupInput(inputId = "chooseTreatment4", label = h4("Treatment"),
                                 choices = levels(DF$Treatment),
                                 selected = levels(DF$Treatment)[1:5]),
              sliderInput(inputId = "Transfer4", label = h4("Transfer Range"), 
                          min = min(as.numeric(DF$seqRep)), 
                          max = max(as.numeric(DF$seqRep)), 
                          value = c(min(as.numeric(DF$seqRep)), 
                                    max(as.numeric(DF$seqRep)) ),
                          ticks=TRUE),
              selectInput(inputId = "switchPlot4", label = h4("Facet by:"),
                          choices = list("Strain" = 1, "Treatment" = 2),
                          selected = 1),
              sliderInput("aspect_ratio4", 
                          label = h4("Aspect ratio"), 
                          min = 0, 
                          max = 10, 
                          value = 7, 
                          ticks=TRUE)
  )
  return(out)
})

# choose the case (combination of strain and treatment)
whichSubset4 <- reactive({
  DF <- isolate( whichExperiment4() )
  DF <- DF[which(DF$Strain %in% input$chooseStrain4 & DF$Treatment %in% input$chooseTreatment4), ]
  DF <- DF[which(as.numeric(DF$seqRep) >= input$Transfer4[1] & as.numeric(DF$seqRep) <= input$Transfer4[2]), ]
  return(DF)
}) 

getMean4 <- reactive({
  DF4 <- whichSubset4()
  meanSlopes <- ddply(.data=DF4,
                      .variables=.(Strain, Treatment, seqRep),
                      .fun=meanNsd)
  return(meanSlopes)
})

# plot the mean slopes (growth rates)
pl4.1 <- reactive({
  DF4 <- getMean4()
  Cols <- brewer.pal(9, "Blues")[c(4:7,9)]
  ENV4 <- environment()
    plotMeanSlopesThruTime <- ggplot(data = DF4, environment=ENV4, 
                                   aes(x=seqRep, y=Mean, colour=Treatment, group=Treatment, shape=Treatment)) + 
    geom_line(size=0.5, linetype=3) +
    geom_point(size=4) +
    geom_errorbar(aes(ymin=Mean-(1.96 * SD/sqrt(3)), ymax=Mean+(1.96 * SD/sqrt(3))), width=.02, data = DF4) +
    #scale_colour_brewer(palette="PuBuGn") + #, name="Salinity") +
    scale_colour_manual(values=Cols) + #, name="Salinity") +
    facet_wrap(~ Strain, nrow = 2) +
    geom_hline(aes(yintercept=0), size=0.3, linetype=3, colour="firebrick4") +
    theme_bw() +
    ggtitle("Mean growth rate (+- 95% confidence interval, n=3) from technical Replicates over the course of the experiment.") +
    ylab("Mean growth rate (slope)") +
    xlab("Transfer (sequential replicate)")
  return(plotMeanSlopesThruTime)
  
})

# plot the mean slopes (growth rates)
pl4.2 <- reactive({
  DF4 <- getMean4()
  Cols <- brewer.pal(9, "Blues")[c(4:7,9)]
  ENV4 <- environment()
  plotMeanSlopesThruTime <- ggplot(data = DF4, environment=ENV4, 
                                   aes(x=seqRep, y=Mean, colour=Strain, group=Strain, shape=Strain)) + 
    geom_line(size=0.5, linetype=3) +
    geom_point(size=4) +
    geom_errorbar(aes(ymin=Mean-(1.96 * SD/sqrt(3)), ymax=Mean+(1.96 * SD/sqrt(3))), width=.02, data = DF4) +
    #scale_colour_brewer(palette="PuBuGn") + #, name="Salinity") +
    scale_colour_manual(values=Cols) + #, name="Salinity") +
    facet_wrap(~ Treatment, nrow = 2) +
    geom_hline(aes(yintercept=0), size=0.3, linetype=3, colour="firebrick4") +
    theme_bw() +
    ggtitle("Mean growth rate (+- 95% confidence interval, n=3) from technical Replicates over the course of the experiment.") +
    ylab("Mean growth rate (slope)") +
    xlab("Transfer (sequential replicate)")
  return(plotMeanSlopesThruTime)
  
})

pl4 <- reactive({
  if (input$switchPlot4 == 1) {
    plot <- pl4.1() + coord_fixed(ratio=input$aspect_ratio4) 
  } else {
    plot <- pl4.2() + coord_fixed(ratio=input$aspect_ratio4)
  }
  return(plot)
})

output$Plot4 <- renderPlot({
  pl4() + coord_fixed(ratio=input$aspect_ratio4)
})

####
## end of third tab

  output$textAbout <- renderUI({
    HTML("<p>Tool to view and calculate growth rates.</p>")
  })
  
})
