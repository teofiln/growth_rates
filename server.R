library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)
library(HH)
library(nlme)
library(car)
library(shinythemes)
library(RColorBrewer)

# update the datasets 
# creates symbolic links from original files in different folder
# will work only localy
#system("./update_datasets.sh")
#system("./copy_datasets.sh")

# helper to get mean, SD
meanNsd <- function(x) { c(Mean=mean(x$trDay, na.rm=TRUE), SD=sd(x$trDay, na.rm=TRUE)) }

shinyServer(function(input, output, session) {

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
  CFLAS <- ALLDATA[[14]]
  
  ##############################################
  ##     start browse data tab                ##
  ##############################################
  
  # assign dataset to plot
  # based on selected experiment
  whichExperiment0 <- reactive({
    DF <- switch(input$Experiment0,
                 "1" = WSALT,
                 "2" = WTEMP,
                 "3" = CSALT,
                 "4" = WFLAS,
                 "5" = WFLAS2,
                 "6" = CFLAS)
    return(DF)
  })

  # create a spreadsheet for transfer
  createTransfer <- reactive({
    input$calculateTransferVolumes
   # isolate(
      DF <- whichExperiment0()
      Today <- DF[DF$Day==max(DF$Day), ]
      
      VOL <- input$finalVolume
      RFU <- input$desiredRFU
      
      out <- mutate(.data = Today,
                    VolCul = round((RFU * VOL)/(Today$RF-Today$Rfctrl), 2),
                    VolMed = round(VOL - VolCul, 2))
   # )
    return(out[,c(7,10,14:15,12:13,18:19)])
  })
  
  # download the spreadsheet
  output$downloadTransferSheet <- downloadHandler(
    filename =  function() {
      paste("Transfer_", Sys.Date(), '.csv', sep='')
    },
    content = function(file) { 
      write.csv(createTransfer(), file)
    },
    contentType='text/csv'
  )
  
  # render all the data
  output$Table0a <- renderDataTable({
    whichExperiment0()[,1:13]
  }, options = list(pageLength = 10) )
  
  # prepare and render the new data table
  output$Table0b <- renderDataTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    out <- read.csv(inFile$datapath, header = input$header, skip=input$skip_lines,
                    sep = input$sep, quote = input$quote)
    colnames(out) <- c("Sample", "Fluorescence", "Unit")
    blanks <- tail(out$Fluorescence, input$numTreat)
    out <- out[1:(nrow(out) - input$numTreat), ]
    out$Blank <- rep(blanks, length.out=nrow(out), each=input$numStrain)
    
    out <- out[,c(1,2,4,3)]
    return(out)
    
  }, options = list(pageLength = 10) )
  
  output$Table0c <- renderDataTable({
    createTransfer()
  }, options = list(pageLength = 10) )
  
  ##############################################
  ##     end browse data tab                  ##
  ##############################################
  
  ##############################################
  ##     start growth curves tab              ##
  ##############################################
  
  # assign dataset to plot
  # based on selected experiment
  whichExperiment1 <- reactive({
    DF <- switch(input$Experiment1,
                 "1" = WSALT,
                 "2" = WTEMP,
                 "3" = CSALT,
                 "4" = WFLAS,
                 "5" = WFLAS2,
                 "6" = CFLAS)
    return(DF)
  })

  # create conditional panel
  # for subseting an experiment
  output$whichSelectInput1 <- renderUI({
    DF <- whichExperiment1()
    out <-  div(id = "PlotControls", class = "collapse", 
                wellPanel(
                checkboxGroupInput(inputId = "chooseStrain1", label = h5("Strain"), inline = TRUE,
                            choices = levels(DF$Strain),
                            selected = levels(DF$Strain)[1:5]),
                checkboxGroupInput(inputId = "chooseTreatment1", label = h5("Treatment"), inline = TRUE,
                            choices = levels(DF$Treatment),
                            selected = levels(DF$Treatment)[1:5]),
                checkboxGroupInput(inputId = "chooseReplicate1", label = h5("Replicate"), inline = TRUE,
                                   choices = levels(DF$Rep),
                                   selected = levels(DF$Rep)[1:3]),
                sliderInput(inputId = "Transfer1", label = h5("Transfer Range"), 
                            min = min(DF$Transfer), 
                            max = max(DF$Transfer), 
                            value = c(max(DF$Transfer)-4, 
                                      max(DF$Transfer)), ticks=TRUE),
                sliderInput("aspect_ratio1",
                            label = h5("Aspect ratio (semi-log plot only)"), 
                            min = 1, 
                            max = 10, 
                            value = 5, 
                            ticks=TRUE,
                            step=0.5),
                checkboxInput("logScale", 
                              label=h5("Toggle log scale"),
                              value=TRUE) 
                ) # end wellPanel
    ) # end collapsable div
    return(out)
  })
  
  # subset the data based on choice of
  # strain, treatment and transfer range
  # in conditional panels
  whichSubset1 <- reactive({
    DF <- isolate( whichExperiment1() )
    DF <- DF[which(DF$Strain %in% input$chooseStrain1 & 
                     DF$Treatment %in% input$chooseTreatment1 & 
                     DF$Rep %in% input$chooseReplicate1), ]
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
      xlab("Day") +
      facet_grid(Strain ~ Treatment) +
      theme_bw()
    return(plGrowCurv)
  })
  
  # render with control for aspect ratio
  # and logic for log scale
  output$Plot1 <- renderPlot({
    if (input$logScale==TRUE) {
    pl1() + coord_fixed(ratio=input$aspect_ratio1) + 
      ylab("RFU (log10 scale)") +
      scale_y_continuous(trans=log10_trans(),
                         breaks = trans_breaks("log10", function(x) 10^x, n=3),
                         labels = trans_format("log10", math_format(10^.x))) 
    } else {
      pl1() + coord_fixed(ratio=0.001) + ylab("RFU")
    }
  })

  ##############################################
  ##          end growth curves tab           ##
  ##############################################
    
  ##############################################
  ##          start compare slopes tab        ##
  ##############################################
    
  # assign dataset to plot
  # based on selected experiment
  whichExperiment2 <- reactive({
    DF <- switch(input$Experiment2,
                 "1" = WSALT,
                 "2" = WTEMP,
                 "3" = CSALT )
    return(DF)
  })
  
  # create conditional panel
  # for subseting an experiment
  output$whichSelectInput <- renderUI({
    DF <- whichExperiment2()
    out <- div(id = "PlotControls2", class = "collapse", 
               wellPanel(
                selectInput(inputId = "chooseStrain", label = h5("Strain"),
                            choices = levels(DF$Strain),
                            selected = levels(DF$Strain)[1]),
                selectInput(inputId = "chooseTreatment", label = h5("Treatment"),
                            choices = levels(DF$Treatment),
                            selected = levels(DF$Treatment)[1]),
                sliderInput(inputId = "Transfer2", label = h5("Transfer Range"), 
                            min = min(DF$Transfer), 
                            max = max(DF$Transfer), 
                            value = c(max(DF$Transfer)-5, 
                                      max(DF$Transfer)), ticks=TRUE),
                selectInput(inputId = "switchPlot", label = h5("Toggle plot"),
                            choices = list("Faceted" = 1, "Superimposed" = 2),
                            selected = 1),
                sliderInput("aspect_ratio2",
                            label = h5("Aspect ratio"),
                            min=1,
                            max=10,
                            value = 6,
                            #step=0.5,
                            ticks=TRUE),
                checkboxInput("removeDay0", 
                              label = h5("Remove Day 0?"),
                              value = FALSE)
               ) # end wellPanel
    ) # end collapsable div
  return(out)
  })
  
  # choose the subject (combination of strain and treatment)
  whichSubset2 <- reactive({
    DF <- whichExperiment2()
    DF <- DF[which(DF$Strain == input$chooseStrain & DF$Treatment == input$chooseTreatment), ]
    DF <- DF[which(DF$Transfer >= input$Transfer2[1] & DF$Transfer <= input$Transfer2[2]), ]
 
    if (input$removeDay0 == TRUE) {
      DF <- DF[- which(DF$Hour == 0), ]
    } 
    
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
  
  # plot model fit
  # facet by transfer
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
                         ggtitle(paste("Slopes of", input$chooseStrain, 
                                       "at", input$chooseTreatment,
                                       "ppt. Shaded region corresponds to predicted value +- standard error.",
                                       sep=' '))
    return(plotANCOVA)
  })
  
  # plot model fit 
  # superimposed
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
      geom_point() +
      theme_bw() +
      ggtitle(paste("Slopes and confidence region of ", 
                    input$chooseStrain, "at", 
                    input$chooseTreatment, "ppt.",  sep=' '))
      return(plotANCOVA)
  })

  # logic for plot output
  # facet or superimposed
  pl2 <- reactive({
    if (input$switchPlot == 1) {
      plot <- pl2.1() + coord_fixed(ratio=input$aspect_ratio2) 
    } else {
      plot <- pl2.2() + coord_fixed(ratio=input$aspect_ratio2)
    }
    return(plot)
  })
  
  # render the plot with control for aspect ratio
  output$Plot2 <- renderPlot({
    #if (input$confidence_region == TRUE)
    pl2() + coord_fixed(ratio=input$aspect_ratio2)
  })

  # get an analysis of variance table
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
  
  # render the table
  output$TableAOV <- renderTable({
    tb1()#[c(1,4,5,7,8),]
  })

  ##############################################
  ##          end compare slopes tab          ##
  ##############################################
    
  ##############################################
  ##          start slopes through time tab   ##
  ##############################################
    
  # same as third tab
  # except different plots
  
  # choose experiment
  whichExperiment4 <- reactive({
    DF <- switch(input$Experiment4,
                 "1" = WSALTslopes,
                 "2" = WTEMPslopes,
                 "3" = CSALTslopes )
    return(DF)
  })
  
  # create conditional panel
  # for subseting the experiment
  output$whichCheckBoxInput4 <- renderUI({
    DF <- whichExperiment4()
    out <- div(id = "PlotControls4", class = "collapse", 
               wellPanel(
                checkboxGroupInput(inputId = "chooseStrain4", label = h5("Strain"), inline = TRUE,
                                   choices = levels(DF$Strain),
                                   selected = levels(DF$Strain)[1:5]),
                checkboxGroupInput(inputId = "chooseTreatment4", label = h5("Treatment"), inline = TRUE,
                                   choices = levels(DF$Treatment),
                                   selected = levels(DF$Treatment)[1:5]),
                sliderInput(inputId = "Transfer4", label = h5("Transfer Range"), 
                            min = min(as.numeric(DF$seqRep)), 
                            max = max(as.numeric(DF$seqRep)), 
                            value = c(min(as.numeric(DF$seqRep)), 
                                      max(as.numeric(DF$seqRep)) ),
                            ticks=TRUE),
                selectInput(inputId = "switchPlot4", label = h5("Facet by:"),
                            choices = list("Strain" = 1, "Treatment" = 2),
                            selected = 1),
                sliderInput("aspect_ratio4", 
                            label = h5("Aspect ratio"),
                            min=1,
                            max=10,
                            value = 7, 
                            step=1,
                            ticks=TRUE)
               ) # end wellPanel
    ) # end collapsable div
    return(out)
  })
  
  # choose the subject (combination of strain and treatment)
  whichSubset4 <- reactive({
    DF <- isolate( whichExperiment4() )
    DF <- DF[which(DF$Strain %in% input$chooseStrain4 & DF$Treatment %in% input$chooseTreatment4), ]
    DF <- DF[which(as.numeric(DF$seqRep) >= input$Transfer4[1] & as.numeric(DF$seqRep) <= input$Transfer4[2]), ]
    return(DF)
  }) 
  
  # function to get the mean growth rate
  # from three Replicates within a Transfer
  getMean4 <- reactive({
    DF4 <- whichSubset4()
    meanSlopes <- ddply(.data=DF4,
                        .variables=.(Strain, Treatment, seqRep),
                        .fun=meanNsd)
    return(meanSlopes)
  })
  
  # plot the mean slopes by Transfer
  # facet by Strain
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
      ggtitle("Mean growth rate (+- 95% confidence interval, n=3) 
              from technical Replicates over the course of the experiment.") +
      ylab("Mean growth rate (slope)") +
      xlab("Transfer (sequential replicate)")
    return(plotMeanSlopesThruTime)
    
  })
  
  # plot the mean slopes by Transfer
  # facet by Treatment
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
      ggtitle("Mean growth rate (+- 95% confidence interval, n=3) 
              from technical Replicates over the course of the experiment.") +
      ylab("Mean growth rate (slope)") +
      xlab("Transfer (sequential replicate)")
    return(plotMeanSlopesThruTime)
    
  })
  
  # decide how to facet
  pl4 <- reactive({
    if (input$switchPlot4 == 1) {
      plot <- pl4.1() + coord_fixed(ratio=input$aspect_ratio4) 
    } else {
      plot <- pl4.2() + coord_fixed(ratio=input$aspect_ratio4)
    }
    return(plot)
  })
  
  # render
  output$Plot4 <- renderPlot({
    pl4() + coord_fixed(ratio=input$aspect_ratio4)
  })

  ##############################################
  ##          end slopes through time tab     ##
  ##############################################
  
  ##############################################
  ##     start compare growth rates tab       ##
  ##############################################

  # the logic is the same as the second tab
  # with group checkbox instead of select inputs
  # and a different output
  
  # choose experiment
  whichExperiment3 <- reactive({
    DF <- switch(input$Experiment3,
                 "1" = WSALTslopes, 
                 "2" = WTEMPslopes, 
                 "3" = CSALTslopes )
    return(DF)
  })
  
  # create conditional panel
  # for subseting an experiment
  output$whichCheckBoxInput3 <- renderUI({
    DF <- whichExperiment3()
    out <- div(id = "PlotControls3", class = "collapse", 
               wellPanel(
                checkboxGroupInput(inputId = "chooseStrain3", label = h5("Strain"), inline = TRUE,
                                   choices = levels(DF$Strain),
                                   selected = levels(DF$Strain)[1:5]),
                checkboxGroupInput(inputId = "chooseTreatment3", label = h5("Treatment"), inline = TRUE,
                                   choices = levels(DF$Treatment),
                                   selected = levels(DF$Treatment)[1:5]),
                radioButtons(inputId = "chooseMeasure3", label = h5("Measure"),
                             choices = list("Growth rate" = 1,
                                            "Divisions per day" = 2,
                                            "Doubling time" = 3 ),
                             selected = 1),
                sliderInput(inputId = "Transfer3", label = h5("Transfer Range"), 
                            min = min(as.numeric(DF$seqRep)), 
                            max = max(as.numeric(DF$seqRep)), 
                            value = c(min(as.numeric(DF$seqRep)), 
                                      max(as.numeric(DF$seqRep)) ) ),
                selectInput(inputId = "switchPlot3", label = h5("Facet by:"),
                            choices = list("Strain" = 1, "Treatment" = 2),
                            selected = 1)
               ) # end wellPanel
    ) # end collapsable div
    return(out)
  })
  
  # choose the subject (combination of strain and treatment)
  whichSubset3 <- reactive({
    DF <- isolate( whichExperiment3() )
    DF <- DF[which(DF$Strain %in% input$chooseStrain3 & DF$Treatment %in% input$chooseTreatment3), ]
    DF <- DF[which(as.numeric(DF$seqRep) >= input$Transfer3[1] & as.numeric(DF$seqRep) <= input$Transfer3[2]), ]
    return(DF)
  }) 
  
  # reactive function to get the mean/SD
  # and convert r to K and T2
  getMean <- reactive({
    DF3 <- whichSubset3()
    meanSlopes <- ddply(.data=DF3,
                        .variables=.(Strain, Treatment),
                        .fun=meanNsd)
    meanSlopes$Mean[which(meanSlopes$Mean < 0)] <- NA
    
    out <- mutate(meanSlopes,
                  K = Mean/log(2),
                  sdK = SD/log(2),
                  T2 = log(2)/Mean,
                  sdT2 =  SD + T2)
    return(out)
  })
  
  # subset the above data frame
  # to get only r, K or T2
  getData <- reactive({
    DAT <- getMean()
    out3 <- switch(input$chooseMeasure3,
                   "1" = DAT[,1:4],
                   "2" = DAT[,c(1:2,5:6)],
                   "3" = DAT[,c(1:2,7:8)] )
    colnames(out3) <- c("Strain", "Treatment", "MEAN", "SD")
    return(out3)
  })
  
  # plot
  # facet by Strain
  pl3.a1 <- reactive({
    DF3 <- getData()
    ENV3 <- environment()
    plotMeanSlopes <- ggplot(data = DF3, environment=ENV3, aes(x=Treatment, y=MEAN, fill=Treatment)) + 
      geom_bar(stat="identity", data=DF3, width=.8, colour="black") +
      geom_errorbar(aes(ymin=MEAN-SD, ymax=MEAN+SD), width=.2, data = DF3) +
      scale_fill_brewer(palette="Blues") + #, name="Salinity") +
      facet_grid(. ~ Strain) +
      theme_bw() +
      ggtitle("Mean growth rate (+- SD) across Transfers and Replicates at different salinities.") +
      ylab("Mean growth rate (slope)") 
    return(plotMeanSlopes)
    
  })
  
  # plot the coefficient of variation
  # not meaningful for T2
  # facet by Strain
  pl3.a2 <- reactive({
    DF3 <- getData()
    ENV3 <- environment()
    plotCV <- ggplot(data = DF3, environment=ENV3, aes(x=Treatment, y=abs(SD/MEAN), fill=Treatment)) + 
      geom_bar(stat="identity", data=DF3, width=.8, colour="black") +
      scale_fill_brewer(palette="Blues") + #, name="Salinity") +
      facet_grid(. ~ Strain) +
      theme_bw() +
      ggtitle("Coefficient of variation (|SD/Mean|) as a measure of 
              the variability around the mean growth rate across Transfers and Replicates.") +
      ylab("|Coefficient of variation|") #+
    #ylim(0,max(abs(DF3$SD/DF3$Mean))+0.02) 
    return(plotCV)
  }) 
  
  # plot
  # facet by Treatment
  pl3.b1 <- reactive({
    DF3 <- getData()
    ENV3 <- environment()
    plotMeanSlopes <- ggplot(data = DF3, environment=ENV3, aes(x=Strain, y=MEAN, fill=Treatment)) + 
      geom_bar(stat="identity", data=DF3, width=.8, colour="black") +
      geom_errorbar(aes(ymin=MEAN-SD, ymax=MEAN+SD), width=.2, data = DF3) +
      scale_fill_brewer(palette="Blues") + #, name="Salinity") +
      facet_grid(. ~ Treatment) +
      theme_bw() +
      ggtitle("Mean growth rate (+- SD) across Transfers and Replicates at different salinities.") +
      ylab("Mean growth rate (slope)") #+
    # ylim(0,1) 
    return(plotMeanSlopes)
    
  })
  
  # plot the coefficient of variation
  # meaningless for T2
  # facet by Treatment
  pl3.b2 <- reactive({
    DF3 <- getData()
    ENV3 <- environment()
    plotCV <- ggplot(data = DF3, environment=ENV3, aes(x=Strain, y=abs(SD/MEAN), fill=Treatment)) + 
      geom_bar(stat="identity", data=DF3, width=.8, colour="black") +
      scale_fill_brewer(palette="Blues") + #, name="Salinity") +
      facet_grid(. ~ Treatment) +
      theme_bw() +
      ggtitle("Coefficient of variation (|SD/Mean|) as a measure of 
              the variability around the mean growth rate across Transfers and Replicates.") +
      ylab("|Coefficient of variation|") #+
    #ylim(0,max(abs(DF3$SD/DF3$Mean))+0.02) 
    return(plotCV)
    
  }) 
  
  # decide how to facet
  # this needs fixing
  pl3 <- reactive({
    if (input$switchPlot3 == 1) {
      plot <- grid.arrange(pl3.a1(), pl3.a2(), nrow=2)
    } else {
      plot <- grid.arrange(pl3.b1(), pl3.b2(), nrow=2)
    }
    return(plot)
  })
  
  # render the barchart
  output$Plot3 <- renderPlot({
    pl3()
  })
  
  ##############################################
  ##     end compare growth rates tab         ##
  ##############################################
  
  ##############################################
  ##     end compare growth rates tab         ##
  ##############################################


  output$textAbout <- renderUI({
    HTML("<p>Tool to view and calculate growth rates.</p>")
  })
  
})
