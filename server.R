library(shiny)
library(shinythemes)
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)
library(nlme)
library(car)
library(RColorBrewer)

##############################################
##     define functions                     ##
##############################################

# function for transforming the metadata 
# for data uploads
makeMetaData <- function(dat.fram) {
  out <- mutate(.data=dat.fram,
                Date=Date + 1,
                Day=Day + 1,
                Hour=Hour + 24)
  return(out)
}

# function for transforming the metadata 
# for data uploads
makeMetaTransfer <- function(dat.fram) {
  out <- mutate(.data=dat.fram,
                Hour=rep(0,nrow(dat.fram)),
                Transfer=Transfer + 1)
  return(out)
}

# function to reformat the dataset
# after data upload
MUTATE <- function(x) {
  out <- mutate(.data=x,
                Date=as.Date(as.character(Date)),
                Rep=factor(Replicate),
                Transfer=as.numeric(Transfer), 
                seqRep=LETTERS[Transfer],
                Treatment=factor(Treatment),
                Temperature=factor(Temperature),
                Media=factor(Media),
                Experiment=factor(Experiment),
                Strain=factor(Strain),
                lnRF=log(RF-Rfctrl),
                trDay=as.integer((Hour/24)+1))
  return(out)
}

# get mean, SD
# for some of the plots
meanNsd <- function(x) { c(Mean=mean(x$trDay, na.rm=TRUE), SD=sd(x$trDay, na.rm=TRUE)) }

shinyServer(function(input, output, session) {

  ##############################################
  ##     load the data                        ##
  ##############################################
  source("./load_prep_data.R")
  ALLDATA <- load_prep_data()
  
  WTEMPsplit <- ALLDATA[[2]]
  WTEMP <- ALLDATA[[3]]
  WSALTsplit <- ALLDATA[[4]]
  WSALT <- ALLDATA[[5]]
  WTEMPslopes <- ALLDATA[[6]]
  WSALTslopes <- ALLDATA[[7]]
  CSALT <- ALLDATA[[8]]
  CSALTsplit <- ALLDATA[[9]]
  CSALTslopes <- ALLDATA[[10]]
  WFLAS <- ALLDATA[[11]]
  WFLAS2 <- ALLDATA[[12]]
  CFLAS <- ALLDATA[[14]]
  
  ##############################################
  ##     browse/upload/transfer data tab      ##
  ##############################################
  
  # assign dataset to plot
  # based on selected experiment
  whichExperiment0 <- reactive({
    DF <- switch(input$Experiment0,
                 "1" = WSALT,
                 "2" = WFLAS,
                 "3" = WFLAS2,
                 "4" = CSALT,
                 "5" = CFLAS,
                 "6" = WTEMP)
    return(DF)
  })
  
  # filename for output of updated data frames
  # based on selected experiment
  whichFilename0 <- reactive({
    DF <- switch(input$Experiment0,
                 "1" = "WSALT.csv",
                 "2" = "WFLAS.csv",
                 "3" = "WFLAS2.csv",
                 "4" = "CSALT.csv",
                 "5" = "CFLAS.csv",
                 "6" = "WTEMP.csv")
    return(DF)
  })
  
  ###
  # render a data table for all data form selected experiment
  output$Table0a <- renderDataTable({
    whichExperiment0()[,1:13]
  }, options = list(pageLength = 10) )
  
  ###
  # render a data table for the RNA
  output$Table0d <- renderDataTable({
    # load the RNA data
    read.csv("./RNA_PROGRESS_2015-03-02.csv", header=TRUE)
  }, options = list(pageLength = 10) )
  
  ###############################
  ##    processing new data    ##
  ###############################
  
  # read new data
  readNewData <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
#     read.csv(inFile$datapath, header = input$header, skip=input$skip_lines,
#              sep = input$sep, quote = input$quote)

  read.csv(inFile$datapath, header = FALSE, skip = 0, sep = "\t", quote = "")
  })
  
  Upload <- reactive({
    if (input$clearUpload) {
      out <- NULL
    } else {
      out <- readNewData()
    }
    return(out)
  })

  # prep new data
  prepNewData <- reactive({
    if (is.null(Upload()))
      return(NULL)
    
    newData <- data.frame(Upload())
    DF <- whichExperiment0()
    META <- DF[DF$Day==max(DF$Day) & DF$Transfer==max(DF$Transfer), ]
    newDay <- makeMetaData(META)
    
    blanks <- tail(newData$V2, n=input$numTreat)
    dataLen <- nrow(newData) - input$numTreat
    
    newDay <- newDay[1:dataLen, ]
    newDay$RF <- newData[1:dataLen, 2]
    newDay$Rfctrl <- rep(blanks, length.out=dataLen, each=input$numStrain)
    newDay <- MUTATE(newDay)
    return(newDay)
  })

  # render the new data table
  output$Table0b <- renderDataTable({
    prepNewData()[,1:13]
  }, options = list(pageLength = 10) )
  
  # commit the new data to main experiment file
  observe({
    if (input$submitNewData == 0)
      return(NULL)
    
    #input$submitNewData
    isolate({
      APPEND <- rbind(whichExperiment0(), prepNewData())
      write.csv(APPEND, file=paste(whichFilename0(), sep=""), row.names=FALSE)
      output$uploadSubmitted <- renderText({HTML("New data submitted. Refresh the browser to view.")})
    })
  })
  
  ###############################
  ##  end processing new data  ##
  ###############################
  
  ##################################
  ##    processing new transfer   ##
  ##################################
  
  # create a spreadsheet for transfer
  createTransfer <- reactive({
    if (input$calculateTransfer == 0)
      return(NULL)
    
    DF <- whichExperiment0()
    Today <- DF[DF$Day==max(DF$Day) & DF$Transfer==max(DF$Transfer), ]
    
    VOL <- input$finalVolume
    RFU <- input$desiredRFU
    
    out <- mutate(.data = Today,
                  VolCul = round((RFU * VOL)/(Today$RF-Today$Rfctrl), 2),
                  VolMed = round(VOL - VolCul, 2))
    
    return(out[,c(7,10,14:15,12:13,18:19)])
  })

  Transfer <- reactive({
    if (input$clearTransfer) {
      out <- NULL
    } else {
      out <- createTransfer()
    }
    return(out)    
  })

  # download the transfer spreadsheet
  output$downloadTransferSheet <- downloadHandler(
    filename =  function() {
      paste("Transfer_", Sys.Date(), '.csv', sep='')
    },
    content = function(file) { 
      write.csv(Transfer(), file)
    },
    contentType='text/csv'
  )
  
  # render the transfer table
  output$Table0c <- renderDataTable({
    Transfer()
  }, options = list(pageLength = 10) )
  
  # prep transfer data
  prepTransfer <- reactive({
    if (input$calculateTransfer == 0)
      return(NULL)
    
    DF <- whichExperiment0()
    META <- DF[DF$Day==max(DF$Day) & DF$Transfer==max(DF$Transfer), ]
    newDay <- makeMetaTransfer(META)
  
    newDay$RF <- input$desiredRFU
    newDay$Rfctrl <- rep(0, nrow(newDay))
    newDay <- MUTATE(newDay)
    return(newDay)
  })

  # commit the transfer data to experiment
  observe({
    if (input$submitTransfer == 0)
      return(NULL)
    
    isolate({
      APPEND <- rbind(whichExperiment0(), prepTransfer())
      write.csv(APPEND, file=paste(whichFilename0(), sep=""), row.names=FALSE)
      output$transferSubmitted <- renderText({HTML("Transfer data submitted. Refresh the browser to view.")})
    })
  })
  
  ####################################
  ##  end processing new transfer   ##
  ####################################

  ##############################################
  ##  end browse/upload/transfer data tab     ##
  ##############################################
  
  ########################################
  ##     start growth curves tab        ##
  ########################################
  
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
    if (is.null(whichExperiment1()))
      return(NULL)
    
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
    if (!isTRUE(input$logScale)) {
      pl1() + coord_fixed(ratio=0.001) + ylab("RFU")
    } else {
      pl1() + coord_fixed(ratio=input$aspect_ratio1) + 
        ylab("RFU (log10 scale)") +
        scale_y_continuous(trans=log10_trans(),
                           breaks = trans_breaks("log10", function(x) 10^x, n=3),
                           labels = trans_format("log10", math_format(10^.x)))
    }
  })

  ##############################################
  ##          end growth curves tab           ##
  ##############################################
    
  output$textAbout <- renderUI({
    HTML("<p>Tool to view and calculate growth rates.</p>")
  })
  
})
