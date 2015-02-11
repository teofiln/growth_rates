
load_prep_data <- function() {
library(plyr)

# read all experiments: waller_salinity, waller_temperature, waller_flasks, cryptica_salinity
Wfami <- read.csv("./WFAMI.csv", header=TRUE)
Wsalt <- read.csv("./WSALT.csv", header=TRUE)
Wtemp <- read.csv("./WTEMP.csv", header=TRUE)
Wflas <- read.csv("./WFLAS.csv", header=TRUE)
Wflas2 <- read.csv("./WFLAS2.csv", header=TRUE)
Csalt <- read.csv("./CSALT.csv", header=TRUE)

# prepare the data
DAT <- rbind(Wfami, Wsalt, Wtemp, Wflas, Wflas2, Csalt)
DAT$Treatment[which(DAT$Treatment == "2")] <- "02"
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
              trDay=as.integer((Hour/24)+1))



# prep temperature trial data for ancova
WTEMP <- mutate(.data=Wtemp,
                Rep=factor(Replicate),
                seqRep=factor(Transfer),
                Transfer=as.numeric(Transfer), 
                Treatment=factor(Treatment),
                Temperature=factor(Temperature),
                Media=factor(Media),
                Experiment=factor(Experiment),
                Strain=factor(Strain),
                lnRF=log(RF-Rfctrl),
                trDay=as.integer((Hour/24)+1))

# ancova for temperature
ANCOV <- function(x) { aov(data=x, formula=lnRF ~ trDay*seqRep*Rep, na.action=na.exclude)}
wtempAncovas <- dlply(.data = WTEMP, .variables=.(Strain, Treatment), .fun=ANCOV)
wtempPredict <- ldply(.data = wtempAncovas, .variables=.(Strain, Treatment), .fun=predict)

# transpose the predicted values data frame
wtempPredict <- t(wtempPredict[,3:ncol(wtempPredict)])

# create a list of data frames of
# raw data + predicted values
WTEMPsplit <- dlply(WTEMP, .(Strain, Treatment), .fun=subset)
for (i in 1:ncol(wtempPredict)) {WTEMPsplit[[i]]$pred <- wtempPredict[,i]}

# function to fit linear model
# retaining only the slope
getSlope <- function(x) { coefficients(lm(formula=lnRF ~ trDay, data=x, na.action=na.exclude))[2] }

# function to get the mean and standard deviation of the slope
# given the output from above function
# currently not used 
meanNsd <- function(x) { c(Mean=mean(x$trDay, na.rm=TRUE), SD=sd(x$trDay, na.rm=TRUE)) }

# get slopes for Transfer > 2 
# assuming 'acclimation'
WTEMPafter2 <- WTEMP[which(WTEMP$Transfer > 2), ]

# ddply getting slope for each case
slopesTemp <- ddply(.data=WTEMPafter2, 
                    .variables=.(Strain, Treatment, seqRep, Rep),
                    .fun=getSlope)

# perhaps most appropriate
# estimate slopes of the model
# ln RF against Day for every level of Treatment
# with Strain, Transfer and Replicate giving the error 

#not used
treatAncova <- function(x) {
  fit <- aov(formula = lnRF ~ Treatment*trDay + Error(Rep * seqRep), 
             data = WTEMPafter2, na.action=na.exclude)
  return(fit)
}

# ddply getting slope for each case
treatSlopesTemp <- dlply(.data=WTEMPafter2, 
                    .variables=.(Strain, Treatment),
                    .fun=treatAncova)


# average by Strain and Treatment only 
# disregarding Rep and seqRep
# not used
# meanSlopesTemp <- ddply(.data=slopesTemp,
#                         .variables=.(Strain, Treatment),
#                         .fun=meanNsd)

# prep salinity trial data for ancova
WSALT <- mutate(.data=Wsalt,
                Rep=factor(Replicate),
                seqRep=factor(Transfer),
                Transfer=as.numeric(Transfer), 
                Treatment=factor(Treatment),
                Temperature=factor(Temperature),
                Media=factor(Media),
                Experiment=factor(Experiment),
                Strain=factor(Strain),
                lnRF=log(RF-Rfctrl),
                trDay=as.integer((Hour/24)+1))

WSALT <- WSALT[-which(WSALT$seqRep == "F"),]
# ancova for salinity
wsaltAncovas <- dlply(.data = WSALT, .variables=.(Strain, Treatment), .fun=ANCOV)
wsaltPredict <- ldply(.data = wsaltAncovas, .variables=.(Strain, Treatment), .fun=predict)

# transpose the predicted values data frame
wsaltPredict <- t(wsaltPredict[,3:ncol(wsaltPredict)])

# create a list of data frames of
# raw data + predicted values
WSALTsplit <- dlply(WSALT, .(Strain, Treatment), .fun=subset)
for (i in 1:ncol(wsaltPredict)) {WSALTsplit[[i]]$pred <- wsaltPredict[,i]}

# get slopes for Transfer > 2 
# assuming 'acclimation'
WSALTafter10 <- WSALT[which(WSALT$Transfer > 10), ]

# ddply getting slope for each case
slopesSalt <- ddply(.data=WSALTafter10, 
                    .variables=.(Strain, Treatment, seqRep, Rep),
                    .fun=getSlope)

return(list(DAT, WTEMPsplit, WTEMP, WSALTsplit, WSALT, slopesTemp, slopesSalt))

} # end function