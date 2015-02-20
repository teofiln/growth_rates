
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

###########################
# ANCOVAS FOR TEMPERATURE #
###########################

# prep temperature trial data for ancova
#Wtemp <- Wtemp[- which(Wtemp$Hour == 0), ]
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

# remove Day 0  (doesn't work)
# WTEMP <- WTEMP[- which(WTEMP$Hour == 0), ]

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
last3t <- max(WTEMP$Transfer) - 2
WTEMPlast3 <- WTEMP[which(WTEMP$Transfer > last3t), ]

# ddply getting slope for each case
slopesTemp <- ddply(.data=WTEMP, 
                    .variables=.(Strain, Treatment, seqRep, Rep),
                    .fun=getSlope)

# perhaps most appropriate
# estimate slopes of the model
# ln RF against Day for every level of Treatment
# with Strain, Transfer and Replicate giving the error 

#not used
# treatAncova <- function(x) {
#   fit <- aov(formula = lnRF ~ Treatment*trDay + Error(Rep * seqRep), 
#              data = WTEMPafter2, na.action=na.exclude)
#   return(fit)
# }
# 
# # ddply getting slope for each case
# treatSlopesTemp <- dlply(.data=WTEMPafter2, 
#                     .variables=.(Strain, Treatment),
#                     .fun=treatAncova)


# average by Strain and Treatment only 
# disregarding Rep and seqRep
# not used
# meanSlopesTemp <- ddply(.data=slopesTemp,
#                         .variables=.(Strain, Treatment),
#                         .fun=meanNsd)

###############################
# ANCOVAS FOR TEMPERATURE END #
###############################



###########################
# ANCOVAS FOR SALINITY    #
###########################

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
# remove Day 0 (doesn't work)
#WSALT <- WSALT[- which(WSALT$Hour == 0), ]

# remove Transfer F (start and end data only)
WSALT <- WSALT[- which(WSALT$seqRep == "F"),]

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
last3 <- max(WSALT$Transfer) - 2
WSALTlast3 <- WSALT[which(WSALT$Transfer > last3), ]

# ddply getting slope for each case
slopesSalt <- ddply(.data=WSALT, 
                    .variables=.(Strain, Treatment, seqRep, Rep),
                    .fun=getSlope)

############################
# ANCOVAS FOR SALINITY END #
############################


###########################
# ANCOVAS FOR CRYPTICA    #
###########################

# prep cryptica salinity trial data for ancova
CSALT <- mutate(.data=Csalt,
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

# remove Day 0 (doesn't work)
# CSALT <- CSALT[- which(CSALT$Hour == 0), ]

# ancova for salinity
CSALTAncovas <- dlply(.data = CSALT, .variables=.(Strain, Treatment), .fun=ANCOV)
CSALTPredict <- ldply(.data = CSALTAncovas, .variables=.(Strain, Treatment), .fun=predict)

# transpose the predicted values data frame
CSALTPredict <- t(CSALTPredict[,3:ncol(CSALTPredict)])

# create a list of data frames of
# raw data + predicted values
CSALTsplit <- dlply(CSALT, .(Strain, Treatment), .fun=subset)
for (i in 1:ncol(CSALTPredict)) {CSALTsplit[[i]]$pred <- CSALTPredict[,i]}

# get slopes for the last 3 Transfers
# assuming 'acclimation'
last3c <- max(CSALT$Transfer) - 2
CSALTlast3 <- CSALT[which(CSALT$Transfer > last3c), ]

# ddply getting slope for each case
slopesCSalt <- ddply(.data=CSALT, 
                    .variables=.(Strain, Treatment, seqRep, Rep),
                    .fun=getSlope)

############################
# ANCOVAS FOR CRYPTICA END #
############################

#############################################################
# Prep flasks data and familiarizing period of waller creek #
#############################################################

Wfami <- mutate(.data=Wfami,
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

Wflas <- mutate(.data=Wflas,
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

Wflas2 <- mutate(.data=Wflas2,
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

#############################################################
# END  flasks data and familiarizing period of waller creek #
#############################################################

return(list(DAT, 
            WTEMPsplit, 
            WTEMP, 
            WSALTsplit, 
            WSALT, 
            slopesTemp, 
            slopesSalt, 
            CSALT, 
            CSALTsplit, 
            slopesCSalt,
            Wflas,
            Wflas2,
            Wfami))

} # end function