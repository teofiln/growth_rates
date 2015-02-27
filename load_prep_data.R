load_prep_data <- function() {
library(plyr)

# read all experiments: waller_salinity, waller_temperature, waller_flasks, cryptica_salinity, ...
Wfami <- read.csv("./WFAMI.csv", header=TRUE)
Wsalt <- read.csv("./WSALT.csv", header=TRUE)
Wtemp <- read.csv("./WTEMP.csv", header=TRUE)
Wflas <- read.csv("./WFLAS.csv", header=TRUE)
Wflas2 <- read.csv("./WFLAS2.csv", header=TRUE)
Csalt <- read.csv("./CSALT.csv", header=TRUE)
Cflas <- read.csv("./CFLAS.csv", header=TRUE)

#########################
# define some functions #
#########################

# function to reformat the datasets
MUTATE <- function(x) { mutate(.data=x,
                                Rep=factor(Replicate),
                                seqRep=factor(Transfer),
                                Transfer=as.numeric(Transfer), 
                                Treatment=factor(Treatment),
                                Temperature=factor(Temperature),
                                Media=factor(Media),
                                Experiment=factor(Experiment),
                                Strain=factor(as.character(Strain)),
                                lnRF=log(RF-Rfctrl),
                                trDay=as.integer((Hour/24)+1))
                        }

# function to fit an ancova model. 
# to be used by plyr bellow
ANCOV <- function(x) { aov(data=x, formula=lnRF ~ trDay*seqRep*Rep, na.action=na.exclude)}

# function to fit linear model retaining only the slope. 
# to be used by plyr bellow
getSlope <- function(x) { coefficients(lm(formula=lnRF ~ trDay, data=x, na.action=na.exclude))[2] }

#########################
# Reformat the datasets #
#########################

WTEMP <- MUTATE(Wtemp)
WSALT <- MUTATE(Wsalt)
CSALT <- MUTATE(Csalt)
Wfami <- MUTATE(Wfami)
Wflas <- MUTATE(Wflas)
Wflas2 <- MUTATE(Wflas2)
Cflas <- MUTATE(Cflas)
# this combined dataset not used in app anymore
<<<<<<< HEAD
#DAT <- rbind(Wfami, Wsalt, Wtemp, Wflas, Wflas2, Csalt)
#DAT <- MUTATE(DAT)
=======
# DAT <- rbind(Wfami, Wsalt, Wtemp, Wflas, Wflas2, Csalt)
# DAT <- MUTATE(DAT)
DAT <- NULL
>>>>>>> 7df895e23cacd1346d3c09062789b582fe0711ad

###########################
# ANCOVAS FOR TEMPERATURE #
###########################

# remove Day 0
WTEMPnoZero <- droplevels(WTEMP[- which(WTEMP$Hour == 0), ])

# ancova for temperature
wtempAncovas <- dlply(.data = WTEMPnoZero, .variables=.(Strain, Treatment), .fun=ANCOV)
wtempPredict <- ldply(.data = wtempAncovas, .variables=.(Strain, Treatment), .fun=predict)

# transpose the predicted values data frame
wtempPredict <- t(wtempPredict[,3:ncol(wtempPredict)])

# create a list of data frames of
# raw data + predicted values
WTEMPsplit <- dlply(WTEMPnoZero, .(Strain, Treatment), .fun=subset)
for (i in 1:ncol(wtempPredict)) {WTEMPsplit[[i]]$pred <- wtempPredict[,i]}

# ddply getting slope for each case
slopesTemp <- ddply(.data=WTEMPnoZero, 
                    .variables=.(Strain, Treatment, seqRep, Rep),
                    .fun=getSlope)

########################################
# ancovas for Waller Creek salinity    #
########################################

# remove Day 0
WSALTnoZero <- droplevels(WSALT[- which(WSALT$Hour == 0), ])

# remove Transfer F (start and end data only)
WSALTnoZero <- droplevels(WSALTnoZero[- which(WSALTnoZero$seqRep == "F"), ])

# ancova for salinity
wsaltAncovas <- dlply(.data = WSALTnoZero, .variables=.(Strain, Treatment), .fun=ANCOV)
wsaltPredict <- ldply(.data = wsaltAncovas, .variables=.(Strain, Treatment), .fun=predict)

# transpose the predicted values data frame
wsaltPredict <- t(wsaltPredict[,3:ncol(wsaltPredict)])

# create a list of data frames of
# raw data + predicted values
WSALTsplit <- dlply(WSALTnoZero, .(Strain, Treatment), .fun=subset)
for (i in 1:ncol(wsaltPredict)) {WSALTsplit[[i]]$pred <- wsaltPredict[,i]}

# ddply getting slope for each case
slopesSalt <- ddply(.data=WSALTnoZero, 
                    .variables=.(Strain, Treatment, seqRep, Rep),
                    .fun=getSlope)

########################################
# ancovas for cryptica salinity        #
########################################

# remove the first three transfers w/o replication
CSALTnoZero <- droplevels(CSALT[- which(CSALT$Transfer < 4), ])

# remove Day 0
CSALTnoZero <- droplevels(CSALTnoZero[- which(CSALTnoZero$Hour == 0), ])

# ancova for salinity
CSALTAncovas <- dlply(.data = CSALTnoZero, .variables=.(Strain, Treatment), .fun=ANCOV)
CSALTPredict <- ldply(.data = CSALTAncovas, .variables=.(Strain, Treatment), .fun=predict)

# transpose the predicted values data frame
CSALTPredict <- t(CSALTPredict[,3:ncol(CSALTPredict)])

# create a list of data frames of
# raw data + predicted values
CSALTsplit <- dlply(CSALTnoZero, .(Strain, Treatment), .fun=subset)
for (i in 1:ncol(CSALTPredict)) {CSALTsplit[[i]]$pred <- CSALTPredict[,i]}

# ddply getting slope for each case
slopesCSalt <- ddply(.data=CSALTnoZero, 
                     .variables=.(Strain, Treatment, seqRep, Rep),
                     .fun=getSlope, .inform=TRUE)

#############################################################
# Bundle all the data in a list to be subset on server side #
#############################################################

return(list(DAT= NULL, 
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
            Wfami,
            Cflas))

} # end function