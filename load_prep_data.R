
load_prep_data <- function() {
library(plyr)

# read all experiments: waller_salinity, waller_temperature, waller_flasks, cryptica_salinity
Wfami <- read.csv("./WFAMI.csv", header=TRUE)
Wsalt <- read.csv("./WSALT.csv", header=TRUE)
Wtemp <- read.csv("./WTEMP.csv", header=TRUE)
Wflas <- read.csv("./WFLAS.csv", header=TRUE)
Csalt <- read.csv("./CSALT.csv", header=TRUE)

# prepare the data
DAT <- rbind(Wfami, Wsalt, Wtemp, Wflas, Csalt)
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
              trDay=(Hour/24)+1)



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
                trDay=(Hour/24)+1)

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
                trDay=(Hour/24)+1)

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

return(list(DAT, WTEMPsplit, WTEMP, WSALTsplit, WSALT))

} # end function