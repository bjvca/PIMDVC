library(dplyr)

#Reading in raw data
#FARMERS
farmers <- read.csv("G:/My Drive/Classroom/Documents from Drive/KUL PhD/Uganda_PIMDVC/PIMDVC/wave_2021/raw_data/farmers/Farmers_DairyV3.csv")
farmers<-farmers[-c(1:8, 11:13, 17, 26, 27, 418:423 )] #removing variables not needed
farmers=farmers[,!grepl("X_", names(farmers))]  #removing variables starting with X_

#------------------------------------------------------------#

#MCCS
#we have two sets of raw data, follow-up mccs and new mccs
mcc <- read.csv("G:/My Drive/Classroom/Documents from Drive/KUL PhD/Uganda_PIMDVC/PIMDVC/wave_2021/raw_data/MCC/MCC_DairyV3_2021.csv")
mcc_new <- read.csv("G:/My Drive/Classroom/Documents from Drive/KUL PhD/Uganda_PIMDVC/PIMDVC/wave_2021/raw_data/MCC/NEW_MCC_2021.csv")

#dropping variables not needed
mcc<-mcc[-c(1:9, 12:14,16:17, 23, 24, 25, 26 )]
mcc_new<-mcc_new[-c(1:9, 12, 14:17 )]

#adding variables missing to be consistent in both datasets
mcc_new$hh_id<- NA
mcc_new$shed<- NA
mcc_new$q1<- NA
mcc_new$q1a<- NA
mcc_new$q1b<- NA
mcc_new$mcc.secB_group.b01<- NA
mcc_new$mcc.secB_group.b02.5<- NA
mcc_new$mcc.secB_group.b02.6<- NA
mcc_new$mcc.secB_group.b02.7<- NA
mcc_new$mcc.secB_group.b02.99<- NA
mcc_new$mcc.secB_group.b03<- NA
mcc_new$mcc.secB_group.b04<- NA
mcc_new$mcc.secB_group.b05.1<- NA
mcc_new$mcc.secB_group.b05.2<- NA
mcc_new$mcc.secB_group.b05.3<- NA
mcc_new$mcc.secB_group.b05.4<- NA
mcc_new$mcc.secB_group.b05.5<- NA
mcc_new$mcc.secB_group.b05.6<- NA
mcc_new$mcc.secB_group.b05.7<- NA
mcc_new$mcc.secB_group.b05.8<- NA
mcc_new$mcc.secB_group.b05.96<- NA

#mcc dataset
mccs <- rbind(mcc, mcc_new)
#removing some variables starting with X_
mccs=mccs[,!grepl("X_", names(mccs))] 
#removing location (GPS) variables 
mccs<-mccs[-c(237:242)]

#------------------------------------------------------------#

#TRADERS
#we have 3 sets of raw data - follow up traders (with random sampling), new traders, trader_xxx with the entire sample
trader <- read.csv("G:/My Drive/Classroom/Documents from Drive/KUL PhD/Uganda_PIMDVC/PIMDVC/wave_2021/raw_data/traders/TRADER_DairyV3.csv")
trader_new <- read.csv("G:/My Drive/Classroom/Documents from Drive/KUL PhD/Uganda_PIMDVC/PIMDVC/wave_2021/raw_data/traders/TRADER_NEW_2021.csv")
trader_xxx <- read.csv("G:/My Drive/Classroom/Documents from Drive/KUL PhD/Uganda_PIMDVC/PIMDVC/wave_2021/raw_data/traders/TRADER_XXX_2021.csv")

#trader and trader_xxx have same variables 
trad <- rbind(trader, trader_xxx)

#now we need to ensure same number of variables and same name of variables in the 2 trader datasets
#adding variables 
trader_new$trader.secB_group.b01<- NA
trader_new$trader.secB_group.b02.1<- NA
trader_new$trader.secB_group.b02.2<- NA
trader_new$trader.secB_group.b02.3<- NA
trader_new$trader.secB_group.b02.4<- NA
trader_new$trader.secB_group.b02.5<- NA
trader_new$trader.secB_group.b02.6<- NA
trader_new$trader.secB_group.b02.7<- NA
trader_new$trader.secB_group.b02.99<- NA
trader_new$trader.secB_group.b03<-NA
trader_new$trader.secB_group.b04<-NA
trader_new$trader.secB_group.b05.1<-NA
trader_new$trader.secB_group.b05.2<-NA
trader_new$trader.secB_group.b05.3<-NA
trader_new$trader.secB_group.b05.4<-NA
trader_new$trader.secB_group.b05.5<-NA
trader_new$trader.secB_group.b05.6<-NA
trader_new$trader.secB_group.b05.7<-NA
trader_new$trader.secB_group.b05.8<-NA
trader_new$trader.secB_group.b05.9<-NA
trader_new$trader.secB_group.b05.96<-NA
trader_new$shed<-NA
trader_new$q1<-NA
trader_new$hh_id<-NA

trad$trader.when<-NA

#dropping variables not needed
trad<-trad[-c(1:9, 12:14,16, 20:21)]
trader_new<-trader_new[-c(1:9, 12:13, 15:16)]

#trader dataset
traders <- rbind(trad, trader_new)
#removing some variables starting with X_
traders=traders[,!grepl("X_", names(traders))] 
#removing location (GPS) variables 
traders<-traders[-c(278:283)]

#------------------------------------------------------------#
