
set.seed(11921)

### for central milk shed
rm(list=ls())

setwd("G:/My Drive/Classroom/Documents from Drive/Pre Doctoral KUL/Paper with Bjorn/CLONE_Origin")
path <- getwd()

farmers <- read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/dairy_farmers.csv", sep="/"), stringsAsFactors = FALSE)
mccs <- read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/MCCs.csv", sep="/"), stringsAsFactors = FALSE)
traders <- read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/traders.csv", sep="/"), stringsAsFactors = FALSE)

farmers$district <- toupper(farmers$district)
farmers$sub <- toupper(farmers$sub)

## district and subcounty names in CAPS, but they need cleaning - Do not just use them as FEs in regressions!
traders$district <- toupper(traders$district)
traders$sub <- toupper(traders$sub)
traders$trader.q11 <- toupper(traders$trader.q11)
traders$trader.q12 <- toupper(traders$trader.q12)
mccs$district <- toupper(mccs$district)
mccs$sub_county <- toupper(mccs$sub_county)

### create a ID and put this up front
farmers_C <- cbind(ID=paste("F",rownames(farmers),sep="_"), farmers)
traders_C <- cbind(ID=paste("T",rownames(traders),sep="_"), traders)
mccs_C <- cbind(ID=paste("M",rownames(mccs),sep="_"), mccs)

#### now for SW milkshed

farmers <- read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/Mbarara_farmers.csv", sep="/"), stringsAsFactors = FALSE)
mccs <- read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/Mbarara_MCC.csv", sep="/"), stringsAsFactors = FALSE)
traders <- read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/Mbarara_Traders.csv", sep="/"), stringsAsFactors = FALSE)

farmers$district <- toupper(farmers$district)
farmers$sub <- toupper(farmers$sub)

## district and subcounty names in CAPS, but they need cleaning - Do not just use them as FEs in regressions!
traders$district <- toupper(traders$district)
traders$sub <- toupper(traders$sub)
traders$trader.q11 <- toupper(traders$trader.q11)
traders$trader.q12 <- toupper(traders$trader.q12)
mccs$district <- toupper(mccs$district)
mccs$sub_county <- toupper(mccs$sub_county)

### create a ID and put this up front
farmers_SW <- cbind(ID=paste("F",rownames(farmers),sep="_"), farmers)
traders_SW <- cbind(ID=paste("T",rownames(traders),sep="_"), traders)
mccs_SW <- cbind(ID=paste("M",rownames(mccs),sep="_"), mccs)


### some issues the question on destination for MCCS, only q43.1, 98 and 99 are the same
### first rename
#names(mccs_C)[names(mccs_C) ==  "mcc.q43.2"] <- "mcc.q43.12"
#names(mccs_C)[names(mccs_C) ==  "mcc.q43.3"] <- "mcc.q43.13"
#names(mccs_C)[names(mccs_C) ==  "mcc.q43.4"] <- "mcc.q43.14"
#names(mccs_C)[names(mccs_C) ==  "mcc.q43.5"] <- "mcc.q43.15"
mccs_C[c("mcc.q43.6","mcc.q43.7","mcc.q43.8","mcc.q43.9","mcc.q43.10","mcc.q43.11")] <- FALSE 

###now we need to oder the variables in both mccs_C and mccs_SW in the same way before we can rbind
mccs_C <- mccs_C[c(names(mccs_C)[1:61],"mcc.q43.2","mcc.q43.3","mcc.q43.4","mcc.q43.5","mcc.q43.6", "mcc.q43.7","mcc.q43.8","mcc.q43.9","mcc.q43.10","mcc.q43.11",names(mccs_C)[66:106] )] 
mccs_SW <- mccs_SW[c(names(mccs_SW)[1:61],"mcc.q43.2","mcc.q43.3","mcc.q43.4","mcc.q43.5","mcc.q43.6", "mcc.q43.7","mcc.q43.8","mcc.q43.9","mcc.q43.10","mcc.q43.11",names(mccs_SW)[72:112] )] 

### for farmers selling to farmers that sell to MCC in the last 7 days, they really screwed up, with only recording frequency
#Frequency of payment of transactions with this trader that buys from farmers and sells to MCC in central q105
#and only recording price in SW q105x
farmers_SW$hh_head.HH.sales.q105 <- NA
farmers_C$hh_head.HH.sales.q105x <- NA
###now we need to insert these at correct positions
farmers_SW <- farmers_SW[c(names(farmers_SW)[1:230],"hh_head.HH.sales.q105",names(farmers_SW)[231:573])]


farmers_C <- farmers_C[c(names(farmers_C)[1:231],"hh_head.HH.sales.q105x",names(farmers_C)[232:573])]


### insert GPS coordinates 

mccs_C <- cbind(mccs_C,read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/MCCs.csv", sep="/"))[c("mcc._gps_latitude", "mcc._gps_longitude", "mcc._gps_altitude")])
mccs_SW <- cbind(mccs_SW,read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/Mbarara_MCC.csv", sep="/"))[c("mcc._gps_latitude", "mcc._gps_longitude", "mcc._gps_altitude")])

traders_C <- cbind(traders_C,read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/traders.csv", sep="/"))[c("trader._gps_latitude", "trader._gps_longitude", "trader._gps_altitude")])
traders_SW <- cbind(traders_SW,read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/Mbarara_Traders.csv", sep="/"))[c("trader._gps_latitude", "trader._gps_longitude", "trader._gps_altitude"  )])

farmers_C <- cbind(farmers_C,read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/dairy_farmers.csv", sep="/"))[c("hh_head.HH._gps_latitude", "hh_head.HH._gps_longitude", "hh_head.HH._gps_altitude")])
farmers_SW <- cbind(farmers_SW,read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/Mbarara_farmers.csv", sep="/"))[c("hh_head.HH._gps_latitude", "hh_head.HH._gps_longitude", "hh_head.HH._gps_altitude"  )])

#### insert telephone numbers and names of household members
farmers_C_raw <-  cbind(farmers_C[c("ID","district","sub","hh_head.HH._gps_latitude", "hh_head.HH._gps_longitude")],read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/dairy_farmers.csv", sep="/"))[c("parish","village","hh_head.HH.q3", 
                                                                                                                            "hh_head.HH.q3b","hh_head.HH.hh_member.1..q5a","hh_head.HH.hh_member.2..q5a","hh_head.HH.hh_member.3..q5a","hh_head.HH.hh_member.4..q5a","hh_head.HH.hh_member.5..q5a","hh_head.HH.hh_member.6..q5a","hh_head.HH.hh_member.7..q5a", "hh_head.HH.Housing.q17")])
farmers_SW_raw <-  cbind(farmers_SW[c("ID","district","sub","hh_head.HH._gps_latitude", "hh_head.HH._gps_longitude")],read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/Mbarara_farmers.csv", sep="/"))[c("parish","village","hh_head.HH.q3", "hh_head.HH.q3b","hh_head.HH.hh_member.1..q5a","hh_head.HH.hh_member.2..q5a","hh_head.HH.hh_member.3..q5a","hh_head.HH.hh_member.4..q5a","hh_head.HH.hh_member.5..q5a","hh_head.HH.hh_member.6..q5a","hh_head.HH.hh_member.7..q5a", "hh_head.HH.Housing.q17")])

traders_C_raw <-  cbind(traders_C[c("ID","district","sub","trader._gps_latitude", "trader._gps_longitude")],read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/traders.csv", sep="/"))[c("parish","village","trader.q1","trader.q2")])
traders_SW_raw <-  cbind(traders_SW[c("ID","district","sub","trader._gps_latitude", "trader._gps_longitude")],read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/Mbarara_traders.csv", sep="/"))[c("parish","village","trader.q1","trader.q2")])

mccs_C_raw <-  cbind(mccs_C[c("ID","district","sub_county","mcc._gps_latitude", "mcc._gps_longitude")],read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/MCCs.csv", sep="/"))[c("parish","village", "mcc.q2", "mcc.q7", "mcc.q3","mcc.q10")])
mccs_SW_raw <-  cbind(mccs_SW[c("ID","district","sub_county","mcc._gps_latitude", "mcc._gps_longitude")],read.csv(paste(path,"USAID_SME_project/Country folders/Uganda dairy/data/raw/Mbarara_MCC.csv", sep="/"))[c("parish","village", "mcc.q2", "mcc.q7", "mcc.q3","mcc.q10")])


mccs_SW_raw$shed <- "SW"
mccs_C_raw$shed <- "C"
mccs_raw <-  rbind(mccs_SW_raw, mccs_C_raw)

traders_SW_raw$shed <- "SW"
traders_C_raw$shed <- "C"
traders_raw <- rbind(traders_SW_raw, traders_C_raw)

farmers_SW_raw$shed <- "SW"
farmers_C_raw$shed <- "C"
farmers_raw <- rbind(farmers_SW_raw, farmers_C_raw)


### fix id
farmers_raw$ID <- paste("F", rownames(farmers_raw), sep="_")
traders_raw$ID <- paste("T", rownames(traders_raw), sep="_")
mccs_raw$ID <- paste("M", rownames(mccs_raw), sep="_")

farmers_raw <- subset(farmers_raw, hh_head.HH.q3b != 999 )
traders_raw <- subset(traders_raw, trader.q2 != 999)

write.csv(farmers_raw, "G:/My Drive/Classroom/Documents from Drive/Pre Doctoral KUL/Paper with Bjorn/CLONE_Origin/USAID_SME_project/Country folders/Uganda dairy/data/raw/farmers_raw_SW_C.csv")

#sampling all SW farmers 
SW_farm<- subset(farmers_raw, shed=="SW")
table(farmers_raw$shed=="SW") #323

#sampling 877 of the C farmers 
C_farm<- subset(farmers_raw, shed=="C")
sample_id<-data.frame(sample(C_farm$ID, size=877))

names(sample_id)[1] <- "ID" #changing var name 

#merging to get final farmer sample 
merge<-merge(sample_id, C_farm, all.x=TRUE)  
final<-rbind(merge,SW_farm)

#sampling all SW traders 
#SW_trade<- subset(traders_raw, shed=="SW")
#table(traders_raw$shed=="SW") #138

#sampling 362 of the C traders
#C_trade<- subset(traders_raw, shed=="C")
#sample_idt<-data.frame(sample(C_trade$ID, size=362))

#names(sample_idt)[1] <- "ID" #changing var name 

#merging to get final trader sample 
#merge_trade<-merge(sample_idt, C_trade, all.x=TRUE)  
#final_trader<-rbind(merge_trade,SW_trade)


names(mccs_raw)[11] <- "main_collection_centre" #changing var name 

table(mccs_raw$shed=="SW") #37 from SW and 55 from C

write.csv(final, "G:/My Drive/Classroom/Documents from Drive/Pre Doctoral KUL/Paper with Bjorn/CLONE_Origin/USAID_SME_project/Country folders/Uganda dairy/data/raw/farmers_sample.csv")
write.csv(mccs_raw, "G:/My Drive/Classroom/Documents from Drive/Pre Doctoral KUL/Paper with Bjorn/CLONE_Origin/USAID_SME_project/Country folders/Uganda dairy/data/raw/mcc_sample.csv")
write.csv(traders_raw, "G:/My Drive/Classroom/Documents from Drive/Pre Doctoral KUL/Paper with Bjorn/CLONE_Origin/USAID_SME_project/Country folders/Uganda dairy/data/raw/traders_sample.csv")


#39.15% of the MCCs should be from SW
#We have 37 from SW, we need 3 more
#We have 55 from C, we need 5 more 

