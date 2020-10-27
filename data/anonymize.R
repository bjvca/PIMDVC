### for central milk shed
rm(list=ls())
farmers <- read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/dairy_farmers.csv")
mccs <- read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/MCCs.csv")
traders <- read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/traders.csv")

### remove location (GPS)
farmers <- farmers[,1:605]
mccs <- mccs[,1:119]
traders <- traders[,1:124]

### drop junk such as deviceID etc
farmers <- farmers[,-c(1:7)]
mccs <- mccs[,-c(1:7)]
traders <- traders[,-c(1:7)]

### mask enumerator names
farmers$enumerator <- paste("EN",as.numeric(farmers$enumerator),sep="_")
mccs$enumerator <- paste("EN",as.numeric(mccs$enumerator),sep="_")
traders$enumerator <- paste("EN",as.numeric(traders$enumerator),sep="_")

####remove additional data that may identify individuals
farmers <- farmers[,!(names(farmers) %in% c("parish","village","hh_head.consent", "hh_head.HH.q3", "hh_head.HH.q3b","hh_head.HH.child_name")) ]
farmers <- farmers[,!(names(farmers) %in%  unlist(lapply(1:20, function( i) paste(paste("hh_head.HH.hh_member",i,sep="."),"q5a",sep=".."))))]
farmers$district <- toupper(farmers$district)
farmers$sub <- toupper(farmers$sub)

traders <- traders[,!(names(traders) %in% c("parish","village","consent","trader.q1","trader.q2","trader.q13")) ]
mccs <- mccs[,!(names(mccs) %in% c("parish","village","consent",  "mcc.q2", "mcc.q3", "mcc.q7", "mcc.q10")) ]
## district and subcounty names in CAPS, but they need cleaning - Do not just use them as FEs in regressions!
traders$district <- toupper(traders$district)
traders$sub <- toupper(traders$sub)
traders$trader.q11 <- toupper(traders$trader.q11)
traders$trader.q12 <- toupper(traders$trader.q12)
mccs$district <- toupper(mccs$district)
mccs$sub_county <- toupper(mccs$sub_county)

### create a trader ID and put this up front
farmers_C <- cbind(ID=paste("F",rownames(farmers),sep="_"), farmers)
traders_C <- cbind(ID=paste("T",rownames(traders),sep="_"), traders)
mccs_C <- cbind(ID=paste("MCC",rownames(mccs),sep="_"), mccs)

#### now for SW milkshed

farmers <- read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/Mbarara_farmers.csv")
mccs <- read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/Mbarara_MCC.csv")
traders <- read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/Mbarara_Traders.csv")

### remove location (GPS)
farmers <- farmers[,1:605]
mccs <- mccs[,1:125]
traders <- traders[,1:124]

### drop junk such as deviceID etc
farmers <- farmers[,-c(1:7)]
mccs <- mccs[,-c(1:7)]
traders <- traders[,-c(1:7)]

### mask enumerator names
farmers$enumerator <- paste("EN",as.numeric(farmers$enumerator),sep="_")
mccs$enumerator <- paste("EN",as.numeric(mccs$enumerator),sep="_")
traders$enumerator <- paste("EN",as.numeric(traders$enumerator),sep="_")

####remove additional data that may identify individuals

farmers <- farmers[,!(names(farmers) %in% c("parish","village","hh_head.consent", "hh_head.HH.q3", "hh_head.HH.q3b","hh_head.HH.child_name")) ]
farmers <- farmers[,!(names(farmers) %in%  unlist(lapply(1:20, function( i) paste(paste("hh_head.HH.hh_member",i,sep="."),"q5a",sep=".."))))]
farmers$district <- toupper(farmers$district)
farmers$sub <- toupper(farmers$sub)

traders <- traders[,!(names(traders) %in% c("parish","village","consent","trader.q1","trader.q2","trader.q13")) ]
mccs <- mccs[,!(names(mccs) %in% c("parish","village","consent",  "mcc.q2", "mcc.q3", "mcc.q7", "mcc.q10")) ]
## district and subcounty names in CAPS, but they need cleaning - Do not just use them as FEs in regressions!
traders$district <- toupper(traders$district)
traders$sub <- toupper(traders$sub)
traders$trader.q11 <- toupper(traders$trader.q11)
traders$trader.q12 <- toupper(traders$trader.q12)
mccs$district <- toupper(mccs$district)
mccs$sub_county <- toupper(mccs$sub_county)

### create a trader ID and put this up front
farmers_SW <- cbind(ID=paste("F",rownames(farmers),sep="_"), farmers)
traders_SW <- cbind(ID=paste("T",rownames(traders),sep="_"), traders)
mccs_SW <- cbind(ID=paste("MCC",rownames(mccs),sep="_"), mccs)


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


### insert GPS coordinates - for liz 
#mccs_C <- cbind(mccs_C,read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/MCCs.csv")[c("mcc._gps_latitude", "mcc._gps_longitude", "mcc._gps_altitude")])
#mccs_SW <- cbind(mccs_SW,read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/Mbarara_MCC.csv")[c("mcc._gps_latitude", "mcc._gps_longitude", "mcc._gps_altitude")])

#traders_C <- cbind(traders_C,read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/traders.csv")[c("trader._gps_latitude", "trader._gps_longitude", "trader._gps_altitude")])
#traders_SW <- cbind(traders_SW,read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/Mbarara_Traders.csv")[c("trader._gps_latitude", "trader._gps_longitude", "trader._gps_altitude"  )])

#farmers_C <- cbind(farmers_C,read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/dairy_farmers.csv")[c("hh_head.HH._gps_latitude", "hh_head.HH._gps_longitude", "hh_head.HH._gps_altitude")])
#farmers_SW <- cbind(farmers_SW,read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/Mbarara_farmers.csv")[c("hh_head.HH._gps_latitude", "hh_head.HH._gps_longitude", "hh_head.HH._gps_altitude"  )])

#### insert telephone numbers for follow up
farmers_C_follow_up <-  cbind(farmers_C[c("district","sub")],read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/dairy_farmers.csv")[c("parish","village","hh_head.HH.q3", "hh_head.HH.q3b")])
farmers_SW_follow_up <-  cbind(farmers_SW[c("district","sub")],read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/Mbarara_farmers.csv")[c("parish","village","hh_head.HH.q3", "hh_head.HH.q3b")])

traders_C_follow_up <-  cbind(traders_C[c("district","sub")],read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/traders.csv")[c("parish","village","trader.q1","trader.q2")])
traders_SW_follow_up <-  cbind(traders_SW[c("district","sub")],read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/Mbarara_Traders.csv")[c("parish","village","trader.q1","trader.q2")])

mccs_C_follow_up <-  cbind(mccs_C[c("district","sub_county")],read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/MCCs.csv")[c("parish","village", "mcc.q2", "mcc.q7", "mcc.q3")])
mccs_SW_follow_up <-  cbind(mccs_SW[c("district","sub_county")],read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/Mbarara_MCC.csv")[c("parish","village", "mcc.q2", "mcc.q7", "mcc.q3")])


mccs_SW$shed <- "SW"
mccs_C$shed <- "C"
mccs <-  rbind(mccs_SW, mccs_C)

mccs_SW_follow_up$shed <- "SW"
mccs_C_follow_up$shed <- "C"
mccs_follow_up <-  rbind(mccs_SW_follow_up, mccs_C_follow_up)


traders_SW$shed <- "SW"
traders_C$shed <- "C"
traders <- rbind(traders_SW, traders_C)

traders_SW_follow_up$shed <- "SW"
traders_C_follow_up$shed <- "C"
traders_follow_up <- rbind(traders_SW_follow_up, traders_C_follow_up)

farmers_SW$shed <- "SW"
farmers_C$shed <- "C"
farmers <- rbind(farmers_SW, farmers_C)

farmers_SW_follow_up$shed <- "SW"
farmers_C_follow_up$shed <- "C"
farmers_follow_up <- rbind(farmers_SW_follow_up, farmers_C_follow_up)

### fix id
farmers$ID <- paste("F", rownames(farmers), sep="_")
traders$ID <- paste("T", rownames(traders), sep="_")
mccs$ID <- paste("M", rownames(mccs), sep="_")

farmers_follow_up$ID <- paste("F", rownames(farmers_follow_up), sep="_")
traders_follow_up$ID <- paste("T", rownames(traders_follow_up), sep="_")
mccs_follow_up$ID <- paste("M", rownames(mccs_follow_up), sep="_")

farmers_follow_up <- subset(farmers_follow_up, hh_head.HH.q3b != 999 )
traders_follow_up <- subset(traders_follow_up, trader.q2 != 999)


write.csv(farmers_follow_up, "/home/bjvca/data/projects/PIMDVC/data/raw/farmers_follow_up.csv")
write.csv(traders_follow_up, "/home/bjvca/data/projects/PIMDVC/data/raw/traders_follow_up.csv")
write.csv(mccs_follow_up, "/home/bjvca/data/projects/PIMDVC/data/raw/mccs_follow_up.csv")


#### uncomment if you also want gps coordinates
#write.csv(farmers, "/home/bjvca/data/projects/PIMDVC/data/public/farmers_gps.csv")
#write.csv(traders, "/home/bjvca/data/projects/PIMDVC/data/public/traders_gps.csv")
#write.csv(mccs, "/home/bjvca/data/projects/PIMDVC/data/public/mccs_gps.csv")

#merge in travel time in minutes to nearest small town (>50,000 inhabitants -  Weiss et al. (2018) https://www.nature.com/articles/nature25181/) 

farmers <- merge(farmers, read.csv("/home/bjvca/data/projects/PIMDVC/data/non_public/TravelTimeNearest50kCity_farmers.csv")[c("ID","Travel_time_min")] ,all.x=T)

write.csv(farmers, "/home/bjvca/data/projects/PIMDVC/data/public/farmers.csv")
write.csv(traders, "/home/bjvca/data/projects/PIMDVC/data/public/traders.csv")
write.csv(mccs, "/home/bjvca/data/projects/PIMDVC/data/public/mccs.csv")


