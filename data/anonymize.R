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

mccs_C[c("mcc.q43.6","mcc.q43.7","mcc.q43.8","mcc.q43.9","mcc.q43.10","mcc.q43.11")] <- NA 
###now we need to oder the variables in mccs_C to the same order as variable names in mccs_SW before we can rbind
mccs_C <- mccs_C[names(mccs_SW)]

### for farmers selling to farmers that sell to MCC in the last 7 days, they really screwed up, with only recording frequency
#Frequency of payment of transactions with this trader that buys from farmers and sells to MCC in central q105
#and only recording price in SW q105x
farmers_SW$hh_head.HH.sales.q105 <- NA
farmers_C$hh_head.HH.sales.q105x <- NA
###now we need to insert these at correct positions
farmers_SW <- farmers_SW[c(names(farmers_SW)[1:230],"hh_head.HH.sales.q105",names(farmers_SW)[231:573])]


farmers_C <- farmers_C[c(names(farmers_C)[1:231],"hh_head.HH.sales.q105x",names(farmers_C)[232:573])]


mccs_SW$shed <- "SW"
mccs_C$shed <- "C"
mccs <-  rbind(mccs_SW, mccs_C)

traders_SW$shed <- "SW"
traders_C$shed <- "C"
traders <- rbind(traders_SW, traders_C)

farmers_SW$shed <- "SW"
farmers_C$shed <- "C"
farmers <- rbind(farmers_SW, farmers_C)

### fix id
farmers$ID <- paste("F", rownames(farmers), sep="_")
traders$ID <- paste("T", rownames(traders), sep="_")
mccs$ID <- paste("M", rownames(mccs), sep="_")


write.csv(farmers, "/home/bjvca/data/projects/PIMDVC/data/public/farmers.csv")
write.csv(traders, "/home/bjvca/data/projects/PIMDVC/data/public/traders.csv")
write.csv(mccs, "/home/bjvca/data/projects/PIMDVC/data/public/mccs.csv")

