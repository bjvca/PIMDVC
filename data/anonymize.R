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
farmers <- cbind(ID=paste("F",rownames(farmers),sep="_"), farmers)
traders <- cbind(ID=paste("T",rownames(traders),sep="_"), traders)
mccs <- cbind(ID=paste("MCC",rownames(mccs),sep="_"), mccs)

write.csv(farmers, "/home/bjvca/data/projects/PIMDVC/data/public/farmers.csv", row.names=FALSE)
write.csv(traders, "/home/bjvca/data/projects/PIMDVC/data/public/traders.csv", row.names=FALSE)
write.csv(mccs, "/home/bjvca/data/projects/PIMDVC/data/public/mccs.csv", row.names=FALSE)
