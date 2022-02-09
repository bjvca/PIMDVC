library(dplyr)
library(plyr)

#Reading in raw data
#FARMERS
farmers <- read.csv("G:/My Drive/Classroom/Documents from Drive/KUL PhD/Uganda_PIMDVC/PIMDVC/wave_2021/raw_data/farmers/Farmers_DairyV3.csv")

#since some farmers were in the tool but not in the list, date like q17, hh-id has not been obtained for these farmers ----- this has to be filled in
farmers_old <- read.csv("G:/My Drive/Classroom/Documents from Drive/Pre Doctoral KUL/Paper with Bjorn/CLONE_Origin/USAID_SME_project/Country folders/Uganda dairy/data/raw/farmers_raw_SW_C.csv")

farmers_sample<- read.csv("G:/My Drive/Classroom/Documents from Drive/Pre Doctoral KUL/Paper with Bjorn/CLONE_Origin/USAID_SME_project/Country folders/Uganda dairy/data/raw/farmers_sample.csv")

table(farmers$q17)  #156 n/a 
table(farmers$hh_name=="n/a") #TRUE = 156 
list(farmers$hh_namex[farmers$hh_name=="n/a"]) #list of farmer names which have n/a for hh_id, q17 etc. 

#prepping data for merging 
farm<- subset(farmers_old[c(2,9, 10, 18, 19)])
farm$hh_namex<-farm$hh_head.HH.q3 
farm$allshed <- farm$shed
farm<- subset(farm[-c(2,5)])

#trim trailing space 
farm$hh_namex <- trimws(farm$hh_namex, which = c("right"))

#merge
farmer_fin<- merge(farmers, farm, by="hh_namex")
farmer_final<- merge(farmers, farm, by="hh_namex")

#after merging, farmer_final has 2 extra obs 
#check for duplicates 
dup<-subset(farmer_final,duplicated(hh_namex)) 

#enumerator selected wrong farmer 
#Duplicate 1 --- Asaba Bukenya
table(farmer_final$ID[farmer_final$hh_namex=="Asaba Bukenya"]) #F_977 --- 2
table(farmer_final$village[farmer_final$hh_namex=="Asaba Bukenya"]) #both from same village
which(farmer_final$hh_namex == 'Asaba Bukenya')
farmer_final[48,]
farmer_final[49,] 

#enumerator selected wrong farmer 
#Duplicate 2 --- Bulemu Hassan
table(farmer_final$ID[farmer_final$hh_namex=="Bulemu Hassan"])
table(farmer_final$village[farmer_final$hh_namex=="Bulemu Hassan"]) #both from same village
which(farmer_final$hh_namex == 'Bulemu Hassan')
farmer_final[124,]
farmer_final[125,]
bulemu<- farmer_final[124:125,] #data varies for both the farmers 

#enumerator selected wrong farmer 
#Duplicate 3 - DIDASI TUMWESIGE
table(farmer_final$ID[farmer_final$hh_namex=="DIDASI TUMWESIGE"]) 
table(farmer_final$village[farmer_final$hh_namex=="DIDASI TUMWESIGE"])  #both from same village
which(farmer_final$hh_namex == 'DIDASI TUMWESIGE')
farmer_final[164,]
farmer_final[165,]
didasi<- farmer_final[164:165,] #data varies for both the farmers 


#Duplicate 4 - Dudu Godfrey
table(farmer_final$ID[farmer_final$hh_namex=="Dudu Godfrey"]) #different IDs
table(farmer_final$village[farmer_final$hh_namex=="Dudu Godfrey"])  #both from same village
which(farmer_final$hh_namex == 'Dudu Godfrey')
farmer_final[166,]
farmer_final[167,]
#In the raw data collected, only F_1162 is there, so we drop F_1199

#enumerator selected wrong farmer 
#Duplicate 5 - Kamurasi Isa
table(farmer_final$ID[farmer_final$hh_namex=="Kamurasi Isa"]) #same IDs
table(farmer_final$village[farmer_final$hh_namex=="Kamurasi Isa"])  #both from same village
which(farmer_final$hh_namex == 'Kamurasi Isa')
farmer_final[356,]
farmer_final[357,]
isa<- farmer_final[356:357,]

#enumerator selected wrong farmer 
#Duplicate 6 - Kankyiriho Apollo
table(farmer_final$ID[farmer_final$hh_namex=="Kankyiriho Apollo"]) #same IDs
table(farmer_final$village[farmer_final$hh_namex=="Kankyiriho Apollo"])  #both from same village
which(farmer_final$hh_namex == 'Kankyiriho Apollo')
farmer_final[366,]
farmer_final[367,]

#enumerator selected wrong farmer 
#Duplicate 7 - Kazungu misaki steven
table(farmer_final$ID[farmer_final$hh_namex=="Kazungu misaki steven"]) #same IDs
table(farmer_final$village[farmer_final$hh_namex=="Kazungu misaki steven"])  #both from same village
which(farmer_final$hh_namex == 'Kazungu misaki steven')
farmer_final[424,]
farmer_final[425,]

#Duplicate 8 - Mbabazi Justine
table(farmer_final$ID[farmer_final$hh_namex=="Mbabazi Justine"]) #different IDs
table(farmer_final$village[farmer_final$hh_namex=="Mbabazi Justine"])  #both from same village
which(farmer_final$hh_namex == 'Mbabazi Justine')
farmer_final[541,] #should only keep this 
farmer_final[542,]
#delete the duplicate as it does not exist in the collected data 

#enumerator selected wrong farmer 
#Duplicate 9 - Mutabazi Frank
table(farmer_final$ID[farmer_final$hh_namex=="Mutabazi Frank"]) #same IDs
table(farmer_final$village[farmer_final$hh_namex=="Mutabazi Frank"])  #both from same village
which(farmer_final$hh_namex == 'Mutabazi Frank')
farmer_final[649,] 
farmer_final[650,]

#duplicate 10 - Mwesigye David
table(farmer_final$ID[farmer_final$hh_namex=="Mwesigye David"]) #different IDs
table(farmer_final$village[farmer_final$hh_namex=="Mwesigye David"])  #both from same village
which(farmer_final$hh_namex == 'Mwesigye David')
farmer_final[673,] 
farmer_final[674,]
#The ID F_213 is the one in the collected data, so remove the other one. 

#enumerator selected wrong farmer 
#duplicate 11 - Mwesigye Godfrey Mujwiga
table(farmer_final$ID[farmer_final$hh_namex=="Mwesigye Godfrey Mujwiga"]) #same IDs
table(farmer_final$village[farmer_final$hh_namex=="Mwesigye Godfrey Mujwiga"])  #both from same village
which(farmer_final$hh_namex == 'Mwesigye Godfrey Mujwiga')
farmer_final[678,] 
farmer_final[679,]

#enumerator selected wrong farmer 
#duplicate 12 - Rwamishango Yonnah
table(farmer_final$ID[farmer_final$hh_namex=="Rwamishango Yonnah"]) #same IDs
table(farmer_final$village[farmer_final$hh_namex=="Rwamishango Yonnah"])  #both from same village
which(farmer_final$hh_namex == 'Rwamishango Yonnah')
farmer_final[855,] 
farmer_final[856,]

#enumerator selected wrong farmer 
#duplicate 13 - Rwomushana Geofrey
table(farmer_final$ID[farmer_final$hh_namex=="Rwomushana Geofrey"]) #same IDs
table(farmer_final$village[farmer_final$hh_namex=="Rwomushana Geofrey"])  #both from same village
which(farmer_final$hh_namex == 'Rwomushana Geofrey')
farmer_final[875,] 
farmer_final[876,]

#trim trailing space for village 
farmer_final$village <- trimws(farmer_final$village, which = c("both"))

#dropping duplicate rows based on decisions made above 
farmer_final<-subset(farmer_final[-(167),])
farmer_final<-subset(farmer_final[-(542),])
farmer_final<-subset(farmer_final[-(673),])

farmer_final<-farmer_final[-c(1:9, 13, 17, 26, 27, 438)] #removing variables not needed
farmer_final=farmer_final[,!grepl("X_", names(farmer_final))]   #removing variables starting with X_
farmer_final<-farmer_final[-c(405:410)]


#dairy.g18 has not been captured properly --- even if dairy.g16=q17, the enumerators have inserted answer for the change in most imp source of income being a result of COVID
table(farmer_final$dairy.g18)

table(farmer_final$dairy.g16[farmer_final$dairy.g18==1])
table(farmer_final$q17[farmer_final$dairy.g18==1])

table(farmer_final$dairy.g16[farmer_final$dairy.g18==2])
table(farmer_final$q17[farmer_final$dairy.g18==2])

table(farmer_final$dairy.g16[farmer_final$dairy.g18==3])
table(farmer_final$q17[farmer_final$dairy.g18==3])

table(farmer_final$dairy.g16[farmer_final$dairy.g18=="n/a"]) 
table(farmer_final$q17[farmer_final$dairy.g18=="n/a"])
#the changes are when the value is "n/a"

#So, dropping dairy.g18 completely
farmer_final$dairy.g18 <- NULL

#checking most important source of income -- dairy.g16 is from 2021
table(farmer_final$hh_head.HH.Housing.q17)
table(farmer_final$dairy.g16)

table(farmer_final$hh_head.HH.Housing.q17==1)
table(farmer_final$dairy.g16[farmer_final$hh_head.HH.Housing.q17=="1"])

table(farmer_final$hh_head.HH.Housing.q17==2)
table(farmer_final$dairy.g16[farmer_final$hh_head.HH.Housing.q17=="2"])

table(farmer_final$hh_head.HH.Housing.q17==3)
table(farmer_final$dairy.g16[farmer_final$hh_head.HH.Housing.q17=="3"])

table(farmer_final$hh_head.HH.Housing.q17==4)
table(farmer_final$dairy.g16[farmer_final$hh_head.HH.Housing.q17=="4"])

table(farmer_final$hh_head.HH.Housing.q17==5)
table(farmer_final$dairy.g16[farmer_final$hh_head.HH.Housing.q17=="5"])

table(farmer_final$hh_head.HH.Housing.q17==6)
table(farmer_final$dairy.g16[farmer_final$hh_head.HH.Housing.q17=="6"])

table(farmer_final$hh_head.HH.Housing.q17==7)
table(farmer_final$dairy.g16[farmer_final$hh_head.HH.Housing.q17=="7"])

table(farmer_final$hh_head.HH.Housing.q17==96)
table(farmer_final$dairy.g16[farmer_final$hh_head.HH.Housing.q17=="96"])

table(farmer_final$hh_head.HH.Housing.q17==98)
table(farmer_final$dairy.g16[farmer_final$hh_head.HH.Housing.q17=="98"])


#------------------------------------------------------------#

#MCCS
#we have two sets of raw data, follow-up mccs and new mccs
mcc <- read.csv("G:/My Drive/Classroom/Documents from Drive/KUL PhD/Uganda_PIMDVC/PIMDVC/wave_2021/raw_data/MCC/MCC_DairyV3_2021.csv")
mcc_new <- read.csv("G:/My Drive/Classroom/Documents from Drive/KUL PhD/Uganda_PIMDVC/PIMDVC/wave_2021/raw_data/MCC/NEW_MCC_2021.csv")

#dropping variables not needed
mcc<-mcc[-c(1:9, 13:14,16:17, 23, 24, 25, 26 )]
mcc_new<-mcc_new[-c(1:9, 14:17 )]

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

mcc_loc<-mccs

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
trad<-trad[-c(1:9, 13:14,16, 20:21)]
trader_new<-trader_new[-c(1:9, 13, 15:16)]

#trader dataset
traders <- rbind(trad, trader_new)
trad_dairy <- rbind(trad, trader_new)
#removing some variables starting with X_
traders=traders[,!grepl("X_", names(traders))] 
#removing location (GPS) variables 
traders<-traders[-c(278:283)]

#trim trailing space 
traders$village <- trimws(traders$village, which = c("both"))

#assigning IDs to all traders 
traders_ID <- cbind(ID=paste("T",rownames(traders),sep="_"), traders)


#------------------------------------------------------------#

### CHECKING IF VILLAGE NAMES MATCH FOR FARMERS, TRADERS AND MCCS BASED ON THE MAP    --------  TO BE DONE 
table(traders$village[traders$village=="Commercial street"])
table(mccs$village[mccs$village=="Commercial street"])

#------------------------------------------------------------#

#### COMPETITION - FARMERS DATASET

#number of traders in the neighbourhood 
table(farmer_final$dairy.sales.q95)

farmer_final$nr_trad <- farmer_final$dairy.sales.q95

#a farmer reported 0 trader in the neighbourhood but sold to one trader - so changing the value to 1 
farmer_final$nr_trad [farmer_final$dairy.sales.q95=="0"] <- 1

farmer_final$nr_trad [farmer_final$dairy.sales.q95=="999"] <- NA
farmer_final$nr_trad [farmer_final$dairy.sales.q95=="9999"] <- NA
farmer_final$nr_trad [farmer_final$dairy.sales.q95=="n/a"] <- NA
table (farmer_final$nr_trad)

mean(as.numeric(farmer_final$nr_trad), na.rm=T) #trader competition at the farmer level

count_farm<-data.frame(cbind(lm(farmer_final$nr_trad~-1+farmer_final$village)$coef)) #average number of traders in each village
count_farm$village <- rownames(count_farm)
colnames(count_farm)[1] <- "avg" 
count_farm$village<-gsub("farmer_final$village","",count_farm$village,fixed = TRUE)
rownames(count_farm) <- 1:nrow(count_farm)

##COMPETITION - TRADERS DATASET

count<- count(traders_ID, "village") #counting number of traders in each village 

traders_ID<-traders_ID[c("ID", "village")] #subset
traders_ID$village<-toupper(traders_ID$village) #changing case
comp<-merge(traders_ID, count_farm) #merging 
#we only get 28 matches --- check if this is correct
mean(comp$avg, na.rm=T) #4.12381

traders$trader.secC_group.secC_groupcomp.q24 [traders$trader.secC_group.secC_groupcomp.q24=="999"] <- NA
traders$trader.secC_group.secC_groupcomp.q24 [traders$trader.secC_group.secC_groupcomp.q24=="n/a"] <- NA
table(traders$trader.secC_group.secC_groupcomp.q24)

#average number of traders competing as reported by the traders 
mean(as.numeric(traders$trader.secC_group.secC_groupcomp.q24), na.rm=T)  # 23.07197
#average number of traders competing as reported by the traders whose area of operation is a village 
mean(as.numeric(traders$trader.secC_group.secC_groupcomp.q24[traders$trader.secC_group.q94==1]), na.rm=T) #  25.29333


count_trad<-data.frame(cbind(lm(traders$trader.secC_group.secC_groupcomp.q24~-1+traders$village)$coef)) #average trader competition according to traders 
count_trad$village <- rownames(count_trad)
colnames(count_trad)[1] <- "avg" 
count_trad$village<-gsub("traders$village","",count_trad$village,fixed = TRUE)
rownames(count_trad) <- 1:nrow(count_trad)

#------------------------------------------------------------#



library(leaflet)

#create map
#traders <-  traders[ is.na(traders$nr_traders),]

farmer_fin<-subset (farmer_fin, dairy._gps_longitude!="n/a") #38 farmers - NAs
mcc_loc<- subset (mcc_loc, mcc._gps_longitude!="n/a") #7 NAs

#changing names of the location variables to match
names(trad_dairy)[283] <-"dairy._gps_longitude"
names(trad_dairy)[282] <-"dairy._gps_latitude"
names(mcc_loc)[240] <-"dairy._gps_longitude"
names(mcc_loc)[239] <-"dairy._gps_latitude"

trad_dairy<-subset (trad_dairy, dairy._gps_longitude!="n/a") #16 are NAs

to_plot_f <- farmer_fin[c("dairy._gps_longitude", "dairy._gps_latitude","village")]
to_plot_t <- trad_dairy[c("dairy._gps_longitude", "dairy._gps_latitude","village")]
to_plot_m <- mcc_loc[c("dairy._gps_longitude", "dairy._gps_latitude","village")]

to_plot_f$actor <- "farmer"
to_plot_t$actor <- "trader"
to_plot_m$actor <- "mcc"

#data for plotting
to_plot <- rbind(to_plot_f,to_plot_t)
to_plot_all <- rbind(to_plot_f,to_plot_t, to_plot_m)

pal <- colorFactor(c("green", "red"), domain = c("farmer", "trader"))
pal_all <- colorFactor(c("green", "red", "blue"), domain = c("farmer", "trader", "mcc"))

m <- leaflet() %>% setView(lat = 0.6, lng = 33.5, zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google') %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=to_plot, lng=~as.numeric(as.character(dairy._gps_longitude)), 
                                                                                    lat=~as.numeric(as.character(dairy._gps_latitude)),radius= 3, label=~as.character(village),color=~pal(actor), group="X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))

m #traders and farmers

m_all <- leaflet() %>% setView(lat = 0.6, lng = 33.5, zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google') %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=to_plot_all, lng=~as.numeric(as.character(dairy._gps_longitude)), 
                                                                                                                                                                                                                                                                                                                        lat=~as.numeric(as.character(dairy._gps_latitude)),radius= 2, label=~as.character(village),color=~pal_all(actor), group="X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))
m_all #traders, farmers and mccs 

library(htmlwidgets)
saveWidget(m, file="farm_trad_dairy.html") #traders and farmers 
saveWidget(m_all, file="dairy_21.html") #traders, farmers and mccs 
