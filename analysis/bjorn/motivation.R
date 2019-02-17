#milk consumption in 2005/06
library(foreign) 
library(ggplot2)
rm(list=ls())

#food, beverages and tobacco
secA <- read.dta("/home/bjvca/data/projects/GAP/Haruna/UNHS_2005/UNHS_III_SOCIO_DATA/hsec14a.dta")
secA[is.na(secA)] <- 0
milk <- subset(secA,h14aq2=="fresh milk")
#keep only liters and cup/mug

milk$conv <- NA

milk$conv[milk$h14aq3=="Cup/Mug (0.5 lt)"] <- .5
milk$conv[milk$h14aq3=="Liter"] <- 1
milk$conv[milk$h14aq3=="nice cup"] <- .57
milk$conv[milk$h14aq3=="Jerrican (5 lts)"] <- 5
milk$conv[milk$h14aq3=="Jerrican (1 lt)"] <- 1
milk$conv[milk$h14aq3=="Bottle (500 ml)"] <- .5
milk$conv[milk$h14aq3=="Bottle (350 ml)"] <- .35          
milk$conv[milk$h14aq3=="Bottle (300 ml)"] <- .3

milk$totmilk <-  (milk$h14aq4 + milk$h14aq6 + milk$h14aq8 +  milk$h14aq10)*milk$conv
milk2005 <- milk

### per capita
hhsize <- data.frame(read.dta("/home/bjvca/data/projects/GAP/Haruna/UNHS_2005/UNHS_III_SOCIO_DATA/hsec14.dta")[,1],rowSums(read.dta("/home/bjvca/data/projects/GAP/Haruna/UNHS_2005/UNHS_III_SOCIO_DATA/hsec14.dta")[,2:9], na.rm=T))
names(hhsize) <- c("hh","hhsize")
hhsize$hhsize[hhsize$hhsize >= 30] <- NA
milk2005 <- merge(milk2005, hhsize[c("hh","hhsize")])
milk2005$milk_cap <- (milk2005$totmilk/milk2005$hhsize)*52.177457
### merge in weights and location
loc <- read.dta("/home/bjvca/data/projects/GAP/Haruna/UNHS_2005/UNHS_III_SOCIO_DATA/hsec1b.dta")[c("hh","district","substrat","hmult")]
milk2005 <- merge(milk2005,loc, by="hh", all.y=T)
milk2005$milk_cap[is.na(milk2005$milk_cap)] <- 0
milk2005$milk_cap[milk2005$milk_cap > 800] <- 0
weighted.mean(milk2005$milk_cap, milk2005$hmult)
unlist(lapply(split(milk2005, milk2005$substrat), function(z) weighted.mean(z$milk_cap, z$hmult)))

###2009_10
#food, beverages and tobacco
secA <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS 2009_10 DATASET/SOCIO/HSEC10A_CLN.dta")
secA[is.na(secA)] <- 0
milk <- subset(secA,itmcd=="Fresh Milk")
#keep only liters and cup/mug

milk$conv <- NA

milk$conv[milk$untcd=="GLASS (0.25)"] <- .25
milk$conv[milk$untcd=="LITRE"] <- 1
milk$conv[milk$untcd=="CUP/ MUG (0.5)"] <- .5

milk$totmilk <-  (milk$h10aq4 + milk$h10aq6 + milk$h10aq8 +  milk$h10aq10)*milk$conv
milk2009 <- milk

### per capita
hhsize <- data.frame(read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS 2009_10 DATASET/SOCIO/HSEC10AA.dta")[,1],rowSums(read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS 2009_10 DATASET/SOCIO/HSEC10AA.dta")[,2:9], na.rm=T))
names(hhsize) <- c("hh","hhsize")
hhsize$hhsize[hhsize$hhsize >= 30] <- NA
hhsize$hhsize[hhsize$hhsize == 0] <- NA
milk2009 <- merge(milk2009, hhsize[c("hh","hhsize")])
milk2009$milk_cap <- (milk2009$totmilk/milk2009$hhsize)*52.177457

### merge in weights and location
loc <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS 2009_10 DATASET/SOCIO/HSEC1.dta")[c("hh","sregion","urban","hmult")]
milk2009 <- merge(milk2009,loc, by="hh", all.y=T)
milk2009$milk_cap[is.na(milk2009$milk_cap)] <- 0
milk2009$milk_cap[milk2009$milk_cap > 800] <- 0
weighted.mean(milk2009$milk_cap, milk2009$hmult)
unlist(lapply(split(milk2009, milk2009$urban), function(z) weighted.mean(z$milk_cap, z$hmult)))


###2012_13
secA <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS2012_13/GSEC6b.dta")

secA[is.na(secA)] <- 0
milk <- subset(secA ,itmcd==125)

milk$conv <- NA

milk$conv[milk$untcd==32] <- .5
milk$conv[milk$untcd==3] <- 1
milk$conv[milk$untcd==107] <- 0.58/1.04
milk$conv[milk$untcd==108] <- 0.45/1.04
milk$conv[milk$untcd==109] <- 0.35/1.04

milk$totmilk <-  (milk$ceb06 + milk$ceb08 + milk$ceb10 + milk$ceb12)*milk$conv
milk$totmilk[milk$totmilk >50 ] <- NA   
milk2012  <- milk


### per capita
hhsize <- data.frame(read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS2012_13/GSEC6a.dta")[,1],rowSums(read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS2012_13/GSEC6a.dta")[,4:11], na.rm=T))
names(hhsize) <- c("HHID","hhsize")
hhsize$hhsize[hhsize$hhsize >= 30] <- NA
hhsize$hhsize[hhsize$hhsize == 0] <- NA
milk2012 <- merge(milk2012, hhsize[c("HHID","hhsize")])
milk2012$milk_cap <- (milk2012$totmilk/milk2012$hhsize)*52.177457

### merge in weights and location
loc <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS2012_13/GSEC1cln.dta")[c("HHID","sregion","urban","wgt")]
milk2012 <- merge(milk2012,loc, by="HHID", all.y=T)
milk2012$milk_cap[is.na(milk2012$milk_cap)] <- 0
milk2012$milk_cap[milk2012$milk_cap > 800] <- 0
weighted.mean(milk2012$milk_cap, milk2012$wgt)
unlist(lapply(split(milk2012, milk2012$urban), function(z) weighted.mean(z$milk_cap, z$wgt)))



test_data <-
  data.frame(
   liters = c(9.496439,12.06651, 19.93746, 10.284458, 28.72254 ,32.59935),
    rur_urb = c("rural","rural","rural","urban","urban","urban" ),
    time = c("2005","2009","2012","2005","2009","2012")
  )
pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/consumption.pdf")
ggplot(data=test_data, aes(x=time, y=liters, group=rur_urb, colour=rur_urb)) +
    geom_line(size=3) +
    geom_point(size=6) +  theme_bw()+theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) + ylim(0,35)  + theme(legend.title=element_blank()) + theme(legend.text=element_text(size=18))  + scale_colour_grey(start = .2, end = .8)
dev.off()

pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/consumption_pres.pdf")
ggplot(data=test_data, aes(x=time, y=liters, group=rur_urb, colour=rur_urb)) +
    geom_line(size=3) +
    geom_point(size=6)+theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold")) + ylim(0,35)  + theme(legend.title=element_blank()) + theme(legend.text=element_text(size=30))  
dev.off()

### consumption as reported by farmers in the survey
farmers <- read.csv("/home/bjvca/data/projects/PIMDVC/data/public/farmers.csv")
 summary(as.numeric(as.character(farmers$hh_head.HH.consumption.milk_value)))
 summary(as.numeric(as.character(farmers$hh_head.HH.consumption.bongo_value)))



#### using UNPS

###2013-2014
secA <- read.csv("/home/bjvca/data/data/UGANDA/UNPS_2013_14/gsec15b.csv")
secA[is.na(secA)] <- 0
milk <- subset(secA ,itmcd==125)
milk <- subset(milk,untcd == 3 | untcd ==  32 | milk$untcd==107 | milk$untcd==108)
milk$conv <- 1

milk$conv[milk$untcd==32] <- .5

milk$conv[milk$untcd==107 | milk$untcd==108 ] <- .6

milk$totmilk <-  (milk$h15bq4 + milk$h15bq6+ milk$h15bq8 + milk$h15bq10)*milk$conv
milk$totmilk[milk$totmilk >50 ] <- NA   

milk_hh <- aggregate(milk$totmilk,list(milk$HHID), sum)
names(milk_hh) <- c("HHID","totmilk")

sec1 <- read.csv("/home/bjvca/data/data/UGANDA/UNPS_2013_14/gsec1.csv")
milk2013 <- merge(sec1,milk_hh, by="HHID", all.x=T)
milk2013$totmilk[is.na(milk2013$totmilk)] <- 0
weighted.mean(milk2013$totmilk, milk2013$wgt_X)*52.177457/6
unlist(lapply(split(milk2013, milk2013$urban), function(z) weighted.mean(z$totmilk, z$wgt_X))) *52.177457/6

###2005-05
secA <- read.dta("/home/bjvca/data/data/UGANDA/UNPS2005/UNHS05_HH_DTA/GSEC14A.dta")
secA[is.na(secA)] <- 0
milk <- subset(secA,h14aq2=="fresh milk")
#keep only liters and cup/mug
milk <- subset(milk, h14aq3 == "Liter" | h14aq3=="Cup/Mug (0.5 lt)")
milk$conv <- 1

milk$conv[milk$h14aq3=="Cup/Mug (0.5 lt)"] <- .5
milk$totmilk <-  (milk$h14aq4 + milk$h14aq6 + milk$h14aq8 +  milk$h14aq10)*milk$conv
milk$totmilk[milk$totmilk >50 ] <- NA   

milk_hh <- aggregate(milk$totmilk,list(milk$HHID), sum)
names(milk_hh) <- c("HHID","totmilk")

sec1 <- read.dta("/home/bjvca/data/data/UGANDA/UNPS2005/UNHS05_HH_DTA/GSEC1.dta")
milk2005 <- merge(sec1,milk_hh, by="HHID", all.x=T)
milk2005$totmilk[is.na(milk2005$totmilk)] <- 0
weighted.mean(milk2005$totmilk, milk2005$hmult)*52.177457/6
unlist(lapply(split(milk2005, milk2005$substrat), function(z) weighted.mean(z$totmilk, z$hmult))) *52.177457/6

###2009-10
secA <- read.dta("/home/bjvca/data/projects/GAP/Haruna/UNPS_2009_10/in/GSEC15b.dta")
secA[is.na(secA)] <- 0
milk <- subset(secA,itmcd=="Fresh Milk")
#keep only liters and cup/mug
milk <- subset(milk, untcd == 3 | untcd==32)
milk$conv <- 1

milk$conv[milk$untcd==32] <- .5
milk$totmilk <-  (milk$h15bq4  + milk$h15bq6  + milk$h15bq8 +  milk$h15bq10)*milk$conv
milk$totmilk[milk$totmilk >50 ] <- NA   

milk_hh <- aggregate(milk$totmilk,list(milk$hh), sum)
names(milk_hh) <- c("HHID","totmilk")

sec1 <- read.dta("/home/bjvca/data/projects/GAP/Haruna/UNPS_2009_10/in/GSEC1.dta")
milk2009 <- merge(sec1,milk_hh, by="HHID", all.x=T)
milk2009$totmilk[is.na(milk2009$totmilk)] <- 0
milk2009 <- subset(milk2009, !is.na(wgt09))
weighted.mean(milk2009$totmilk, milk2009$wgt09)*52.177457/6
unlist(lapply(split(milk2009, milk2009$urban), function(z) weighted.mean(z$totmilk, z$wgt09))) *52.177457/6

test_data <-
  data.frame(
   liters = c(7.522380,16.82576, 18.84406, 6.500355, 21.85333, 20.98074),
    rur_urb = c("rural","rural","rural","urban","urban","urban" ),
    time = c("2005/06","2009/10","2013/14","2005/06","2009/10","2013/14")
  )
pdf("/home/bjvca/data/projects/PIMDVC/presentations/consumption.pdf")
 

ggplot(data=test_data, aes(x=time, y=liters, group=rur_urb, colour=rur_urb)) +
    geom_line(size=1.5) +
    geom_point() + theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))  + ylim(0, 25)
dev.off()


