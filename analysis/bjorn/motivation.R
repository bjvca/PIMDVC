#milk consumption in 2005/06
library(foreign) 
library(ggplot2)
rm(list=ls())

#food, beverages and tobacco
secA <- read.csv("/home/bjvca/data/projects/PIMDVC/analysis/bjorn/cons_2005.csv")
secA[is.na(secA)] <- 0
milk <- subset(secA,h14aq2=="fresh milk")
#milk[is.na(milk)] <- 0
#keep only liters and cup/mug

milk$conv <- NA

milk$conv[milk$h14aq3=="Cup/Mug (0.5 lt)"] <- .5
milk$conv[milk$h14aq3=="Litre"] <- 1
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
#milk2005 <- merge(milk2005,loc, by="hh")
milk2005$milk_cap[is.na(milk2005$milk_cap)] <- 0
#milk2005$milk_cap[milk2005$milk_cap > 800] <- 0
weighted.mean(milk2005$milk_cap, milk2005$hmult)
unlist(lapply(split(milk2005, milk2005$substrat), function(z) weighted.mean(z$milk_cap, z$hmult)))

###2009_10
#food, beverages and tobacco
secA <- read.csv("/home/bjvca/data/projects/PIMDVC/analysis/bjorn/cons_2009.csv")
secA[is.na(secA)] <- 0
milk <- subset(secA,itmcd=="Fresh Milk")
#keep only liters and cup/mug

milk$conv <- NA

milk$conv[milk$untcd=="GLASS (0.25)"] <- .25
milk$conv[milk$untcd=="LITRE"] <- 1
milk$conv[milk$untcd=="CUP/ MUG (0.5)"] <- .5
milk$conv[milk$h14aq3=="JERRICANS (5 LITRES)"] <- 5
milk$conv[milk$h14aq3=="JERRICANS (1 LITRES)"] <- 1
milk$conv[milk$h14aq3=="BOTTLE (500ML)"] <- .5
milk$conv[milk$h14aq3=="BOTTLE (350ML)"] <- .35          
milk$conv[milk$h14aq3=="BOTTLE (300ML)" ] <- .3


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
#milk2009 <- merge(milk2009,loc, by="hh")
milk2009$milk_cap[is.na(milk2009$milk_cap)] <- 0
#milk2009$milk_cap[milk2009$milk_cap > 800] <- 0
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

milk$conv[milk$untcd==32] <- .5
milk$conv[milk$untcd==3] <- 1
milk$conv[milk$untcd==14] <- 20
milk$conv[milk$untcd==15] <- 10
milk$conv[milk$untcd==16] <- 5
milk$conv[milk$untcd==23] <- .75
milk$conv[milk$untcd==24] <- .5
milk$conv[milk$untcd==26] <- .3
milk$conv[milk$untcd==33] <-.25
milk$conv[milk$untcd==54] <- 1
milk$conv[milk$untcd==71] <- 2.5
milk$conv[milk$untcd==79] <- 1




milk$totmilk <-  (milk$ceb06 + milk$ceb08 + milk$ceb10 + milk$ceb12)*milk$conv
#milk$totmilk[milk$totmilk >50 ] <- NA   
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
#milk2012$milk_cap[milk2012$milk_cap > 800] <- 0
weighted.mean(milk2012$milk_cap, milk2012$wgt)
unlist(lapply(split(milk2012, milk2012$urban), function(z) weighted.mean(z$milk_cap, z$wgt)))


###2002
secA <- read.csv("/home/bjvca/data/projects/PIMDVC/analysis/bjorn/cons_2002.csv")

secA[is.na(secA)] <- 0
milk <- subset(secA ,s6aq2==125)

milk$conv <- NA

milk$conv[milk$s6aq3==32] <- .5
milk$conv[milk$s6aq3==3] <- 1
milk$conv[milk$s6aq3==14] <- 20
milk$conv[milk$s6aq3==15] <- 10
milk$conv[milk$s6aq3==16] <- 5
milk$conv[milk$s6aq3==23] <- .75
milk$conv[milk$s6aq3==24] <- .5
milk$conv[milk$s6aq3==26] <- .3
milk$conv[milk$s6aq3==33] <-.25
milk$conv[milk$s6aq3==54] <- 1
milk$conv[milk$s6aq3==71] <- 2.5
milk$conv[milk$s6aq3==79] <- 1



milk$totmilk <-  (milk$s6aq4 + milk$s6aq6 + milk$s6aq8 + milk$s6aq10)*milk$conv
#milk$totmilk[milk$totmilk >50 ] <- NA   
milk2002  <- milk


### per capita 
hhsize <- data.frame(read.spss("/home/bjvca/data/data/UGANDA/UNHS/UNHS_2002/socio-spss/sec6.sav"))
hhsize[c("S6Q1", "S6Q2", "S6Q3", "S6Q4", "S6Q5", "S6Q6")] <- lapply(hhsize[c("S6Q1", "S6Q2", "S6Q3", "S6Q4", "S6Q5", "S6Q6")], function(x) as.numeric(as.character(x)) )
hhsize <- data.frame(hhsize$HH,rowSums(hhsize[c("S6Q1", "S6Q2", "S6Q3", "S6Q4", "S6Q5", "S6Q6")], na.rm=T))

hhsize <- data.frame(read.csv("/home/bjvca/data/projects/PIMDVC/analysis/bjorn/hhcomp_2002.csv")[,2],rowSums(read.csv("/home/bjvca/data/projects/PIMDVC/analysis/bjorn/hhcomp_2002.csv")[,3:8], na.rm=T))
names(hhsize) <- c("HHID","hhsize")
hhsize$hhsize[hhsize$hhsize >= 30] <- NA
hhsize$hhsize[hhsize$hhsize == 0] <- NA
milk2002 <- merge(milk2002, hhsize[c("HHID","hhsize")],by.x="hh", by.y="HHID")
milk2002$milk_cap <- (milk2002$totmilk/milk2002$hhsize)*52.177457

### merge in weights and location
loc <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS_2002/socio/sec1.dta")[c("hh","urban","mult")]
milk2002 <- merge(milk2002,loc, by="hh", all.y=T)
milk2002$milk_cap[is.na(milk2002$milk_cap)] <- 0
#milk2012$milk_cap[milk2012$milk_cap > 800] <- 0
weighted.mean(milk2002$milk_cap, milk2002$mult)
unlist(lapply(split(milk2002, milk2002$urban), function(z) weighted.mean(z$milk_cap, z$mult)))



test_data <-
  data.frame(
   liters = c(12.64,21.73,12.06651, 21.05,10.78, 28.62, 28.72254 ,33.24),
    rur_urb = c("rural","rural","rural","rural","urban","urban","urban","urban" ),
    time = c("2002","2005","2009","2012","2002","2005","2009","2012")
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

milk <- subset(milk,untcd == 3 | untcd ==  32 | milk$untcd==107 | milk$untcd==108 |  milk$untcd==108 )
milk$conv <- 1

milk$conv[milk$untcd==32] <- .5
milk$conv[milk$untcd==107] <- 0.58/1.04
milk$conv[milk$untcd==108] <- 0.45/1.04
milk$conv[milk$untcd==109] <- 0.35/1.04


milk$totmilk <-  (milk$h15bq4 + milk$h15bq6+ milk$h15bq8 + milk$h15bq10)*milk$conv
#milk$totmilk[milk$totmilk >50 ] <- NA   
milk2014  <- milk

### per capita
hhsize <- data.frame(read.csv("/home/bjvca/data/data/UGANDA/UNPS_2013_14/gsec15.csv")[,1],rowSums(read.csv("/home/bjvca/data/data/UGANDA/UNPS_2013_14/gsec15.csv")[,2:9], na.rm=T))
names(hhsize) <- c("hh","hhsize")
hhsize$hhsize[hhsize$hhsize >= 30] <- NA
milk2014 <- merge(milk2014, hhsize,by.x="HHID", by.y="hh")
milk2014$milk_cap <- (milk2014$totmilk/milk2014$hhsize)*52.177457
milk2014$wgt_X <- NULL

milk2014$milk_cap[is.infinite(milk2014$milk_cap)] <- NA

sec1 <- read.csv("/home/bjvca/data/data/UGANDA/UNPS_2013_14/gsec1.csv")
milk2014 <- merge(sec1,milk2014, by="HHID", all.x=T)
milk2014$milk_cap[is.na(milk2014$milk_cap)] <- 0
weighted.mean(milk2014$milk_cap, milk2014$wgt_X)
unlist(lapply(split(milk2014, milk2014$urban), function(z) weighted.mean(z$milk_cap, z$wgt_X))) 

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



test_data <-
  data.frame(
   usd = c(2,3,5,10,16,18,20,22, 42, 60, 130),
    time =as.character(seq(2007,2017,1))
  )
pdf("/home/bjvca/data/projects/PIMDVC/presentations/exports.pdf")
 

ggplot(data=test_data, aes(x=time, y=usd, group=1)) +
    geom_line(size=3) +
    geom_point() + theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) + scale_y_continuous(name="Million USD")
dev.off()



