rm(list=ls())
library(ggplot2)
library(ggridges)
library(reshape2)
mccs <- read.csv("/home/bjvca/data/projects/PIMDVC/data/public/mccs.csv")

mccs$average_collected <-  as.numeric(as.character(mccs$mcc.q42))

mccs$average_collected[mccs$average_collected==999] <- NA
mccs$average_collected[mccs$average_collected>10000] <- NA

mccs$coop <- mccs$mcc.q8 ==3 | mccs$mcc.q8 ==4 


mccs$always_buys <- mccs$mcc.q53.a
mccs$good_price <- mccs$mcc.q53.b
mccs$gives_loans <- mccs$mcc.q53.c
mccs$transport <- mccs$mcc.q53.d
mccs$training <- mccs$mcc.q53.e
mccs$no_payment_delays <- mccs$mcc.q53.f


to_plot <- mccs[c("q1","coop", "always_buys","good_price","gives_loans", "transport","training","no_payment_delays" )]

to_plot_means <- data.frame(c(colMeans(to_plot[,3:8], na.rm=T), colMeans(to_plot[to_plot$coop==T,3:8], na.rm=T), colMeans(to_plot[to_plot$coop==F,3:8], na.rm=T)))
to_plot_means$reason <- names(c(colMeans(to_plot[,3:8], na.rm=T), colMeans(to_plot[to_plot$coop==T,3:8], na.rm=T), colMeans(to_plot[to_plot$coop==F,3:8], na.rm=T)))

to_plot_means$type <- "all"
to_plot_means$type[7:12] <- "coop"
to_plot_means$type[13:18] <- "private"
names(to_plot_means)[1] <- "percentage"
to_plot_means$percentage <- to_plot_means$percentage * 100

to_plot_means$reason <- ordered(to_plot_means$reason,levels = c("always_buys","good_price","gives_loans", "transport","training","no_payment_delays" ))


pdf("/home/bjvca/data/projects/PIMDVC/presentations/mcc_reason.pdf")
ggplot(to_plot_means, aes(reason, percentage)) +   
  geom_point(aes(color = type), position = "dodge", stat="identity",size =5)  +
  coord_flip() + theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) + ylim(0,100)
dev.off()

mccs$mcc.q39[mccs$mcc.q39==999] <- NA
mccs$mcc.q40[mccs$mcc.q40==999] <- NA
tapply(mccs$mcc.q39,mccs$shed,sd,na.rm=T)
## standard error is higher in central region - this is the relevant price for market participationd decsions in the last 7 days
tapply(mccs$mcc.q40,mccs$shed,sd,na.rm=T)
#use an F test to compare variances
var.test(mccs$mcc.q40~mccs$shed)


df <- data.frame(cbind(c(mccs$mcc.q39,mccs$mcc.q40),c(mccs$shed,mccs$shed)))
names(df) <- c("price","shed")
tapply(df$price,df$shed,sd,na.rm=T)

tapply(mccs$mcc.q39,mccs$shed,sd, na.rm=T)
tapply(mccs$mcc.q40,mccs$shed,sd, na.rm=T)

mccs$av_price <- (mccs$mcc.q39 + mccs$mcc.q40)/2
##prices
mccs$mcc.q39[mccs$mcc.q39==999] <- NA
mccs$mcc.q40[mccs$mcc.q40==999] <- NA


mccs$mcc.q24[mccs$mcc.q24>100 | mccs$mcc.q24<=0] <- NA
mccs$mcc.q25[mccs$mcc.q25>100 | mccs$mcc.q24<=0] <- NA


tapply(mccs$mcc.q24, mccs$shed, mean, na.rm=T)
tapply(mccs$mcc.q25, mccs$shed, mean, na.rm=T)

tapply(mccs$mcc.q39, mccs$shed, mean, na.rm=T)
tapply(mccs$mcc.q40, mccs$shed, mean, na.rm=T)

res <- matrix(NA,8,2)

res[1,] <- prop.table(table(mccs$mcc.q52.a, mccs$shed), margin=2)[2,]
res[2,] <- prop.table(table(mccs$mcc.q52.b, mccs$shed), margin=2)[2,]
res[3,] <-  prop.table(table(mccs$mcc.q52.c, mccs$shed), margin=2)[2,]
res[4,] <- prop.table(table(mccs$mcc.q52.d, mccs$shed), margin=2)[2,]
res[5,] <- prop.table(table(mccs$mcc.q52.e, mccs$shed), margin=2)[2,]
res[6,] <-  prop.table(table(mccs$mcc.q52.f, mccs$shed), margin=2)[2,]
res[7,] <-  prop.table(table(mccs$mcc.q52.g, mccs$shed), margin=2)[2,]
res[8,] <-  prop.table(table(mccs$mcc.q52.j, mccs$shed), margin=2)[2,]

services <- c("Training on milk production",
"Training on milk hygiene/quality",
"Credit/loans to suppliers",
"Feed to suppliers",
"Milk cans to suppliers",
"Offer veterinary services",
"Offer transport services",

"Supply medicines, vaccinations,...")

res <- data.frame(services, res)

names(res) <- c("services","Central","Southwest")


prop.test(table(mccs$mcc.q52.a, mccs$shed))
prop.test(table(mccs$mcc.q52.b, mccs$shed))
prop.test(table(mccs$mcc.q52.c, mccs$shed))
prop.test(table(mccs$mcc.q52.d, mccs$shed))
prop.test(table(mccs$mcc.q52.e, mccs$shed))
prop.test(table(mccs$mcc.q52.f, mccs$shed))
prop.test(table(mccs$mcc.q52.g, mccs$shed))
prop.test(table(mccs$mcc.q52.j, mccs$shed))



res_m <- melt(res)
res_m$value <- res_m$value*100

names(res_m) <-  c("services","location","percent")
pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/services_annoted.pdf")
ggplot(data=res_m, aes(x=reorder(services,percent), y=percent, fill=location)) +
geom_bar(stat="identity", position=position_dodge()) + ylim(0,100) +coord_flip() +  theme_bw() +theme(axis.text = element_text(size = 14))+ theme(axis.title = element_text(size = 14)) + theme(text = element_text(size = 14)) + theme(axis.title.y=element_blank()) + theme(legend.text=element_text(size=12)) +  scale_fill_grey(start = .2, end = .7) 
dev.off()

### milk cans and jerrycans
mccs$mcc.q44[mccs$mcc.q44==999] <- NA
mccs$mcc.q45[mccs$mcc.q45==999] <- NA
tapply(mccs$mcc.q44,mccs$shed, mean, na.rm=T)
prop.table(table(mccs$mcc.q44>0 & mccs$mcc.q45==0 ,mccs$shed),2)

mccs$only_milk_cans <- mccs$mcc.q44>0 & mccs$mcc.q45==0




mccs$export_ind <- (mccs$mcc.q30==1 | mccs$mcc.q30==2) | (mccs$mcc.q29.3 | mccs$mcc.q29.4 ) 
 prop.table(table(mccs$export_ind, mccs$shed),2)


res <- matrix(NA,8,2)

res[1,] <- prop.table(table(mccs$mcc.q52.a, mccs$export_ind), margin=2)[2,]
res[2,] <- prop.table(table(mccs$mcc.q52.b, mccs$export_ind), margin=2)[2,]
res[3,] <-  prop.table(table(mccs$mcc.q52.c, mccs$export_ind), margin=2)[2,]
res[4,] <- prop.table(table(mccs$mcc.q52.d, mccs$export_ind), margin=2)[2,]
res[5,] <- prop.table(table(mccs$mcc.q52.e, mccs$export_ind), margin=2)[2,]
res[6,] <-  prop.table(table(mccs$mcc.q52.f, mccs$export_ind), margin=2)[2,]
res[7,] <-  prop.table(table(mccs$mcc.q52.g, mccs$export_ind), margin=2)[2,]
res[8,] <-  prop.table(table(mccs$mcc.q52.j, mccs$export_ind), margin=2)[2,]

services <- c("Training on milk production",
"Training on milk hygiene/quality",
"Credit/loans to suppliers",
"Feed to suppliers",
"Milk cans to suppliers",
"Offer veterinary services",
"Offer transport services",

"Supply medicines, vaccinations,...")

res <- data.frame(services, res)

names(res) <- c("services","local","export")

##increase in capacity
mccs$mcc.q36[mccs$mcc.q36 == 999] <- NA
mccs$mcc.q36[mccs$mcc.q36 == 40000] <- 4000
mccs$mcc.q36[mccs$mcc.q36 == 12000] <- 1200
mccs$mcc.q36[mccs$mcc.q36 == 50000] <- 5000
mccs$mcc.q36[mccs$mcc.q36 == 3408] <- 3400
mccs$year_start <- as.numeric(as.character(substr(mccs$mcc.q35,5,8)))
mccs$age <- 2018 - mccs$year_start
mccs$mcc.q36[mccs$age ==0] <- NA
mccs$yearly_increase <- NA
mccs$yearly_increase[mccs$age<=10] <- (mccs$mcc.q23[mccs$age<=10]- mccs$mcc.q36[mccs$age<=10])/mccs$age[mccs$age<=10]
t.test(mccs$yearly_increase~mccs$shed)



summary(lm(yearly_increase~export_ind,data=mccs))
summary(lm(coop~export_ind,data=mccs))
summary(lm(mcc.q52.a~export_ind,data=mccs))
summary(lm(mcc.q52.b~export_ind,data=mccs))
summary(lm(mcc.q52.c~export_ind,data=mccs))
summary(lm(mcc.q52.d~export_ind,data=mccs))
summary(lm(mcc.q52.e~export_ind,data=mccs))
summary(lm(mcc.q52.f~export_ind,data=mccs))
summary(lm(mcc.q52.g~export_ind,data=mccs))
summary(lm(mcc.q52.j~export_ind,data=mccs))
summary(lm(only_milk_cans~export_ind,data=mccs))

mean(mccs$yearly_increase[mccs$export_ind==F], na.rm=T)
mean(mccs$coop[mccs$export_ind==F], na.rm=T)
mean(mccs$mcc.q52.a[mccs$export_ind==F], na.rm=T)
mean(mccs$mcc.q52.b[mccs$export_ind==F], na.rm=T)
mean(mccs$mcc.q52.c[mccs$export_ind==F], na.rm=T)
mean(mccs$mcc.q52.d[mccs$export_ind==F], na.rm=T)
mean(mccs$mcc.q52.e[mccs$export_ind==F], na.rm=T)
mean(mccs$mcc.q52.f[mccs$export_ind==F], na.rm=T)
mean(mccs$mcc.q52.g[mccs$export_ind==F], na.rm=T)
mean(mccs$mcc.q52.j[mccs$export_ind==F], na.rm=T)
mean(mccs$only_milk_cans[mccs$export_ind==F], na.rm=T)

mccs$subcenter <- mccs$mcc.q9=="Yes"

mccs$capacity <- mccs$mcc.q23
r0 <-lm(yearly_increase~export_ind+age+capacity + subcenter+ shed , data=mccs)
r1 <-lm(coop~export_ind+age+capacity + subcenter+ shed , data=mccs)
r2 <-lm(mcc.q52.a~export_ind+age+capacity + subcenter+ shed , data=mccs)
r3 <-lm(mcc.q52.b~export_ind+age+capacity + subcenter+ shed , data=mccs)
r4 <-lm(mcc.q52.c~export_ind+age+capacity + subcenter+ shed , data=mccs)
r5 <-lm(mcc.q52.d~export_ind+age+capacity + subcenter+ shed , data=mccs)
r6 <-lm(mcc.q52.e~export_ind+age+capacity + subcenter+ shed , data=mccs)
r7 <-lm(mcc.q52.f~export_ind+age+capacity + subcenter+ shed , data=mccs)
r8 <-lm(mcc.q52.g~export_ind+age+capacity + subcenter+ shed , data=mccs)
r9 <-lm(mcc.q52.j~export_ind+age+capacity + subcenter+ shed , data=mccs)
r10 <-lm(only_milk_cans~export_ind+age+capacity + subcenter+ shed , data=mccs)
library(stargazer)
stargazer(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9)


library(MatchIt)

set.seed(12345) #matchit randomly picks one of the control subjects that falls within the caliper interval around the treated subject, so set seed to be able to replicate
mccs_cpy <- mccs
mccs <- mccs[complete.cases(mccs[c("ID","export_ind","age","capacity","subcenter","shed")]),]
mccs <- mccs[c("ID","export_ind","age","capacity","subcenter","shed")]
## we have less controls that treatment, turn around so we can match multiple treated to control
mccs$export_ind <- !(mccs$export_ind)
match.it <- matchit(export_ind~age+capacity+subcenter + shed, data =mccs,caliper=.05,method="nearest",ratio=2)
a <- summary(match.it)


df.match <- match.data(match.it)[1:ncol(mccs)]
df.match$export_ind <- !df.match$export_ind   #turn back
### can not use paired t.test here because of 1:2 matching, so just use regressions
summary(lm(coop~export_ind, data = merge(df.match,mccs_cpy[c("ID","coop")])))
summary(lm(mcc.q52.a~export_ind,data= merge(df.match,mccs_cpy[c("ID","mcc.q52.a")])))
summary(lm(mcc.q52.b~export_ind,data= merge(df.match,mccs_cpy[c("ID","mcc.q52.b")])))
summary(lm(mcc.q52.c~export_ind,data= merge(df.match,mccs_cpy[c("ID","mcc.q52.c")])))
summary(lm(mcc.q52.d~export_ind,data= merge(df.match,mccs_cpy[c("ID","mcc.q52.d")])))
summary(lm(mcc.q52.e~export_ind,data= merge(df.match,mccs_cpy[c("ID","mcc.q52.e")])))
summary(lm(mcc.q52.f~export_ind,data= merge(df.match,mccs_cpy[c("ID","mcc.q52.f")])))
summary(lm(mcc.q52.g~export_ind,data= merge(df.match,mccs_cpy[c("ID","mcc.q52.g")])))
summary(lm(mcc.q52.j~export_ind,data= merge(df.match,mccs_cpy[c("ID","mcc.q52.j")])))
summary(lm(only_milk_cans~export_ind,data= merge(df.match,mccs_cpy[c("ID","only_milk_cans")])))

res_m <- melt(res)
names(res_m) <-  c("services","ownership","percent")


ggplot(data=res_m, aes(x=reorder(services,percent), y=percent, fill=ownership)) +
geom_bar(stat="identity", position=position_dodge()) + coord_flip() +  theme_bw() +theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 12)) + theme(text = element_text(size = 12)) + theme(axis.title.y=element_blank()) + theme(legend.text=element_text(size=12)) +  scale_fill_grey(start = .2, end = .7)

mccs <- mccs_cpy
summary(lm(mccs$mcc.q52.a~export_ind, data=mccs))
## when did this mcc start operating
mccs$year_start <- as.numeric(as.character(substr(mccs$mcc.q35,5,8)))
to_plot <- melt(prop.table(table(mccs$year_start,mccs$shed),2))
to_plot <- to_plot[to_plot$Var1 %in% 2007:2018,]
to_plot$Var1 <- factor(to_plot$Var1)
library(plyr)
to_plot$Var2 <- revalue(to_plot$Var2 , c("C"="Central", "SW"="Southwest"))


pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/evo_mccs.pdf")
ggplot(data=to_plot,aes(x=Var1,y=value, group=Var2)) +geom_point(aes(color=Var2), size=3)+geom_smooth(aes(color=Var2),se=F,size=3) +  theme_bw()  + xlab("Year") + ylab("share")+ labs(color="Milk Shed") +theme(axis.text=element_text(size=14),  axis.title=element_text(size=14,face="bold"))  + scale_colour_grey(start = .2, end = .8) + theme(axis.text.x = element_text(angle = 90)) +  theme(legend.text=element_text(size=14))  + theme(legend.title=element_blank())
dev.off()

mcc.q52.a
