
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

res[1,] <- prop.table(table(mccs$mcc.q52.a, mccs$coop), margin=2)[2,]
res[2,] <- prop.table(table(mccs$mcc.q52.b, mccs$coop), margin=2)[2,]
res[3,] <-  prop.table(table(mccs$mcc.q52.c, mccs$coop), margin=2)[2,]
res[4,] <- prop.table(table(mccs$mcc.q52.d, mccs$coop), margin=2)[2,]
res[5,] <- prop.table(table(mccs$mcc.q52.e, mccs$coop), margin=2)[2,]
res[6,] <-  prop.table(table(mccs$mcc.q52.f, mccs$coop), margin=2)[2,]
res[7,] <-  prop.table(table(mccs$mcc.q52.g, mccs$coop), margin=2)[2,]
res[8,] <-  prop.table(table(mccs$mcc.q52.j, mccs$coop), margin=2)[2,]

services <- c("Training on milk production",
"Training on milk hygiene/quality",
"Credit/loans to suppliers",
"Feed to suppliers",
"Milk cans to suppliers",
"Offer veterinary services",
"Offer transport services",

"Supply medicines, vaccinations,...")

res <- data.frame(services, res)

names(res) <- c("services","private","cooperative")




res_m <- melt(res)

names(res_m) <-  c("services","ownership","percent")
pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/services.pdf")
ggplot(data=res_m, aes(x=reorder(services,percent), y=percent, fill=ownership)) +
geom_bar(stat="identity", position=position_dodge()) + coord_flip() +  theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 12)) + theme(text = element_text(size = 12)) + theme(axis.title.y=element_blank()) + theme(legend.text=element_text(size=12))
dev.off()

##redo for presentation

res <- matrix(NA,5,2)

res[1,] <- prop.table(table(mccs$mcc.q52.a, mccs$coop), margin=2)[2,]
res[2,] <- prop.table(table(mccs$mcc.q52.b, mccs$coop), margin=2)[2,]
res[3,] <-  prop.table(table(mccs$mcc.q52.c, mccs$coop), margin=2)[2,]

res[4,] <- prop.table(table(mccs$mcc.q52.e, mccs$coop), margin=2)[2,]


res[5,] <-  prop.table(table(mccs$mcc.q52.j, mccs$coop), margin=2)[2,]

services <- c("Training on milk production",
"Training on milk hygiene/quality",
"Credit/loans to suppliers",

"Milk cans to suppliers",



"Supply medicines, vaccinations,...")

res <- data.frame(services, res)

names(res) <- c("services","private","cooperative")




res_m <- melt(res)
names(res_m) <-  c("services","ownership","percent")
ggplot(data=res_m, aes(x=reorder(services,percent), y=percent, fill=ownership)) +
geom_bar(stat="identity", position=position_dodge()) + coord_flip() +  theme(axis.text = element_text(size = 30))+ theme(axis.title = element_text(size = 25)) + theme(text = element_text(size = 25)) + theme(axis.title.y=element_blank()) + theme(legend.text=element_text(size=25))

