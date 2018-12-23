mccs <- read.csv("/home/bjvca/data/projects/PIMDVC/data/raw/MCCs.csv")

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
