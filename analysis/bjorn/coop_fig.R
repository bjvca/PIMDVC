library(ggplot2)
charts.data <- read.csv("/home/bjvca/data/projects/PIMDVC/analysis/bjorn/coop_table.csv")
charts.data$name <- factor(charts.data$name)
charts.data$percentage <- charts.data$share*100
charts.data <- subset(charts.data, name != "Trader supplies to cooperative mcc?")
charts.data$name <- factor(charts.data$name, levels=rev(c("Farmer is member of a cooperative?","Trader is member of cooperative or union?","Mcc is cooperative?")) )
library(plyr)
charts.data$location <- revalue(charts.data$location , c("central"="Central", "southwest"="Southwest"))

p4 <- ggplot() + theme_bw() + geom_bar(aes(y = percentage, x = name, fill = location), data = charts.data, stat="identity", position="dodge")  + coord_flip() + scale_fill_grey(start = .2, end = .6) +theme_grey(base_size = 14) + theme(axis.title.y=element_blank())
p4

ggsave("/home/bjvca/data/projects/PIMDVC/analysis/bjorn/coop.pdf", p4,   width = 20, height = 6, units = "cm")
