rm(list=ls())
library(ggplot2)
library(ggridges)
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

farmers <- read.csv("/home/bjvca/data/projects/PIMDVC/data/public/farmers.csv")
## herd size now
farmers[c("hh_head.HH.cattle_ownership.q23", "hh_head.HH.cattle_ownership.q25","hh_head.HH.cattle_ownership.q27","hh_head.HH.cattle_ownership.q29", "hh_head.HH.cattle_ownership.q31","hh_head.HH.cattle_ownership.q33")] <- lapply(farmers[c("hh_head.HH.cattle_ownership.q23", "hh_head.HH.cattle_ownership.q25","hh_head.HH.cattle_ownership.q27","hh_head.HH.cattle_ownership.q29", "hh_head.HH.cattle_ownership.q31","hh_head.HH.cattle_ownership.q33")], function(x) replace(x, x == 999, NA) )
farmers$herdsize <- rowSums(farmers[c("hh_head.HH.cattle_ownership.q23", "hh_head.HH.cattle_ownership.q25","hh_head.HH.cattle_ownership.q27","hh_head.HH.cattle_ownership.q29", "hh_head.HH.cattle_ownership.q31","hh_head.HH.cattle_ownership.q33")], na.rm=T)
farmers$herdsize_exot <- rowSums(farmers[c("hh_head.HH.cattle_ownership.q23", "hh_head.HH.cattle_ownership.q25","hh_head.HH.cattle_ownership.q27")], na.rm=T)

farmers$herdsize_local <- rowSums(farmers[c("hh_head.HH.cattle_ownership.q29", "hh_head.HH.cattle_ownership.q31","hh_head.HH.cattle_ownership.q33")], na.rm=T)



## herd size 10 years ago

farmers$hh_head.HH.recall.q234 <- as.numeric(as.character(farmers$hh_head.HH.recall.q234))
farmers$hh_head.HH.recall.q235 <- as.numeric(as.character(farmers$hh_head.HH.recall.q235))
farmers[c("hh_head.HH.recall.q234","hh_head.HH.recall.q235")] <- lapply(farmers[c("hh_head.HH.recall.q234","hh_head.HH.recall.q235")] , function(x) replace(x, x == 999, NA) )
farmers$herdsize_b <- rowSums(farmers[c("hh_head.HH.recall.q234","hh_head.HH.recall.q235")] , na.rm=T)
farmers$herdsize_exot_b <- as.numeric(as.character(farmers$hh_head.HH.recall.q234))
farmers$herdsize_local_b <- as.numeric(as.character(farmers$hh_head.HH.recall.q235))

##change in herd size
 mean(farmers$herdsize_exot_b, na.rm=T)
 mean(farmers$herdsize_exot, na.rm=T)

 mean(farmers$herdsize_local_b, na.rm=T)
 mean(farmers$herdsize_local, na.rm=T)

res <- matrix(NA,4,3)
res[1,1] <- mean(farmers$herdsize_exot_b, na.rm=T)
res[2,1] <-  mean(farmers$herdsize_exot, na.rm=T)

 res[3,1] <- mean(farmers$herdsize_local_b, na.rm=T)
res[4,1] <-  mean(farmers$herdsize_local, na.rm=T)
res <- data.frame(res)
res[c(1,3),2] <- "10 years ago"
res[c(2,4),2] <- "now"
res[c(1,2),3] <- "exotics"
res[c(3,4),3] <- "local"
names(res) <- c("number","time","type")
pdf("/home/bjvca/data/projects/PIMDVC/presentations/herd_time.pdf")
ggplot(res, aes(x = time, y = number, fill = type)) + 
  geom_bar(stat = "identity")  + theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))  + theme(legend.text=element_text(size=14))
dev.off()

farmers$hh_head.HH.distance.q11[farmers$hh_head.HH.distance.q11==999] <- NA
farmers$hh_head.HH.distance.q9[farmers$hh_head.HH.distance.q9==999] <- NA
farmers$hh_head.HH.distance.q10[farmers$hh_head.HH.distance.q10>900] <- NA
farmers$hh_head.HH.distance.q14[farmers$hh_head.HH.distance.q14>900] <- NA
farmers$hh_head.HH.distance.q15[farmers$hh_head.HH.distance.q15>900] <- NA
farmers$hh_head.HH.distance.q12[farmers$hh_head.HH.distance.q12>900] <- NA
farmers$hh_head.HH.distance.q16[farmers$hh_head.HH.distance.q16>900] <- NA

p1 <- ggplot(farmers, aes(hh_head.HH.distance.q9, herdsize_exot/herdsize))  + geom_point() + geom_smooth() + xlim(0,30)  + xlab("distance to tarmac road") + ylab("share of exotic cows")
p2 <- ggplot(farmers, aes(hh_head.HH.distance.q10, herdsize_exot/herdsize))  + geom_point() + geom_smooth() + xlim(0,30) + xlab("distance to murram road") + ylab("share of exotic cows")
p3 <- ggplot(farmers, aes(hh_head.HH.distance.q14, herdsize_exot/herdsize))  + geom_point() + geom_smooth() + xlim(0,30) + xlab("distance to market") + ylab("share of exotic cows")

farmers$hh_head.HH.animal_health.q201[farmers$hh_head.HH.animal_health.q201>900] <- NA
farmers$hh_head.HH.animal_health.q202[farmers$hh_head.HH.animal_health.q202>900] <- NA

p4 <-ggplot(farmers, aes(hh_head.HH.animal_health.q201, herdsize_exot/herdsize)) + geom_point() + geom_smooth() + xlim(0,30)  + xlab("distance vet") + ylab("share of exotic cows")
pdf("/home/bjvca/data/projects/PIMDVC/presentations/exotic_adoption_space.pdf")
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

pdf("/home/bjvca/data/projects/PIMDVC/presentations/exotic_adoption_size.pdf")
ggplot(farmers, aes(herdsize, herdsize_exot/herdsize)) + geom_point()+ geom_smooth() + xlim(0,70)  + xlab("herd size") + ylab("share of exotic cows")+geom_vline(xintercept =20)
dev.off()


##neighbout
summary(as.numeric(as.character(farmers$hh_head.HH.sales.q73)))
##trader
summary(c(as.numeric(as.character(farmers$hh_head.HH.sales.q87)),as.numeric(as.character(farmers$hh_head.HH.sales.q123))))
##transporter
summary(c(as.numeric(as.character(farmers$hh_head.HH.sales.q141)),as.numeric(as.character(farmers$hh_head.HH.sales.q160))))
summary(as.numeric(as.character(farmers$hh_head.HH.sales.R23)))

## need to make a long dataset with price and sold to
ridgeplot1 <-data.frame( as.numeric(as.character(farmers$hh_head.HH.sales.q73)), farmers$shed)
ridgeplot1$sold_to <- "neighbour"
names(ridgeplot1) <- c("price","shed","sold_to") 
tapply(ridgeplot1$price, ridgeplot1$shed,mean, na.rm=T)

ridgeplot2 <- data.frame(as.numeric(as.character(farmers$hh_head.HH.sales.q87)), farmers$shed)
names(ridgeplot2) <- c("price","shed")
ridgeplot2$sold_to <- "trader"
ridgeplot2_2 <- data.frame(as.numeric(as.character(farmers$hh_head.HH.sales.q123)),  farmers$shed)
names(ridgeplot2_2) <- c("price","shed")
ridgeplot2_2$sold_to <- "trader"
ridgeplot2 <- rbind(ridgeplot2, ridgeplot2_2)
rm(ridgeplot2_2)

tapply(ridgeplot2$price, ridgeplot2$shed,mean,ra.rm=T)
t.test(ridgeplot2$price~ridgeplot2$shed)

ridgeplot3 <- data.frame(as.numeric(as.character(farmers$hh_head.HH.sales.q141)), farmers$shed)
names(ridgeplot3) <- c("price","shed")
ridgeplot3$sold_to <- "transporter"
ridgeplot3_2 <- data.frame(as.numeric(as.character(farmers$hh_head.HH.sales.q160)), farmers$shed)
names(ridgeplot3_2) <- c("price","shed")
ridgeplot3_2$sold_to <- "transporter"
ridgeplot3_3 <- data.frame(as.numeric(as.character(farmers$hh_head.HH.sales.R3)), farmers$shed)
names(ridgeplot3_3) <- c("price","shed")
ridgeplot3_3$sold_to <- "transporter"
ridgeplot3 <- rbind(rbind(ridgeplot3, ridgeplot3_2),ridgeplot3_2)
rm(c(ridgeplot3_2,ridgeplot3_3)

tapply(ridgeplot3$price, ridgeplot3$shed,mean,na.rm=T)
t.test(ridgeplot3$price~ridgeplot3$shed)


## need to make a long dataset with price and sold to
ridgeplot4 <-data.frame( as.numeric(as.character(farmers$hh_head.HH.sales.R23)), farmers$shed)
ridgeplot4$sold_to <- "mcc"
names(ridgeplot4) <- c("price","shed","sold_to") 
tapply(ridgeplot4$price, ridgeplot4$shed,mean, na.rm=T)

ridgeplot_all <- rbind(ridgeplot1,ridgeplot2,ridgeplot3,ridgeplot4)

ridgeplot_all$price[ridgeplot_all$price > 1800] <- NA 
pdf("/home/bjvca/data/projects/PIMDVC/presentations/prices.pdf")
ggplot(ridgeplot_all, aes(x = price, y = sold_to, fill = sold_to)) +
  geom_density_ridges() + facet_grid(shed ~ .) +
  theme_ridges() +
  theme(legend.position = "none")
dev.off()

prices <- data.frame(as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q43)), farmers$shed)
prices$quote <- "average price"
names(prices) <- c("price","shed","quote")
prices2 <- data.frame(as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.qX1)), farmers$shed)
prices2$quote <- "lowest price"
names(prices2) <- c("price","shed","quote")
prices3 <- data.frame(as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.qX2)), farmers$shed)
prices3$quote <- "highest price"
names(prices3) <- c("price","shed","quote")
prices_dry <- rbind(prices,prices2,prices3)
prices_dry$season <- "dry season"

prices <- data.frame(as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q52)), farmers$shed)
prices$quote <- "average price"
names(prices) <- c("price","shed","quote")
prices2 <- data.frame(as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q53)), farmers$shed)
prices2$quote <- "lowest price"
names(prices2) <- c("price","shed","quote")
prices3 <- data.frame(as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q54)), farmers$shed)
prices3$quote <- "highest price"
names(prices3) <- c("price","shed","quote")
prices_rainy <- rbind(prices,prices2,prices3)
prices_rainy$season <- "rainy season"

prices <- rbind(prices_dry,prices_rainy)
prices$price[prices$price == 999] <- NA
prices$price[prices$price > 1800] <- NA
prices$price[prices$price < 200] <- NA

ggplot(prices, aes(x = price, y = quote, fill = quote)) +
  geom_density_ridges() + facet_grid(shed ~ season) +
  theme_ridges() +
  theme(legend.position = "none")




