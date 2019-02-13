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
farmers$herdsize_local <- rowSums(farmers[c("hh_head.HH.cattle_ownership.q23", "hh_head.HH.cattle_ownership.q25","hh_head.HH.cattle_ownership.q27")], na.rm=T)

farmers$herdsize_exot <- rowSums(farmers[c("hh_head.HH.cattle_ownership.q29", "hh_head.HH.cattle_ownership.q31","hh_head.HH.cattle_ownership.q33")], na.rm=T)



## herd size 10 years ago

farmers$hh_head.HH.recall.q234 <- as.numeric(as.character(farmers$hh_head.HH.recall.q234))
farmers$hh_head.HH.recall.q235 <- as.numeric(as.character(farmers$hh_head.HH.recall.q235))
farmers[c("hh_head.HH.recall.q234","hh_head.HH.recall.q235")] <- lapply(farmers[c("hh_head.HH.recall.q234","hh_head.HH.recall.q235")] , function(x) replace(x, x == 999, NA) )

farmers$herdsize_exot_b <- as.numeric(as.character(farmers$hh_head.HH.recall.q234))
farmers$herdsize_local_b <- as.numeric(as.character(farmers$hh_head.HH.recall.q235))
farmers$herdsize_exot_b[is.na(farmers$herdsize_exot_b)] <- 0
farmers$herdsize_local_b[is.na(farmers$herdsize_local_b)] <- 0

#unless they did not have cows back then
farmers$herdsize_exot_b[farmers$hh_head.HH.recall.q233!="Yes"] <- NA
farmers$herdsize_local_b[farmers$hh_head.HH.recall.q233!="Yes"] <- NA

farmers$herdsize_b <- rowSums( farmers[c("herdsize_exot_b","herdsize_local_b")] , na.rm=T)
farmers$herdsize_b <- rowSums( farmers[c("herdsize_exot_b","herdsize_local_b")] , na.rm=T)
farmers$herdsize_b[farmers$hh_head.HH.recall.q233!="Yes"] <- NA

##change in herd size
 mean(farmers$herdsize_exot_b, na.rm=T)
 mean(farmers$herdsize_exot, na.rm=T)

 mean(farmers$herdsize_local_b, na.rm=T)
 mean(farmers$herdsize_local, na.rm=T)

res <- matrix(NA,8,4)
res[1,1] <- mean(farmers$herdsize_exot_b[farmers$shed=="SW"], na.rm=T)
res[2,1] <-  mean(farmers$herdsize_exot[farmers$shed=="SW"], na.rm=T)

res[3,1] <- mean(farmers$herdsize_local_b[farmers$shed=="SW"], na.rm=T)
res[4,1] <-  mean(farmers$herdsize_local[farmers$shed=="SW"], na.rm=T)

res[5,1] <- mean(farmers$herdsize_exot_b[farmers$shed=="C"], na.rm=T)
res[6,1] <-  mean(farmers$herdsize_exot[farmers$shed=="C"], na.rm=T)

res[7,1] <- mean(farmers$herdsize_local_b[farmers$shed=="C"], na.rm=T)
res[8,1] <-  mean(farmers$herdsize_local[farmers$shed=="C"], na.rm=T)
res <- data.frame(res)

res[c(1,3,5,7),2] <- "10 y ago"
res[c(2,4,6,8),2] <- "now"
res[c(1,2,5,6),3] <- "exotics"
res[c(3,4,7,8),3] <- "local"
res[1:4,4] <- "Southwest shed"
res[5:8,4] <- "Central shed"

names(res) <- c("number","time","type","shed")
pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/herd_time.pdf")
ggplot(res, aes(x = time, y = number, fill = type)) + 
  geom_bar(stat = "identity")  + facet_grid(. ~ shed) + theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))  + theme(legend.text=element_text(size=14)) + theme(strip.text.x = element_text(size = 14))
dev.off()
pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/herd_time_pres.pdf")
ggplot(res, aes(x = time, y = number, fill = type)) + 
  geom_bar(stat = "identity")  + facet_grid(. ~ shed) + theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24,face="bold"))  + theme(legend.text=element_text(size=24)) + theme(strip.text.x = element_text(size = 24)) + theme(legend.title = element_blank()) 
dev.off()

 mean(farmers$herdsize_exot_b, na.rm=T)/mean(farmers$herdsize_local_b+farmers$herdsize_exot_b, na.rm=T)
 mean(farmers$herdsize_exot, na.rm=T)/mean(farmers$herdsize_local+farmers$herdsize_exot, na.rm=T)


 mean(farmers$herdsize_exot_b[farmers$shed=="C"], na.rm=T)/mean(farmers$herdsize_local_b[farmers$shed=="C"]+farmers$herdsize_exot_b[farmers$shed=="C"], na.rm=T)
 mean(farmers$herdsize_exot[farmers$shed=="C"], na.rm=T)/mean(farmers$herdsize_local[farmers$shed=="C"]+farmers$herdsize_exot[farmers$shed=="C"], na.rm=T)


 mean(farmers$herdsize_exot_b[farmers$shed=="SW"], na.rm=T)/mean(farmers$herdsize_local_b[farmers$shed=="SW"]+farmers$herdsize_exot_b[farmers$shed=="SW"], na.rm=T)
 mean(farmers$herdsize_exot[farmers$shed=="SW"], na.rm=T)/mean(farmers$herdsize_local[farmers$shed=="SW"]+farmers$herdsize_exot[farmers$shed=="SW"], na.rm=T)

### ticks
## are ticks a problem (likert 1-5)


ggplot(farmers, aes(x=hh_head.HH.food_safety.S1 )) + geom_bar()+ facet_grid(. ~ shed)
ggplot(farmers, aes(x=hh_head.HH.food_safety.S2 )) + geom_bar()+ facet_grid(. ~ shed)

prop.table(table(farmers$hh_head.HH.food_safety.S1[farmers$shed=="C"]))
prop.table(table(farmers$hh_head.HH.food_safety.S1[farmers$shed=="SW"]))

prop.table(table(farmers$hh_head.HH.food_safety.S2[farmers$shed=="C"]))
prop.table(table(farmers$hh_head.HH.food_safety.S2[farmers$shed=="SW"]))


## are ticks a problem (likert 1-5)

### do you spray?

 tapply(farmers$hh_head.HH.food_safety.q195,farmers$shed,summary)



###prices

prices <- data.frame(farmers$ID,(as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.qX1))+as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.qX2)))/2 , farmers$shed)
prices$quote <- "average price"
names(prices) <- c("ID","price","shed","quote")
prices2 <- data.frame(farmers$ID,as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.qX1)), farmers$shed)
prices2$quote <- "lowest price"
names(prices2) <- c("ID","price","shed","quote")
prices3 <- data.frame(farmers$ID,as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.qX2)), farmers$shed)
prices3$quote <- "highest price"
names(prices3) <- c("ID","price","shed","quote")
prices_dry <- rbind(prices,prices2,prices3)
prices_dry$season <- "dry season"

prices <- data.frame(farmers$ID,(as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q53))+as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q54)))/2, farmers$shed)
prices$quote <- "average price"
names(prices) <- c("ID","price","shed","quote")
prices2 <- data.frame(farmers$ID,as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q53)), farmers$shed)
prices2$quote <- "lowest price"
names(prices2) <- c("ID","price","shed","quote")
prices3 <- data.frame(farmers$ID,as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q54)), farmers$shed)
prices3$quote <- "highest price"
names(prices3) <- c("ID","price","shed","quote")
prices_rainy <- rbind(prices,prices2,prices3)
prices_rainy$season <- "rainy season"

prices <- rbind(prices_dry,prices_rainy)
prices$price[prices$price == 999] <- NA
prices$price[prices$price > 1800] <- NA
prices$price[prices$price < 200] <- NA

ggplot(prices[prices$quote=="average price",], aes(x = price, y = season, fill = season)) +
  geom_density_ridges() + facet_grid(~ shed) +
  theme_ridges() +
  theme(legend.position = "none")



to_merge <- aggregate( prices$price[prices$quote=="average price"], list(prices$ID[prices$quote=="average price"]) ,mean, na.rm=T)
names(to_merge) <- c("ID","average_price")
farmers <- merge(farmers,to_merge, by = "ID", all.x=T)
farmers <- merge(farmers, prices[prices$quote=="average price" & prices$season=="dry season",][c("ID","price")], by="ID", all.x=T)
names(farmers)[names(farmers) == 'price'] <- 'price_dry_season'
farmers <- merge(farmers, prices[prices$quote=="average price" & prices$season=="rainy season",][c("ID","price")], by="ID", all.x=T)
names(farmers)[names(farmers) == 'price'] <- 'price_rainy_season'

farmers$hh_head.HH.distance.q11[farmers$hh_head.HH.distance.q11==999] <- NA
farmers$hh_head.HH.distance.q9[farmers$hh_head.HH.distance.q9==999] <- NA
farmers$hh_head.HH.distance.q10[farmers$hh_head.HH.distance.q10>900] <- NA
farmers$hh_head.HH.distance.q14[farmers$hh_head.HH.distance.q14>900] <- NA
farmers$hh_head.HH.distance.q15[farmers$hh_head.HH.distance.q15>900] <- NA
farmers$hh_head.HH.distance.q12[farmers$hh_head.HH.distance.q12>900] <- NA
farmers$hh_head.HH.distance.q13[farmers$hh_head.HH.distance.q13>900] <- NA
farmers$hh_head.HH.distance.q16[farmers$hh_head.HH.distance.q16>900] <- NA
farmers$hh_head.HH.animal_health.q201[farmers$hh_head.HH.animal_health.q201>900] <- NA
farmers$hh_head.HH.animal_health.q202[farmers$hh_head.HH.animal_health.q202>900] <- NA

### distance to milk collection center
p1 <- ggplot(farmers, aes(hh_head.HH.distance.q11, herdsize_exot/herdsize, colour=shed)) + geom_smooth()  + xlab("distance to milk collection center") + ylab("share of exotic cows")  + coord_cartesian(xlim = c(0, 20), ylim=c(0,1))
#murram
p2 <-  ggplot(farmers, aes(hh_head.HH.distance.q10, herdsize_exot/herdsize, colour=shed)) + geom_smooth()  + xlab("distance to all weather road") + ylab("share of exotic cows")  + coord_cartesian(xlim = c(0, 20), ylim=c(0,1))
###distance to shop for medicien
p3 <- ggplot(farmers, aes(hh_head.HH.animal_health.q202, herdsize_exot/herdsize, colour=shed))  + coord_cartesian(xlim = c(0, 20), ylim=c(.25,1))+  geom_smooth()  + xlab("distance to drug store (km)") + ylab("share of exotic cows")
###distance to shop for medicien
p4 <- ggplot(farmers, aes(hh_head.HH.animal_health.q201, herdsize_exot/herdsize, colour=shed))  + coord_cartesian(xlim = c(0, 20), ylim=c(.25,1))+  geom_smooth()  + xlab("distance to vet (km)") + ylab("share of exotic cows")


pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/exotic_adoption_space.pdf")
multiplot(p1, p3, p2, p4, cols=2)
dev.off()

###################################################################### market participation ####################################################################
n <- 5
res <- matrix(NA,8,n*2)

### sold to?
res[1,c(1,1+n)] <- tapply(farmers$hh_head.HH.sales.q70.1, farmers$shed,mean)
res[2,c(1,1+n)] <- tapply(farmers$hh_head.HH.sales.q70.2  | farmers$hh_head.HH.sales.q70.3 |farmers$hh_head.HH.sales.q70.4, farmers$shed,mean)

res[3,c(1,1+n)] <- tapply(farmers$hh_head.HH.sales.q70.2, farmers$shed,mean)
res[4,c(1,1+n)] <- tapply(farmers$hh_head.HH.sales.q70.3, farmers$shed,mean)
res[5,c(1,1+n)] <- tapply(farmers$hh_head.HH.sales.q70.4, farmers$shed,mean)

res[6,c(1,1+n)] <- tapply(farmers$hh_head.HH.sales.q70.5  | farmers$hh_head.HH.sales.q70.6 |farmers$hh_head.HH.sales.q70.7, farmers$shed,mean)
res[7,c(1,1+n)] <- tapply(farmers$hh_head.HH.sales.q70.8, farmers$shed,mean)
res[8,c(1,1+n)] <- tapply(farmers$hh_head.HH.sales.q70.9, farmers$shed,mean)

#average number of transactions
farmers[c("hh_head.HH.sales.q71", "hh_head.HH.sales.q85", "hh_head.HH.sales.q103", "hh_head.HH.sales.q121", "hh_head.HH.sales.q139", "hh_head.HH.sales.q158", "hh_head.HH.sales.R1", "hh_head.HH.sales.R20", "hh_head.HH.sales.R40")] <- lapply(farmers[c("hh_head.HH.sales.q71", "hh_head.HH.sales.q85", "hh_head.HH.sales.q103", "hh_head.HH.sales.q121", "hh_head.HH.sales.q139", "hh_head.HH.sales.q158", "hh_head.HH.sales.R1", "hh_head.HH.sales.R20", "hh_head.HH.sales.R40")], function(x) replace(x, x == "999","n/a") )

farmers[c("hh_head.HH.sales.q71", "hh_head.HH.sales.q85", "hh_head.HH.sales.q103", "hh_head.HH.sales.q121", "hh_head.HH.sales.q139", "hh_head.HH.sales.q158", "hh_head.HH.sales.R1", "hh_head.HH.sales.R20", "hh_head.HH.sales.R40")] <- lapply(farmers[c("hh_head.HH.sales.q71", "hh_head.HH.sales.q85", "hh_head.HH.sales.q103", "hh_head.HH.sales.q121", "hh_head.HH.sales.q139", "hh_head.HH.sales.q158", "hh_head.HH.sales.R1", "hh_head.HH.sales.R20", "hh_head.HH.sales.R40")], function(x) as.numeric(as.character(x)) )



res[1,c(2,2+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q71)), farmers$shed,mean, na.rm=T)
res[2,c(2,2+n)] <- tapply(rowSums(farmers[c("hh_head.HH.sales.q85", "hh_head.HH.sales.q103", "hh_head.HH.sales.q121")], na.rm=T)[rowSums(farmers[c("hh_head.HH.sales.q85", "hh_head.HH.sales.q103", "hh_head.HH.sales.q121")], na.rm=T)>0], farmers$shed[rowSums(farmers[c("hh_head.HH.sales.q85", "hh_head.HH.sales.q103", "hh_head.HH.sales.q121")], na.rm=T)>0],mean)

res[3,c(2,2+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q85)), farmers$shed,mean, na.rm=T)
res[4,c(2,2+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q103)), farmers$shed,mean, na.rm=T)
res[5,c(2,2+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q121)), farmers$shed,mean, na.rm=T)

res[6,c(2,2+n)] <- tapply(rowSums(farmers[c("hh_head.HH.sales.q139", "hh_head.HH.sales.q158", "hh_head.HH.sales.R1")], na.rm=T)[rowSums(farmers[c("hh_head.HH.sales.q139", "hh_head.HH.sales.q158", "hh_head.HH.sales.R1")], na.rm=T)>0], farmers$shed[rowSums(farmers[c("hh_head.HH.sales.q139", "hh_head.HH.sales.q158", "hh_head.HH.sales.R1")], na.rm=T)>0],mean)

res[7,c(2,2+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.R20)), farmers$shed,mean, na.rm=T)
res[8,c(2,2+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.R40)), farmers$shed,mean, na.rm=T)

#average number of type 
farmers[c("hh_head.HH.sales.q72", "hh_head.HH.sales.q86", "hh_head.HH.sales.q104", "hh_head.HH.sales.q122", "hh_head.HH.sales.q140", "hh_head.HH.sales.q159", "hh_head.HH.sales.R2", "hh_head.HH.sales.R21", "hh_head.HH.sales.R41")] <- lapply(farmers[c("hh_head.HH.sales.q72", "hh_head.HH.sales.q86", "hh_head.HH.sales.q104", "hh_head.HH.sales.q122", "hh_head.HH.sales.q140", "hh_head.HH.sales.q159", "hh_head.HH.sales.R2", "hh_head.HH.sales.R21", "hh_head.HH.sales.R41")], function(x) replace(x, x == "999","n/a") )

farmers[c("hh_head.HH.sales.q72", "hh_head.HH.sales.q86", "hh_head.HH.sales.q104", "hh_head.HH.sales.q122", "hh_head.HH.sales.q140", "hh_head.HH.sales.q159", "hh_head.HH.sales.R2", "hh_head.HH.sales.R21", "hh_head.HH.sales.R41")] <- lapply(farmers[c("hh_head.HH.sales.q72", "hh_head.HH.sales.q86", "hh_head.HH.sales.q104", "hh_head.HH.sales.q122", "hh_head.HH.sales.q140", "hh_head.HH.sales.q159", "hh_head.HH.sales.R2", "hh_head.HH.sales.R21", "hh_head.HH.sales.R41")], function(x) as.numeric(as.character(x)) )



res[1,c(3,3+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q72)), farmers$shed,mean, na.rm=T)
res[2,c(3,3+n)] <- tapply(rowSums(farmers[c("hh_head.HH.sales.q86", "hh_head.HH.sales.q104", "hh_head.HH.sales.q122")], na.rm=T)[rowSums(farmers[c("hh_head.HH.sales.q86", "hh_head.HH.sales.q104", "hh_head.HH.sales.q122")], na.rm=T)>0], farmers$shed[rowSums(farmers[c("hh_head.HH.sales.q86", "hh_head.HH.sales.q104", "hh_head.HH.sales.q122")], na.rm=T)>0],mean)

res[3,c(3,3+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q86)), farmers$shed,mean, na.rm=T)
res[4,c(3,3+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q104)), farmers$shed,mean, na.rm=T)
res[5,c(3,3+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q122)), farmers$shed,mean, na.rm=T)

res[6,c(3,3+n)] <- tapply(rowSums(farmers[c("hh_head.HH.sales.q140", "hh_head.HH.sales.q159", "hh_head.HH.sales.R2")], na.rm=T)[rowSums(farmers[c("hh_head.HH.sales.q140", "hh_head.HH.sales.q159", "hh_head.HH.sales.R2")], na.rm=T)>0], farmers$shed[rowSums(farmers[c("hh_head.HH.sales.q140", "hh_head.HH.sales.q159", "hh_head.HH.sales.R2")], na.rm=T)>0],mean)

res[7,c(3,3+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.R21)), farmers$shed,mean, na.rm=T)
res[8,c(3,3+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.R41)), farmers$shed,mean, na.rm=T)

### use resampling based hypothesis tests
library(coin)
wilcox_test(hh_head.HH.sales.R21~shed, data=farmers, distribution="exact") 

#prices 
farmers[c("hh_head.HH.sales.q73", "hh_head.HH.sales.q87", "hh_head.HH.sales.q105", "hh_head.HH.sales.q123", "hh_head.HH.sales.q141", "hh_head.HH.sales.q160", "hh_head.HH.sales.R3", "hh_head.HH.sales.R23", "hh_head.HH.sales.R42")] <- lapply(farmers[c("hh_head.HH.sales.q73", "hh_head.HH.sales.q87", "hh_head.HH.sales.q105", "hh_head.HH.sales.q123", "hh_head.HH.sales.q141", "hh_head.HH.sales.q160", "hh_head.HH.sales.R3", "hh_head.HH.sales.R23", "hh_head.HH.sales.R42")], function(x) replace(x, x == "999","n/a") )

farmers[c("hh_head.HH.sales.q73", "hh_head.HH.sales.q87", "hh_head.HH.sales.q105", "hh_head.HH.sales.q123", "hh_head.HH.sales.q141", "hh_head.HH.sales.q160", "hh_head.HH.sales.R3", "hh_head.HH.sales.R23", "hh_head.HH.sales.R42")] <- lapply(farmers[c("hh_head.HH.sales.q73", "hh_head.HH.sales.q87", "hh_head.HH.sales.q105", "hh_head.HH.sales.q123", "hh_head.HH.sales.q141", "hh_head.HH.sales.q160", "hh_head.HH.sales.R3", "hh_head.HH.sales.R23", "hh_head.HH.sales.R42")], function(x) as.numeric(as.character(x)) )

res[1,c(4,4+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q73)), farmers$shed,mean, na.rm=T)
res[2,c(4,4+n)] <- tapply(rowSums(farmers[c("hh_head.HH.sales.q87",  "hh_head.HH.sales.q123")], na.rm=T)[rowSums(farmers[c("hh_head.HH.sales.q87", "hh_head.HH.sales.q123")], na.rm=T)>0], farmers$shed[rowSums(farmers[c("hh_head.HH.sales.q87", "hh_head.HH.sales.q123")], na.rm=T)>0],mean)

res[3,c(4,4+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q87)), farmers$shed,mean, na.rm=T)
res[4,c(4,4+n)] <- NA
res[5,c(4,4+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q123)), farmers$shed,mean, na.rm=T)

res[6,c(4,4+n)] <- tapply(rowSums(farmers[c("hh_head.HH.sales.q141", "hh_head.HH.sales.q160", "hh_head.HH.sales.R3")], na.rm=T)[rowSums(farmers[c("hh_head.HH.sales.q141", "hh_head.HH.sales.q160", "hh_head.HH.sales.R3")], na.rm=T)>0], farmers$shed[rowSums(farmers[c("hh_head.HH.sales.q141", "hh_head.HH.sales.q160", "hh_head.HH.sales.R3")], na.rm=T)>0],mean)

res[7,c(4,4+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.R23)), farmers$shed,mean, na.rm=T)
res[8,c(4,4+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.R42)), farmers$shed,mean, na.rm=T)

## amounts


farmers[c("hh_head.HH.sales.q74", "hh_head.HH.sales.q88", "hh_head.HH.sales.q106", "hh_head.HH.sales.q124", "hh_head.HH.sales.q142", "hh_head.HH.sales.q161", "hh_head.HH.sales.R4", "hh_head.HH.sales.R24", "hh_head.HH.sales.R43")] <- lapply(farmers[c("hh_head.HH.sales.q74", "hh_head.HH.sales.q88", "hh_head.HH.sales.q106", "hh_head.HH.sales.q124", "hh_head.HH.sales.q142", "hh_head.HH.sales.q161", "hh_head.HH.sales.R4", "hh_head.HH.sales.R24", "hh_head.HH.sales.R43")] , function(x) replace(x, x == "999","n/a") )

farmers[c("hh_head.HH.sales.q74", "hh_head.HH.sales.q88", "hh_head.HH.sales.q106", "hh_head.HH.sales.q124", "hh_head.HH.sales.q142", "hh_head.HH.sales.q161", "hh_head.HH.sales.R4", "hh_head.HH.sales.R24", "hh_head.HH.sales.R43")]  <- lapply(farmers[c("hh_head.HH.sales.q74", "hh_head.HH.sales.q88", "hh_head.HH.sales.q106", "hh_head.HH.sales.q124", "hh_head.HH.sales.q142", "hh_head.HH.sales.q161", "hh_head.HH.sales.R4", "hh_head.HH.sales.R24", "hh_head.HH.sales.R43")] , function(x) as.numeric(as.character(x)) )
## for amounts, it seems some pleople entred value here. Delete everything above 99 liters
farmers$hh_head.HH.sales.q74[as.numeric(as.character(farmers$hh_head.HH.sales.q74)) > 99] <- NA
farmers$hh_head.HH.sales.q88[as.numeric(as.character(farmers$hh_head.HH.sales.q88)) > 99] <- NA
farmers$hh_head.HH.sales.q106[as.numeric(as.character(farmers$hh_head.HH.sales.q106)) > 99] <- NA
farmers$hh_head.HH.sales.q124[as.numeric(as.character(farmers$hh_head.HH.sales.q124)) > 99] <- NA
farmers$hh_head.HH.sales.q142[as.numeric(as.character(farmers$hh_head.HH.sales.q142)) > 99] <- NA
farmers$hh_head.HH.sales.q161[as.numeric(as.character(farmers$hh_head.HH.sales.q161)) > 99] <- NA
farmers$hh_head.HH.sales.R4[as.numeric(as.character(farmers$hh_head.HH.sales.R4)) > 99] <- NA
farmers$hh_head.HH.sales.R24[as.numeric(as.character(farmers$hh_head.HH.sales.R24)) > 99] <- NA
farmers$hh_head.HH.sales.R43[as.numeric(as.character(farmers$hh_head.HH.sales.R43)) > 99] <- NA



res[1,c(5,5+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q74)), farmers$shed,mean, na.rm=T)
res[2,c(5,5+n)] <- tapply(rowSums(farmers[c("hh_head.HH.sales.q88","hh_head.HH.sales.q106",  "hh_head.HH.sales.q124")], na.rm=T)[rowSums(farmers[c("hh_head.HH.sales.q88","hh_head.HH.sales.q106",  "hh_head.HH.sales.q124")], na.rm=T)>0], farmers$shed[rowSums(farmers[c("hh_head.HH.sales.q88","hh_head.HH.sales.q106",  "hh_head.HH.sales.q124")], na.rm=T)>0],mean)

res[3,c(5,5+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q88)), farmers$shed,mean, na.rm=T)
res[4,c(5,5+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q106)), farmers$shed,mean, na.rm=T)
res[5,c(5,5+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.q124)), farmers$shed,mean, na.rm=T)

res[6,c(5,5+n)] <- tapply(rowSums(farmers[c("hh_head.HH.sales.q142", "hh_head.HH.sales.q161", "hh_head.HH.sales.R4")], na.rm=T)[rowSums(farmers[c("hh_head.HH.sales.q142", "hh_head.HH.sales.q161", "hh_head.HH.sales.R4")], na.rm=T)>0], farmers$shed[rowSums(farmers[c("hh_head.HH.sales.q142", "hh_head.HH.sales.q161", "hh_head.HH.sales.R4")], na.rm=T)>0],mean)

res[7,c(5,5+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.R24)), farmers$shed,mean, na.rm=T)
res[8,c(5,5+n)] <- tapply( as.numeric(as.character(farmers$hh_head.HH.sales.R43)), farmers$shed,mean, na.rm=T)




















## need to make a long dataset with price and sold to
ridgeplot1 <-data.frame(farmers$ID, as.numeric(as.character(farmers$hh_head.HH.sales.q73)), farmers$shed)
ridgeplot1$sold_to <- "neighbour"
names(ridgeplot1) <- c("ID","price","shed","sold_to") 
tapply(ridgeplot1$price, ridgeplot1$shed,mean, na.rm=T)

ridgeplot2 <- data.frame(farmers$ID,as.numeric(as.character(farmers$hh_head.HH.sales.q87)), farmers$shed)
names(ridgeplot2) <- c("ID","price","shed")
ridgeplot2$sold_to <- "trader"
ridgeplot2_2 <- data.frame(farmers$ID,as.numeric(as.character(farmers$hh_head.HH.sales.q123)),  farmers$shed)
names(ridgeplot2_2) <- c("ID","price","shed")
ridgeplot2_2$sold_to <- "trader"
ridgeplot2 <- rbind(ridgeplot2, ridgeplot2_2)
rm(ridgeplot2_2)

tapply(ridgeplot2$price, ridgeplot2$shed,mean,ra.rm=T)
t.test(ridgeplot2$price~ridgeplot2$shed)

ridgeplot3 <- data.frame(farmers$ID,as.numeric(as.character(farmers$hh_head.HH.sales.q141)), farmers$shed)
names(ridgeplot3) <- c("ID","price","shed")
ridgeplot3$sold_to <- "transporter"
ridgeplot3_2 <- data.frame(farmers$ID,as.numeric(as.character(farmers$hh_head.HH.sales.q160)), farmers$shed)
names(ridgeplot3_2) <- c("ID","price","shed")
ridgeplot3_2$sold_to <- "transporter"
ridgeplot3_3 <- data.frame(farmers$ID,as.numeric(as.character(farmers$hh_head.HH.sales.R3)), farmers$shed)
names(ridgeplot3_3) <- c("ID","price","shed")
ridgeplot3_3$sold_to <- "transporter"
ridgeplot3 <- rbind(rbind(ridgeplot3, ridgeplot3_2),ridgeplot3_2)
rm(c(ridgeplot3_2,ridgeplot3_3)

tapply(ridgeplot3$price, ridgeplot3$shed,mean,na.rm=T)
t.test(ridgeplot3$price~ridgeplot3$shed)


## need to make a long dataset with price and sold to
ridgeplot4 <-data.frame(farmers$ID, as.numeric(as.character(farmers$hh_head.HH.sales.R23)), farmers$shed)
ridgeplot4$sold_to <- "mcc"
names(ridgeplot4) <- c("ID","price","shed","sold_to") 
tapply(ridgeplot4$price, ridgeplot4$shed,mean, na.rm=T)

ridgeplot_all <- rbind(ridgeplot1,ridgeplot2,ridgeplot3,ridgeplot4)

ridgeplot_all$price[ridgeplot_all$price > 1800] <- NA 
pdf("/home/bjvca/data/projects/PIMDVC/presentations/prices.pdf")
ggplot(ridgeplot_all, aes(x = price, y = sold_to, fill = sold_to)) +
  geom_density_ridges() + facet_grid(shed ~ .) +
  theme_ridges() +
  theme(legend.position = "none")
dev.off()

### grazing land
farmers$hh_head.HH.land.q21[farmers$hh_head.HH.land.q21==999] <- NA 
tapply(farmers$hh_head.HH.land.q21 , farmers$shed,  mean, na.rm = T)
tapply(farmers$hh_head.HH.land.q21 , farmers$shed,  median, na.rm = T)

### price of cows
farmers$price_cow_local <- as.numeric(as.character(farmers$hh_head.HH.cattle_ownership.q24))
farmers$price_cow_exot <- as.numeric(as.character(farmers$hh_head.HH.cattle_ownership.q30))
farmers$price_cow_local[farmers$price_cow_local==999] <- NA
farmers$price_cow_exot[farmers$price_cow_exot==999] <- NA
##delete outlier after inspection of boxplot
farmers$price_cow_exot[farmers$price_cow_exot>=3000000] <- NA

summary(farmers[c("price_cow_local","price_cow_exot")])


farmers$price_heifer_local <- as.numeric(as.character(farmers$hh_head.HH.cattle_ownership.q26))
farmers$price_heifer_exot <- as.numeric(as.character(farmers$hh_head.HH.cattle_ownership.q32))
farmers$price_heifer_local[farmers$price_heifer_local==999] <- NA
farmers$price_heifer_exot[farmers$price_heifer_exot==999] <- NA
summary(farmers[c("price_heifer_local","price_heifer_exot")])


farmers$price_calf_local <- as.numeric(as.character(farmers$hh_head.HH.cattle_ownership.q28))
farmers$price_calf_exot <- as.numeric(as.character(farmers$hh_head.HH.cattle_ownership.q34))
farmers$price_calf_local[farmers$price_calf_local==999] <- NA
farmers$price_calf_exot[farmers$price_calf_exot==999] <- NA
summary(farmers[c("price_calf_local","price_calf_exot")])


#output

farmers$liters_dry_exot <-  as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q42))
farmers$liters_dry_exot[farmers$liters_dry_exot > 25] <- NA
farmers$liters_dry_exot[farmers$liters_dry_exot == 0] <- NA

farmers$liters_dry_local <-  as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q38))
farmers$liters_dry_local[farmers$liters_dry_local > 25] <- NA
farmers$liters_dry_local[farmers$liters_dry_local == 0] <- NA


farmers$liters_rain_exot <-  as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q51))
farmers$liters_rain_exot[farmers$liters_rain_exot > 25] <- NA
farmers$liters_rain_exot[farmers$liters_rain_exot == 0] <- NA

farmers$liters_rain_local <-  as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q47))
farmers$liters_rain_local[farmers$liters_rain_local > 25] <- NA
farmers$liters_rain_local[farmers$liters_rain_local == 0] <- NA

summary(c(farmers$liters_dry_exot,farmers$liters_dry_local))
summary(c(farmers$liters_rain_exot,farmers$liters_rain_local))

summary(c(farmers$liters_rain_exot,farmers$liters_dry_exot))
summary(c(farmers$liters_rain_local,farmers$liters_dry_local))
### total daily production in dry season (av yield*number of animals in milk)
farmers$in_milk_dry_local <-  as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q35))
farmers$in_milk_dry_local[farmers$in_milk_dry_local > 200] <- NA
farmers$prod_dry_local <- NA
farmers$prod_dry_local[farmers$in_milk_dry_local==0] <- 0
farmers$prod_dry_local[!is.na(farmers$in_milk_dry_local) & farmers$in_milk_dry_local>0] <- farmers$in_milk_dry_local[!is.na(farmers$in_milk_dry_local) & farmers$in_milk_dry_local>0] * farmers$liters_dry_local[!is.na(farmers$in_milk_dry_local) & farmers$in_milk_dry_local>0]

farmers$in_milk_dry_exot <-  as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q39))
farmers$in_milk_dry_exot[farmers$in_milk_dry_exot > 200] <- NA
farmers$prod_dry_exot <- NA
farmers$prod_dry_exot[farmers$in_milk_dry_exot==0] <- 0
farmers$prod_dry_exot[!is.na(farmers$in_milk_dry_exot) & farmers$in_milk_dry_exot>0] <- farmers$in_milk_dry_exot[!is.na(farmers$in_milk_dry_exot) & farmers$in_milk_dry_exot>0] * farmers$liters_dry_exot[!is.na(farmers$in_milk_dry_exot) & farmers$in_milk_dry_exot>0]

farmers$prod_dry <- farmers$prod_dry_exot + farmers$prod_dry_local

### total daily production in rainy season (av yield*number of animals in milk)
farmers$in_milk_rain_local <-  as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q44))
farmers$in_milk_rain_local[farmers$in_milk_rain_local > 200] <- NA
farmers$prod_rain_local <- NA
farmers$prod_rain_local[farmers$in_milk_rain_local==0] <- 0
farmers$prod_rain_local[!is.na(farmers$in_milk_rain_local) & farmers$in_milk_rain_local>0] <- farmers$in_milk_rain_local[!is.na(farmers$in_milk_rain_local) & farmers$in_milk_rain_local>0] * farmers$liters_rain_local[!is.na(farmers$in_milk_rain_local) & farmers$in_milk_rain_local>0]

farmers$in_milk_rain_exot <-  as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q48))
farmers$in_milk_rain_exot[farmers$in_milk_rain_exot > 200] <- NA
farmers$prod_rain_exot <- NA
farmers$prod_rain_exot[farmers$in_milk_rain_exot==0] <- 0
farmers$prod_rain_exot[!is.na(farmers$in_milk_rain_exot) & farmers$in_milk_rain_exot>0] <- farmers$in_milk_rain_exot[!is.na(farmers$in_milk_rain_exot) & farmers$in_milk_rain_exot>0] * farmers$liters_rain_exot[!is.na(farmers$in_milk_rain_exot) & farmers$in_milk_rain_exot>0]

farmers$prod_rain <- farmers$prod_rain_exot + farmers$prod_rain_local
farmers$prod_rain[farmers$prod_rain>1000] <- NA

### sold
farmers$sold_dry <- as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.qX3))
farmers$sold_dry[farmers$sold_dry >200] <- NA 

qplot(farmers$shed,farmers$sold_dry, geom="boxplot")

farmers$sold_dry_share <- farmers$sold_dry/farmers$prod_dry
farmers$sold_dry_share[farmers$sold_dry_share > 1] <- NA

qplot(farmers$shed,farmers$sold_dry, geom="boxplot")

ggplot(farmers, aes(x = sold_dry_share, y = shed, fill = shed)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none") + scale_x_continuous(limits = c(0,1))

farmers$sold_rain <- as.numeric(as.character(farmers$hh_head.HH.dairy_ouput.q55))
farmers$sold_rain[farmers$sold_rain >500] <- NA 

qplot(farmers$shed,farmers$sold_rain, geom="boxplot")

farmers$sold_rain_share <- farmers$sold_rain/farmers$prod_rain
farmers$sold_rain_share[farmers$sold_rain_share > 1] <- NA

qplot(farmers$shed,farmers$sold_rain_share, geom="boxplot")

ggplot(farmers, aes(x = sold_rain_share, y = shed, fill = shed)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none") + scale_x_continuous(limits = c(0,1)) 
##faceting by season?
library(reshape2)
to_plot <- melt(farmers[c("shed","sold_dry_share","sold_rain_share")])

levels(to_plot$variable) <- c("dry season","rainy season") 
pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/market_part.pdf")
ggplot(to_plot, aes(x = value, y = variable, fill = variable)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none") + scale_x_continuous(limits = c(0,1)) + facet_grid(shed ~ .)
dev.off()
### cooperative membership - does it affect hygene?

farmers$coop <- (farmers$hh_head.HH.cooperative.q266==1 | farmers$hh_head.HH.cooperative.q266==2) 
farmers$education_prim <- (farmers$hh_head.HH.q6 != 1)
farmers$education_sec <- (farmers$hh_head.HH.q6 == 4 | farmers$hh_head.HH.q6 == 5 | farmers$hh_head.HH.q6 == 6)
farmers$dairy <- farmers$hh_head.HH.Housing.q17==2

controls <- "shed+education_prim+education_sec+dairy"



## milk in shed
shed <- lm( as.formula(paste("(farmers$hh_head.HH.food_safety.q181==1)~coop",controls,sep="+")),data=farmers)

#wash udders with lukewarm water
wash <- lm( as.formula(paste("(hh_head.HH.food_safety.q186 == 2)~coop",controls,sep="+")),data=farmers)
#udders dried with clean towel
dry <- lm( as.formula(paste("(hh_head.HH.food_safety.q188 ==3)~coop",controls,sep="+")),data=farmers)
#milking cream used
cream <- lm( as.formula(paste("(hh_head.HH.food_safety.q187 == 'Yes')~coop",controls,sep="+")),data=farmers)


##uses aluminum buckets for milking
bucket <- lm( as.formula(paste("(hh_head.HH.food_safety.q190.4 ==T)~coop",controls,sep="+")),data=farmers)
##store milk in cans
milk_cans <- lm( as.formula(paste("(hh_head.HH.livestock_assets.q243 == 'Yes')~coop",controls,sep="+")),data=farmers)
##wash hands with soap
wash_hands <- lm( as.formula(paste("(hh_head.HH.food_safety.q192 ==3)~coop",controls,sep="+")),data=farmers)



credplot.gg <- function(d,units){
 # d is a data frame with 4 columns
 # d$x gives variable names
 # d$y gives center point
 # d$ylo gives lower limits
 # d$yhi gives upper limits
 require(ggplot2)
 p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi))+
 geom_pointrange(position=position_dodge(-.4), color="blue")+ 
geom_text(aes(label=format(round(y, 2), nsmall = 2)), nudge_x = .1, hjust=-0.1)+
 geom_hline(yintercept = 0, linetype=2)+
 coord_flip()+
 xlab('') + ylab(units)+ theme(axis.text=element_text(size=18),
        axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=18), legend.title=element_blank())+
    geom_errorbar(aes(ymin=ylo, ymax=yhi),position=position_dodge(-.4),width=0,cex=1.5, color="blue") 
 return(p)
}



sig <- 1.64  ## 90 percent
plot_ag <- data.frame(matrix(NA, 4,5))
names(plot_ag) <- c("x","y","ylo","yhi","grp")
h <- 7
plot_ag[7,1] <- "Hands washed with soap?"
plot_ag[6,1] <- "Milk cans used to store?"
plot_ag[5,1] <- "Milk bucket used to milk?"
plot_ag[4,1] <- "Used udder cream?"
plot_ag[3,1] <- "Udders dried with clean towel?"
plot_ag[2,1] <- "Udders washed with lukewarm water?"

plot_ag[1,1] <- "Milk in shed?"

plot_ag[7,2] <- coef(wash_hands)[2]
plot_ag[6,2] <- coef(milk_cans)[2]
plot_ag[5,2] <- coef(bucket)[2]
plot_ag[4,2] <- coef(cream)[2]
plot_ag[3,2] <- coef(dry)[2]
plot_ag[2,2] <- coef(wash)[2]

plot_ag[1,2] <-  coef(shed)[2]


plot_ag[7,3] <- confint(wash_hands)[2,1]
plot_ag[6,3] <- confint(milk_cans)[2,1]
plot_ag[5,3] <- confint(bucket)[2,1]
plot_ag[4,3] <- confint(cream)[2,1]
plot_ag[3,3] <- confint(dry)[2,1]
plot_ag[2,3] <- confint(wash)[2,1]

plot_ag[1,3] <- confint(shed)[2,1]

plot_ag[7,4] <- confint(wash_hands)[2,2]
plot_ag[6,4] <- confint(milk_cans)[2,2]
plot_ag[5,4] <- confint(bucket)[2,2]
plot_ag[4,4] <- confint(cream)[2,2]
plot_ag[3,4] <- confint(dry)[2,2]
plot_ag[2,4] <- confint(wash)[2,2]

plot_ag[1,4] <- confint(shed)[2,2]

plot_ag$x <- factor(plot_ag$x, levels=plot_ag$x)


pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/hygene.pdf")
credplot.gg(plot_ag,'%')
dev.off()

farmers$contract <- NA
farmers$contract[farmers$hh_head.HH.sales.R29==1] <- "oral"
farmers$contract[farmers$hh_head.HH.sales.R29==2] <- "written"
farmers$contract[farmers$hh_head.HH.sales.R29==3] <- "no"

tapply(farmers$herdsize_exot/(farmers$herdsize_exot+farmers$herdsize_local),farmers$contract, mean, na.rm=T)

farmers$contract <- NA
farmers$contract[farmers$hh_head.HH.sales.q93==1 | farmers$hh_head.HH.sales.q111==1 | farmers$hh_head.HH.sales.q129==1] <- "oral"
farmers$contract[farmers$hh_head.HH.sales.q93==2 | farmers$hh_head.HH.sales.q111==2 | farmers$hh_head.HH.sales.q129==2] <- "written"
farmers$contract[farmers$hh_head.HH.sales.q93==3 | farmers$hh_head.HH.sales.q111==3 | farmers$hh_head.HH.sales.q129==3] <- "no"

tapply(farmers$herdsize_exot/(farmers$herdsize_exot+farmers$herdsize_local),farmers$contract, mean, na.rm=T)


farmers$contract <- NA
farmers$contract[farmers$hh_head.HH.sales.q147==1 | farmers$hh_head.HH.sales.q166==1 | farmers$hh_head.HH.sales.R9==1] <- "oral"
farmers$contract[farmers$hh_head.HH.sales.q147==2 | farmers$hh_head.HH.sales.q166==2 | farmers$hh_head.HH.sales.R9==2] <- "written"
farmers$contract[farmers$hh_head.HH.sales.q147==3 | farmers$hh_head.HH.sales.q166==3 | farmers$hh_head.HH.sales.R9==3] <- "no"

tapply(farmers$herdsize_exot/(farmers$herdsize_exot+farmers$herdsize_local),farmers$contract, mean, na.rm=T)

### all

farmers$contract <- NA
farmers$contract[farmers$hh_head.HH.sales.R29==1 | farmers$hh_head.HH.sales.q93==1 | farmers$hh_head.HH.sales.q111==1 | farmers$hh_head.HH.sales.q129==1|farmers$hh_head.HH.sales.q147==1 | farmers$hh_head.HH.sales.q166==1 | farmers$hh_head.HH.sales.R9==1] <- "oral"
farmers$contract[farmers$hh_head.HH.sales.R29==2 | farmers$hh_head.HH.sales.q93==2 | farmers$hh_head.HH.sales.q111==2 | farmers$hh_head.HH.sales.q129==2 | farmers$hh_head.HH.sales.q147==2 | farmers$hh_head.HH.sales.q166==2 | farmers$hh_head.HH.sales.R9==2] <- "written"
farmers$contract[farmers$hh_head.HH.sales.R29==3 | farmers$hh_head.HH.sales.q93==3 | farmers$hh_head.HH.sales.q111==3 | farmers$hh_head.HH.sales.q129==3 |farmers$hh_head.HH.sales.q147==3 | farmers$hh_head.HH.sales.q166==3 | farmers$hh_head.HH.sales.R9==3] <- "no"

### adoption of cows
pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/adoption_contract1.pdf")
barplot(tapply(farmers$herdsize_exot/(farmers$herdsize_exot+farmers$herdsize_local),farmers$contract, mean, na.rm=T), xlab="contract type", ylab="share of cross-bred cows in total herd")
dev.off()
pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/adoption_contract2.pdf")
### adoption of milk cans for storage
barplot(tapply(farmers$hh_head.HH.food_safety.q190.6==T,farmers$contract, mean, na.rm=T), xlab="contract type", ylab="stores milk in milk cans (% of farmers)")

dev.off()

pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/adoption_contract3.pdf")
### no free range
barplot(tapply(farmers$hh_head.HH.food_safety.q196.1!=T  ,farmers$contract, mean, na.rm=T), xlab="contract type", ylab="uses improved feeding (% of farmers)")
dev.off()


res <- matrix(NA,3,3)

res[1,] <- tapply(farmers$herdsize_exot/(farmers$herdsize_exot+farmers$herdsize_local),farmers$contract, mean, na.rm=T)
res[2,] <- tapply(farmers$hh_head.HH.food_safety.q190.6==T,farmers$contract, mean, na.rm=T)
res[3,] <- tapply(farmers$hh_head.HH.food_safety.q196.1!=T  ,farmers$contract, mean, na.rm=T)

services <- c("share of cross-bred cows in total herd",
"stores milk in milk cans (% of farmers)",
"uses improved feeding (% of farmers)")

res <- data.frame(services, res)
names(res) <- c("innovation","no","oral","written")


res_m <- melt(res)

names(res_m) <-  c("services","contract","share")
pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/contract.pdf")
ggplot(data=res_m, aes(x=reorder(services,share), y=share, fill=contract)) +
geom_bar(stat="identity", position=position_dodge()) + coord_flip() +  theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 12)) + theme(text = element_text(size = 12)) + theme(axis.title.y=element_blank()) + theme(legend.text=element_text(size=12))
dev.off()

ggplot(data=res_m, aes(x=reorder(services,share), y=share, fill=contract)) +
geom_bar(stat="identity", position=position_dodge()) + coord_flip() +  theme(axis.text = element_text(size = 35))+ theme(axis.title = element_text(size = 25)) + theme(text = element_text(size = 25)) + theme(axis.title.y=element_blank()) + theme(legend.text=element_text(size=25))

farmers$hh_head.HH.recall.q236[farmers$hh_head.HH.recall.q236==999] <- NA

farmers$prod_b <- as.numeric(as.character(farmers$hh_head.HH.recall.q236))

tapply((farmers$prod_dry +farmers$prod_rain)/2, farmers$shed, mean, na.rm=T)
tapply(farmers$prod_b, farmers$shed, mean, na.rm=T)

       C       SW 
28.97966 53.90917 

tapply(farmers$prod_b, farmers$shed, mean, na.rm=T)
       C       SW 
18.31287 35.65895 


df <- data.frame(year=c("2008", "2018","2008", "2018"),
                prod=c(18.31287, 28.97966, 35.65895 ,53.90917 ), shed= c("C","C","SW","SW"))
head(df)


pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/production_pres.pdf")
ggplot(data=df, aes(x=year, y=prod, group=shed, col=shed)) +
  geom_line(size=3)+
  geom_point(size=6)+ theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"),legend.text=element_text(size=30), legend.title=element_blank())
dev.off()




