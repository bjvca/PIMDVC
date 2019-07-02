rm(list=ls())
library(ggplot2)
library(ggridges)
library(stargazer)
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

res[c(1,3,5,7),2] <- "10 years ago"
res[c(2,4,6,8),2] <- "now"
res[c(1,2,5,6),3] <- "exotics"
res[c(3,4,7,8),3] <- "local"
res[1:4,4] <- "Southwest shed"
res[5:8,4] <- "Central shed"

names(res) <- c("number","time","type","shed")
pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/herd_time.pdf")
ggplot(res, aes(x = time, y = number, fill = type)) + 
  geom_bar(stat = "identity")  + facet_grid(. ~ shed) + theme_bw() + theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"))  + theme(legend.text=element_text(size=18)) + theme(strip.text.x = element_text(size = 18))   + theme(legend.title = element_blank()) +  scale_fill_grey(start = .2, end = .7) 
dev.off()
pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/herd_time_pres.pdf")
ggplot(res, aes(x = time, y = number, fill = type)) + 
  geom_bar(stat = "identity")  + facet_grid(. ~ shed) + theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24,face="bold"))  + theme(legend.text=element_text(size=24)) + theme(strip.text.x = element_text(size = 24)) + theme(legend.title = element_blank()) +  scale_fill_grey(start = .2, end = .7)
dev.off()

 mean(farmers$herdsize_exot_b/(farmers$herdsize_local_b+farmers$herdsize_exot_b), na.rm=T)
 mean(farmers$herdsize_exot/(farmers$herdsize_local+farmers$herdsize_exot), na.rm=T)


 mean(farmers$herdsize_exot_b[farmers$shed=="C"]/(farmers$herdsize_local_b[farmers$shed=="C"]+farmers$herdsize_exot_b[farmers$shed=="C"]), na.rm=T)
 mean(farmers$herdsize_exot[farmers$shed=="C"]/(farmers$herdsize_local[farmers$shed=="C"]+farmers$herdsize_exot[farmers$shed=="C"]), na.rm=T)


 mean(farmers$herdsize_exot_b[farmers$shed=="SW"]/(farmers$herdsize_local_b[farmers$shed=="SW"]+farmers$herdsize_exot_b[farmers$shed=="SW"]), na.rm=T)
 mean(farmers$herdsize_exot[farmers$shed=="SW"]/(farmers$herdsize_local[farmers$shed=="SW"]+farmers$herdsize_exot[farmers$shed=="SW"]), na.rm=T)

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
p1 <- ggplot(farmers, aes(hh_head.HH.distance.q11, herdsize_exot/herdsize, colour=shed)) + geom_smooth()  + theme_bw()  + xlab("distance to milk collection center") + ylab("share of exotic cows")  + coord_cartesian(xlim = c(0, 20), ylim=c(0.3,1)) + scale_colour_grey(start = .2, end = .6)
#murram
p2 <-  ggplot(farmers, aes(hh_head.HH.distance.q10, herdsize_exot/herdsize, colour=shed)) + geom_smooth()  + theme_bw() + xlab("distance to all weather road") + ylab("share of exotic cows")  + coord_cartesian(xlim = c(0, 20), ylim=c(0.3,1)) + scale_colour_grey(start = .2, end = .6)
###distance to shop for medicien
p3 <- ggplot(farmers, aes(hh_head.HH.animal_health.q202, herdsize_exot/herdsize, colour=shed)) + theme_bw()  + coord_cartesian(xlim = c(0, 20), ylim=c(.3,1))+  geom_smooth()  + xlab("distance to drug store (km)") + ylab("share of exotic cows") + scale_colour_grey(start = .2, end = .6)
###distance to shop for medicien
p4 <- ggplot(farmers, aes(hh_head.HH.animal_health.q201, herdsize_exot/herdsize, colour=shed)) + theme_bw()  + coord_cartesian(xlim = c(0, 20), ylim=c(.3,1))+  geom_smooth()  + xlab("distance to vet (km)") + ylab("share of exotic cows") + scale_colour_grey(start = .2, end = .6)


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
library(coin)
wilcox_test(hh_head.HH.land.q21~shed, data=farmers, distribution="exact") 
prop.table(table(farmers$hh_head.HH.land.q21b , farmers$shed),2)

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


df <- data.frame(cbind(c(farmers$liters_rain_local,farmers$liters_dry_local),c(farmers$shed,farmers$shed)))
names(df) <- c("price","shed")
tapply(df$price,df$shed,mean,na.rm=T)

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

farmers$in_milk_rain <- farmers$in_milk_rain_local + farmers$in_milk_rain_exot 
farmers$in_milk_dry <- farmers$in_milk_dry_local + farmers$in_milk_dry_exot 


df <- data.frame(cbind(c(farmers$in_milk_rain,farmers$in_milk_dry),c(farmers$shed,farmers$shed)))
names(df) <- c("in_milk","shed")
mean(df$in_milk, na.rm=T)
tapply(df$in_milk,df$shed,mean,na.rm=T)

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
ggplot(to_plot, aes(x = value, y = variable, fill = variable)) + theme_bw() +
  geom_density_ridges() +
  theme_ridges() +  scale_fill_grey(start = .2, end = .7) +
  theme(legend.position = "none") + scale_x_continuous(limits = c(0,1)) + facet_grid(shed ~ .)
dev.off()
### cooperative membership - does it affect hygene?


farmers$coop <- (farmers$hh_head.HH.cooperative.q266==1 | farmers$hh_head.HH.cooperative.q266==2) 
farmers$nondairy_coop <- (farmers$hh_head.HH.cooperative.q267==1 | farmers$hh_head.HH.cooperative.q267==2) 
farmers$education_prim <- (farmers$hh_head.HH.q6 != 1)
farmers$education_sec <- (farmers$hh_head.HH.q6 == 4 | farmers$hh_head.HH.q6 == 5 | farmers$hh_head.HH.q6 == 6)
farmers$dairy <- farmers$hh_head.HH.Housing.q17==2

 prop.table(table(farmers$shed,farmers$coop),1)
 prop.table(table(farmers$shed,farmers$nondairy_coop),1)

 prop.test(table(farmers$shed,farmers$coop))
 prop.test(table(farmers$shed,farmers$nondairy_coop))

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
geom_bar(stat="identity", position=position_dodge()) + coord_flip() + theme_bw() +  theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 12)) + theme(text = element_text(size = 12)) + theme(axis.title.y=element_blank()) + theme(legend.text=element_text(size=12))+scale_fill_grey(start = .2, end = .7) 
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



prop.table(table(farmers$hh_head.HH.sales.q81[!(farmers$hh_head.HH.sales.q81=="n/a")],farmers$shed[!(farmers$hh_head.HH.sales.q81=="n/a")]),2)


prop.table(table(farmers$hh_head.HH.food_safety.q190.4==T | farmers$hh_head.HH.food_safety.q190.6==T, farmers$shed),2)

##number of steel cans
farmers$hh_head.HH.livestock_assets.q244 <- as.numeric(as.character(farmers$hh_head.HH.livestock_assets.q244))
farmers$hh_head.HH.livestock_assets.q244[is.na(farmers$hh_head.HH.livestock_assets.q244)] <- 0
 tapply(farmers$hh_head.HH.livestock_assets.q244,farmers$shed, mean)
##number of steel buckets
farmers$hh_head.HH.livestock_assets.q246 <- as.numeric(as.character(farmers$hh_head.HH.livestock_assets.q246))
farmers$hh_head.HH.livestock_assets.q246[is.na(farmers$hh_head.HH.livestock_assets.q246)] <- 0
 tapply(farmers$hh_head.HH.livestock_assets.q246,farmers$shed, mean)

### analysis of margins

### prices

farmers[c("hh_head.HH.sales.q71", "hh_head.HH.sales.q85", "hh_head.HH.sales.q103", "hh_head.HH.sales.q121", "hh_head.HH.sales.q139", "hh_head.HH.sales.q158", "hh_head.HH.sales.R1", "hh_head.HH.sales.R20", "hh_head.HH.sales.R40")] <- lapply(farmers[c("hh_head.HH.sales.q71", "hh_head.HH.sales.q85", "hh_head.HH.sales.q103", "hh_head.HH.sales.q121", "hh_head.HH.sales.q139", "hh_head.HH.sales.q158", "hh_head.HH.sales.R1", "hh_head.HH.sales.R20", "hh_head.HH.sales.R40")], function(x) replace(x, is.na(x),0) )

## how do we define if a farmer is integrated into the export value chain?
farmers$export_ind <- FALSE
farmers$export_ind <- (farmers$hh_head.HH.sales.q158 + farmers$hh_head.HH.sales.q103  + farmers$hh_head.HH.sales.R20)>=7

farmers[c("hh_head.HH.sales.q73", "hh_head.HH.sales.q87", "hh_head.HH.sales.q105", "hh_head.HH.sales.q123", "hh_head.HH.sales.q141", "hh_head.HH.sales.q160", "hh_head.HH.sales.R3", "hh_head.HH.sales.R23", "hh_head.HH.sales.R42")] <- lapply(farmers[c("hh_head.HH.sales.q73", "hh_head.HH.sales.q87", "hh_head.HH.sales.q105", "hh_head.HH.sales.q123", "hh_head.HH.sales.q141", "hh_head.HH.sales.q160", "hh_head.HH.sales.R3", "hh_head.HH.sales.R23", "hh_head.HH.sales.R42")], function(x) replace(x, is.na(x),0) )

farmers[c("hh_head.HH.sales.q74", "hh_head.HH.sales.q88", "hh_head.HH.sales.q106", "hh_head.HH.sales.q124", "hh_head.HH.sales.q142", "hh_head.HH.sales.q161", "hh_head.HH.sales.R4", "hh_head.HH.sales.R24", "hh_head.HH.sales.R43")] <- lapply(farmers[c("hh_head.HH.sales.q74", "hh_head.HH.sales.q88", "hh_head.HH.sales.q106", "hh_head.HH.sales.q124", "hh_head.HH.sales.q142", "hh_head.HH.sales.q161", "hh_head.HH.sales.R4", "hh_head.HH.sales.R24", "hh_head.HH.sales.R43")] , function(x) replace(x, is.na(x),0) )


##weekly income = price*daily quantity*number of transactions in a week
farmers$weekly_income <- rowSums(farmers[c("hh_head.HH.sales.q73", "hh_head.HH.sales.q87", "hh_head.HH.sales.q105", "hh_head.HH.sales.q123", "hh_head.HH.sales.q141", "hh_head.HH.sales.q160", "hh_head.HH.sales.R3", "hh_head.HH.sales.R23", "hh_head.HH.sales.R42")] *farmers[c("hh_head.HH.sales.q74", "hh_head.HH.sales.q88", "hh_head.HH.sales.q106", "hh_head.HH.sales.q124", "hh_head.HH.sales.q142", "hh_head.HH.sales.q161", "hh_head.HH.sales.R4", "hh_head.HH.sales.R24", "hh_head.HH.sales.R43")]*farmers[c("hh_head.HH.sales.q71", "hh_head.HH.sales.q85", "hh_head.HH.sales.q103", "hh_head.HH.sales.q121", "hh_head.HH.sales.q139", "hh_head.HH.sales.q158", "hh_head.HH.sales.R1", "hh_head.HH.sales.R20", "hh_head.HH.sales.R40")] )
###subtract from this:

#labour
labour <- paste("hh_head.HH.training.q", 209:228, sep="")
farmers[labour] <- lapply(farmers[labour] , function(x)  as.numeric(as.character(x)) ) 
farmers[labour] <- lapply(farmers[labour] , function(x)  replace(x, x==999,0) )
farmers[labour] <- lapply(farmers[labour] , function(x)  replace(x, is.na(x),0) )
### we may want to apply some sort of equivalence scales here
## count male family labour for .8 hired equivalent
farmers[paste("hh_head.HH.training.q", 209:213, sep="")] <-  farmers[paste("hh_head.HH.training.q", 209:213, sep="")] *0
farmers[paste("hh_head.HH.training.q", 214:218, sep="")] <-  farmers[paste("hh_head.HH.training.q", 214:218, sep="")] *0
farmers[paste("hh_head.HH.training.q", 219:228, sep="")] <-  farmers[paste("hh_head.HH.training.q", 219:228, sep="")] *0
###hired labour
farmers$hh_head.HH.training.q230 <- as.numeric(as.character(farmers$hh_head.HH.training.q230))
farmers$hh_head.HH.training.q230[farmers$hh_head.HH.training.q230 == 999] <- 0
farmers$hh_head.HH.training.q230[is.na(farmers$hh_head.HH.training.q230)] <- 0
farmers$man_hours <- rowSums(farmers[labour]) + farmers$hh_head.HH.training.q230
### how do we value man hours? 
farmers$hh_head.HH.training.q231 <- as.numeric(as.character(farmers$hh_head.HH.training.q231))
farmers$hh_head.HH.training.q231[farmers$hh_head.HH.training.q231 == 999] <- NA 

### imputation of missing wages
farmers <- within(farmers, {mean_wage = ave(hh_head.HH.training.q231,sub,FUN=function(x) median(x, na.rm=T))} )
#farmers$hh_head.HH.training.q231[is.na(farmers$hh_head.HH.training.q231)] <- farmers$mean_wage[is.na(farmers$hh_head.HH.training.q231)] 
#farmers$weekly_labour_cost <- (farmers$hh_head.HH.training.q231/8*farmers$man_hours)*7
farmers$weekly_labour_cost <- (farmers$mean_wage/8*farmers$man_hours)*7
### expenses on animal health
farmers$hh_head.HH.animal_health.q205 <- as.numeric(as.character(farmers$hh_head.HH.animal_health.q205))
farmers$hh_head.HH.animal_health.q205[is.na(farmers$hh_head.HH.animal_health.q205)] <- 0
farmers$hh_head.HH.animal_health.q205[farmers$hh_head.HH.animal_health.q205==999] <- 0
farmers$vet_exp <- farmers$hh_head.HH.animal_health.q205/52
farmers$vet_exp[ farmers$vet_exp>30000] <- NA

### cost of cows

farmers$hh_head.HH.cattle_ownership.q24 <- as.numeric(as.character(farmers$hh_head.HH.cattle_ownership.q24))
farmers <- within(farmers, {mean_p_local_cow = ave(hh_head.HH.cattle_ownership.q24,sub,FUN=function(x) median(x, na.rm=T))} )
#farmers$cost_local <- farmers$hh_head.HH.cattle_ownership.q23* farmers$hh_head.HH.cattle_ownership.q24/(12*52)  ## average cow lives 12 years
#farmers$cost_local[is.na(farmers$cost_local )] <-  farmers$hh_head.HH.cattle_ownership.q23[is.na(farmers$cost_local )] * farmers$mean_p_local_cow[is.na(farmers$cost_local )]  /(12*52)  ## average cow lives 12 years
farmers$cost_local <-  farmers$hh_head.HH.cattle_ownership.q23*(farmers$mean_p_local_cow/(12*52))  ## average cow lives 12 years

farmers$hh_head.HH.cattle_ownership.q30 <- as.numeric(as.character(farmers$hh_head.HH.cattle_ownership.q30))
farmers <- within(farmers, {mean_p_imp_cow = ave(hh_head.HH.cattle_ownership.q30,sub,FUN=function(x) median(x, na.rm=T))} )
#farmers$cost_improved <- farmers$hh_head.HH.cattle_ownership.q29* farmers$hh_head.HH.cattle_ownership.q30 /(12*52)  ## average cow lives 12 years
#farmers$cost_improved[is.na(farmers$cost_improved )] <- farmers$hh_head.HH.cattle_ownership.q29[is.na(farmers$cost_improved )]* farmers$mean_p_imp_cow[is.na(farmers$cost_improved )]  /(12*52)  ## average cow lives 12 years
farmers$cost_improved <- farmers$hh_head.HH.cattle_ownership.q29*( farmers$mean_p_imp_cow/(12*52))  ## average cow lives 12 years
#regression 
#exogenous controls - age and education level

farmers$age_head <- ifelse(farmers$hh_head.HH.hh_member.1..q5b == 1,farmers$hh_head.HH.hh_member.1..q5d, ifelse(farmers$hh_head.HH.hh_member.2..q5b == 1,farmers$hh_head.HH.hh_member.2..q5d,ifelse(farmers$hh_head.HH.hh_member.3..q5b == 1,farmers$hh_head.HH.hh_member.3..q5d,ifelse(farmers$hh_head.HH.hh_member.4..q5b == 1,farmers$hh_head.HH.hh_member.4..q5d,ifelse(farmers$hh_head.HH.hh_member.5..q5b == 1,farmers$hh_head.HH.hh_member.5..q5d,ifelse(farmers$hh_head.HH.hh_member.6..q5b == 1,farmers$hh_head.HH.hh_member.6..q5d,ifelse(farmers$hh_head.HH.hh_member.7..q5b == 1,farmers$hh_head.HH.hh_member.7..q5d,ifelse(farmers$hh_head.HH.hh_member.8..q5b == 1,farmers$hh_head.HH.hh_member.8..q5d,ifelse(farmers$hh_head.HH.hh_member.9..q5b == 1,farmers$hh_head.HH.hh_member.9..q5d,ifelse(farmers$hh_head.HH.hh_member.10..q5b == 1,farmers$hh_head.HH.hh_member.10..q5d,ifelse(farmers$hh_head.HH.hh_member.11..q5b == 1,farmers$hh_head.HH.hh_member.11..q5d,ifelse(farmers$hh_head.HH.hh_member.12..q5b == 1,farmers$hh_head.HH.hh_member.12..q5d,ifelse(farmers$hh_head.HH.hh_member.13..q5b == 1,farmers$hh_head.HH.hh_member.13..q5d,ifelse(farmers$hh_head.HH.hh_member.14..q5b == 1,farmers$hh_head.HH.hh_member.14..q5d,ifelse(farmers$hh_head.HH.hh_member.15..q5b == 1,farmers$hh_head.HH.hh_member.15..q5d,ifelse(farmers$hh_head.HH.hh_member.16..q5b == 1,farmers$hh_head.HH.hh_member.16..q5d,ifelse(farmers$hh_head.HH.hh_member.17..q5b == 1,farmers$hh_head.HH.hh_member.17..q5d,ifelse(farmers$hh_head.HH.hh_member.18..q5b == 1,farmers$hh_head.HH.hh_member.18..q5d,ifelse(farmers$hh_head.HH.hh_member.19..q5b == 1,farmers$hh_head.HH.hh_member.19..q5d,ifelse(farmers$hh_head.HH.hh_member.20..q5b == 1,farmers$hh_head.HH.hh_member.20..q5d,NA))))))))))))))))))))
farmers$age_head[is.na(farmers$age_head)] <- farmers$hh_head.HH.hh_member.1..q5d[is.na(farmers$age_head)]
farmers$age_head[farmers$age_head==999] <- NA

farmers$fem_head <- ifelse(farmers$hh_head.HH.hh_member.1..q5b == 1,farmers$hh_head.HH.hh_member.1..gender, ifelse(farmers$hh_head.HH.hh_member.2..q5b == 1,farmers$hh_head.HH.hh_member.2..gender,ifelse(farmers$hh_head.HH.hh_member.3..q5b == 1,farmers$hh_head.HH.hh_member.3..gender,ifelse(farmers$hh_head.HH.hh_member.4..q5b == 1,farmers$hh_head.HH.hh_member.4..gender,ifelse(farmers$hh_head.HH.hh_member.5..q5b == 1,farmers$hh_head.HH.hh_member.5..gender,ifelse(farmers$hh_head.HH.hh_member.6..q5b == 1,farmers$hh_head.HH.hh_member.6..gender,ifelse(farmers$hh_head.HH.hh_member.7..q5b == 1,farmers$hh_head.HH.hh_member.7..gender,ifelse(farmers$hh_head.HH.hh_member.8..q5b == 1,farmers$hh_head.HH.hh_member.8..gender,ifelse(farmers$hh_head.HH.hh_member.9..q5b == 1,farmers$hh_head.HH.hh_member.9..gender,ifelse(farmers$hh_head.HH.hh_member.10..q5b == 1,farmers$hh_head.HH.hh_member.10..gender,ifelse(farmers$hh_head.HH.hh_member.11..q5b == 1,farmers$hh_head.HH.hh_member.11..gender,ifelse(farmers$hh_head.HH.hh_member.12..q5b == 1,farmers$hh_head.HH.hh_member.12..gender,ifelse(farmers$hh_head.HH.hh_member.13..q5b == 1,farmers$hh_head.HH.hh_member.13..gender,ifelse(farmers$hh_head.HH.hh_member.14..q5b == 1,farmers$hh_head.HH.hh_member.14..gender,ifelse(farmers$hh_head.HH.hh_member.15..q5b == 1,farmers$hh_head.HH.hh_member.15..gender,ifelse(farmers$hh_head.HH.hh_member.16..q5b == 1,farmers$hh_head.HH.hh_member.16..gender,ifelse(farmers$hh_head.HH.hh_member.17..q5b == 1,farmers$hh_head.HH.hh_member.17..gender,ifelse(farmers$hh_head.HH.hh_member.18..q5b == 1,farmers$hh_head.HH.hh_member.18..gender,ifelse(farmers$hh_head.HH.hh_member.19..q5b == 1,farmers$hh_head.HH.hh_member.19..gender,ifelse(farmers$hh_head.HH.hh_member.20..q5b == 1,farmers$hh_head.HH.hh_member.20..gender,NA))))))))))))))))))))
farmers$fem_head[is.na(farmers$fem_head)] <- farmers$hh_head.HH.hh_member.1..gender[is.na(farmers$fem_head)]
farmers$fem_head[is.na(farmers$fem_head)] <- 2
farmers$fem_head  <- farmers$fem_head == 1

farmers$edu_sec <- farmers$hh_head.HH.q6  %in% c(4,5,6)

farmers$hh_size <- farmers$hh_head.HH.q4
farmers$nearest_neighbor <- farmers$hh_head.HH.distance.q13
farmers$nearest_milkshop <- farmers$hh_head.HH.distance.q12
farmers$nearest_mcc <- farmers$hh_head.HH.distance.q11
farmers$distance_vetshop <- farmers$hh_head.HH.animal_health.q202

farmers$access_finance <- farmers$hh_head.HH.access_finance.q268=="Yes"
farmers$landsize <- farmers$hh_head.HH.land.q21

#taken a loan to invest in dairy business?
prop.table(table(farmers$hh_head.HH.access_finance.q272 == "Yes", farmers$shed),2)
#average amount?
farmers$hh_head.HH.access_finance.q271  <- as.numeric(as.character(farmers$hh_head.HH.access_finance.q271 ))
tapply(farmers$hh_head.HH.access_finance.q271[farmers$hh_head.HH.access_finance.q272 == "Yes"], farmers$shed[farmers$hh_head.HH.access_finance.q272 == "Yes"], mean)/3600
#where obtained?
##coop
 prop.table(table(farmers$hh_head.HH.access_finance.q269.1[farmers$hh_head.HH.access_finance.q272 == "Yes"] == TRUE, farmers$shed[farmers$hh_head.HH.access_finance.q272 == "Yes"]),2)
##bank
 prop.table(table(farmers$hh_head.HH.access_finance.q269.2[farmers$hh_head.HH.access_finance.q272 == "Yes"] == TRUE, farmers$shed[farmers$hh_head.HH.access_finance.q272 == "Yes"]),2)
##friends
 prop.table(table(farmers$hh_head.HH.access_finance.q269.3[farmers$hh_head.HH.access_finance.q272 == "Yes"] == TRUE, farmers$shed[farmers$hh_head.HH.access_finance.q272 == "Yes"]),2)
##vsla
 prop.table(table(farmers$hh_head.HH.access_finance.q269.4[farmers$hh_head.HH.access_finance.q272 == "Yes"] == TRUE, farmers$shed[farmers$hh_head.HH.access_finance.q272 == "Yes"]),2)
farmers$ind_loan <- farmers$hh_head.HH.access_finance.q272 == "Yes"

#regression 
#exogenous controls - age and education level
farmers$margin <- farmers$weekly_income  - farmers$weekly_labour_cost - farmers$vet_exp  - farmers$cost_local - farmers$cost_improved 
# - farmers$weekly_labour_cost - farmers$cost_local - farmers$cost_improved 

farmers$use_cans <- farmers$hh_head.HH.food_safety.q190.4==T | farmers$hh_head.HH.food_safety.q190.6==T
farmers$share_exot <- farmers$herdsize_exot/farmers$herdsize

farmers$sprayer <- farmers$hh_head.HH.livestock_assets.q264>0
farmers$improved_feed <- farmers$hh_head.HH.food_safety.q196.2==T
farmers$use_dam <- farmers$hh_head.HH.food_safety.q198==1

### indicator that farmer sells to milk collection center
farmers$ind_assist <- FALSE
farmers$ind_assist <-  farmers$hh_head.HH.sales.q83=="Yes" | farmers$hh_head.HH.sales.q97=="Yes" | farmers$hh_head.HH.sales.q115=="Yes" | farmers$hh_head.HH.sales.q133=="Yes" | farmers$hh_head.HH.sales.q151=="Yes" | farmers$hh_head.HH.sales.q170=="Yes" | farmers$hh_head.HH.sales.R13=="Yes" | farmers$hh_head.HH.sales.R33=="Yes" | farmers$hh_head.HH.sales.R52=="Yes"
farmers$assist_training <- FALSE
farmers$assist_training <-  farmers$hh_head.HH.sales.q84.1==T | farmers$hh_head.HH.sales.q98.1==T | farmers$hh_head.HH.sales.q116.1==T | farmers$hh_head.HH.sales.q134.1==T | farmers$hh_head.HH.sales.q152.1==T | farmers$hh_head.HH.sales.q171.1==T | farmers$hh_head.HH.sales.R14.1==T | farmers$hh_head.HH.sales.R34.1==T | farmers$hh_head.HH.sales.R53.1==T

farmers$assist_inputs <-  farmers$hh_head.HH.sales.q84.2==T | farmers$hh_head.HH.sales.q98.2==T | farmers$hh_head.HH.sales.q116.2==T | farmers$hh_head.HH.sales.q134.2==T | farmers$hh_head.HH.sales.q152.2==T | farmers$hh_head.HH.sales.q171.2==T | farmers$hh_head.HH.sales.R14.2==T | farmers$hh_head.HH.sales.R34.2==T | farmers$hh_head.HH.sales.R53.2==T

farmers$assist_credit <-  farmers$hh_head.HH.sales.q84.3==T | farmers$hh_head.HH.sales.q98.3==T | farmers$hh_head.HH.sales.q116.3==T | farmers$hh_head.HH.sales.q134.3==T | farmers$hh_head.HH.sales.q152.3==T | farmers$hh_head.HH.sales.q171.3==T | farmers$hh_head.HH.sales.R14.3==T | farmers$hh_head.HH.sales.R34.3==T | farmers$hh_head.HH.sales.R53.3==T

summary(lm(ind_assist ~export_ind, data=farmers))
summary(lm(assist_training ~export_ind, data=farmers))
summary(lm(assist_inputs ~export_ind, data=farmers))
summary(lm(assist_credit ~export_ind, data=farmers))
summary(lm(coop ~export_ind, data=farmers))
summary(lm(share_exot~export_ind, data=farmers))
summary(lm(sprayer ~export_ind, data=farmers))
summary(lm(improved_feed ~export_ind, data=farmers))
summary(lm(ind_loan ~export_ind, data=farmers))
summary(lm(use_dam ~export_ind, data=farmers))
summary(lm(use_cans ~export_ind, data=farmers))

mean(farmers$ind_assist[farmers$export_ind==F], na.rm=T)
mean(farmers$assist_training[farmers$export_ind==F], na.rm=T)
mean(farmers$assist_inputs[farmers$export_ind==F], na.rm=T)
mean(farmers$assist_credit[farmers$export_ind==F], na.rm=T)
mean(farmers$coop[farmers$export_ind==F], na.rm=T)
mean(farmers$share_exot[farmers$export_ind==F], na.rm=T)
mean(farmers$sprayer[farmers$export_ind==F], na.rm=T)
mean(farmers$improved_feed[farmers$export_ind==F], na.rm=T)
mean(farmers$ind_loan[farmers$export_ind==F], na.rm=T)
mean(farmers$use_dam[farmers$export_ind==F], na.rm=T)
mean(farmers$use_cans[farmers$export_ind==F], na.rm=T)

r1 <-lm(ind_assist~age_head+fem_head+ hh_size+ farmers$edu_sec +export_ind +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data=farmers)
r2 <- lm(assist_training ~age_head+fem_head+ hh_size+ farmers$edu_sec +export_ind +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data=farmers)
r3 <- lm(assist_inputs ~age_head+fem_head+ hh_size+ farmers$edu_sec +export_ind +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data=farmers)
r4 <-lm(assist_credit~age_head+fem_head+ hh_size+ farmers$edu_sec +export_ind +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data=farmers)
r5 <-lm(coop~age_head+fem_head+ hh_size+ farmers$edu_sec +export_ind +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data=farmers)
r6 <-lm(share_exot~age_head+fem_head+ hh_size+ farmers$edu_sec +export_ind +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data=farmers)
r7 <-lm(sprayer~age_head+fem_head+ hh_size+ farmers$edu_sec +export_ind +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data=farmers)
r8 <-lm(improved_feed~age_head+fem_head+ hh_size+ farmers$edu_sec +export_ind +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data=farmers)
r9 <-lm(ind_loan~age_head+fem_head+ hh_size+ farmers$edu_sec +export_ind +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data=farmers)
r10 <-lm(use_dam~age_head+fem_head+ hh_size+ farmers$edu_sec +export_ind +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data=farmers)
r11 <-lm(use_cans~age_head+fem_head+ hh_size+ farmers$edu_sec +export_ind +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data=farmers)
stargazer(r1,r2,r3,r4)
stargazer(r5,r6,r7,r8)
stargazer(r9,r10,r11)

library(MatchIt)
farmers_cpy <- farmers
farmers <- farmers[complete.cases(farmers[c("ID","export_ind","age_head","fem_head","hh_size","edu_sec","export_ind","nearest_neighbor","distance_vetshop","access_finance","district","landsize","nearest_mcc")]),]
farmers <- farmers[c("ID","export_ind","age_head","fem_head","hh_size","edu_sec","export_ind","nearest_neighbor","distance_vetshop","access_finance","district","landsize","nearest_mcc")]
  
match.it <- matchit(export_ind ~ age_head+fem_head+ hh_size+ edu_sec +nearest_neighbor+ distance_vetshop+access_finance+district+landsize, data = farmers, caliper=.05,method="nearest")
a <- summary(match.it)

kable(a$nn, digits = 2, align = 'c', caption = 'Table 2: Sample sizes')

kable(a$sum.matched[c(1,2,4)], digits = 2, align = 'c', caption = 'Table 3: Summary of balance for matched data')
plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[1:ncol(farmers)]
summary(lm(ind_assist~export_ind,data=merge(df.match,farmers_cpy[c("ID","ind_assist")])))
summary(lm(assist_training~export_ind,data=merge(df.match,farmers_cpy[c("ID","assist_training")])))
summary(lm(assist_inputs~export_ind,data=merge(df.match,farmers_cpy[c("ID","assist_inputs")])))
summary(lm(assist_credit~export_ind,data=merge(df.match,farmers_cpy[c("ID","assist_credit")])))
summary(lm(coop~export_ind,data=merge(df.match,farmers_cpy[c("ID","coop")])))
summary(lm(share_exot~export_ind,data=merge(df.match,farmers_cpy[c("ID","share_exot")])))
summary(lm(sprayer~export_ind,data=merge(df.match,farmers_cpy[c("ID","sprayer")])))
summary(lm(improved_feed~export_ind,data=merge(df.match,farmers_cpy[c("ID","improved_feed")])))
summary(lm(ind_loan~export_ind,data=merge(df.match,farmers_cpy[c("ID","ind_loan")])))
summary(lm(use_dam~export_ind,data=merge(df.match,farmers_cpy[c("ID","use_dam")])))
summary(lm(use_cans~export_ind,data=merge(df.match,farmers_cpy[c("ID","use_cans")])))



library(AER)

summary(ivreg(share_exot~export_ind +age_head+fem_head+ hh_size+ farmers$edu_sec+nearest_neighbor+ distance_vetshop+access_finance+landsize+district | age_head+fem_head+ hh_size+ farmers$edu_sec +nearest_neighbor+ distance_vetshop+access_finance+landsize+district+nearest_mcc, data=farmers), diagnostics=T)
summary(ivreg(use_cans~export_ind +age_head+fem_head+ hh_size+ farmers$edu_sec+nearest_neighbor+ distance_vetshop+access_finance+landsize+district | age_head+fem_head+ hh_size+ farmers$edu_sec +nearest_neighbor+ distance_vetshop+access_finance+landsize+district+nearest_mcc, data=farmers), diagnostics=T)
summary(ivreg(coop~export_ind +age_head+fem_head+ hh_size+ farmers$edu_sec+nearest_neighbor+ distance_vetshop+access_finance+landsize+district | age_head+fem_head+ hh_size+ farmers$edu_sec +nearest_neighbor+ distance_vetshop+access_finance+landsize+district+nearest_mcc, data=farmers), diagnostics=T)

hh_head.HH.distance.q10


