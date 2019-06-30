#create maps
library(ggplot2)
library(stargazer)
getmode <- function(v) {
	v.na <- v[!is.na(v)] 	
   uniqv <- unique(v.na)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

traders <- read.csv("/home/bjvca/data/projects/PIMDVC/data/public/traders.csv")
traders$year_start <- as.numeric(as.character(substr(traders$trader.q22,5,8)))

traders[c("trader.q24","trader.q25")] <-lapply(traders[c("trader.q24","trader.q25")], function(x) replace(x, x == 999, NA) )
summary(traders$trader.q24)

qplot(trader.q24, data = traders, geom = "density")
getmode(trader$trader.q24)


table(traders$trader.q24[traders$year_start <2018] >traders$trader.q25[traders$year_start<2018])
### 74 percent of traders state that there are now more traders than there were when they started trading

summary(((traders$trader.q24[traders$year_start <2018 & traders$trader.q24>0]  - traders$trader.q25[traders$year_start<2018 & traders$trader.q24>0])/traders$trader.q24[traders$year_start <2018 & traders$trader.q24>0 ])/(2018-traders$year_start[traders$year_start<2018 & traders$trader.q24>0]) )
## and they report an average increase of about 18 percent between now and when they started trading milk.

#traders are on average 30 years old - this is very young, compare to milk farmers
traders$trader.q3[traders$trader.q3 == 999] <- NA

###

traders$use_boda <- traders$trader.q65==1 & traders$trader.q66==0
traders$use_bicylce <-traders$trader.q65==0 & traders$trader.q66==1 

traders$use_jerrycan <- (traders$trader.q61 == 0 & traders$trader.q62 == 0 & traders$trader.q63 == 0 & traders$trader.q64 > 0)

traders$use_milkcan_only <- ((traders$trader.q61 >0 | traders$trader.q62 > 0 | traders$trader.q63 > 0) & traders$trader.q64 == 0)
traders$use_milkcan <- (traders$trader.q61 >0 | traders$trader.q62 > 0 | traders$trader.q63 > 0)

traders$modern <- traders$use_milkcan & traders$use_boda
traders$traditional <- traders$use_jerrycan & traders$use_bicylce



traders$trader.q50[traders$trader.q50 == 999] <- NA
traders$to_shops <- traders$trader.q50
traders$trader.q51[traders$trader.q51 == 999] <- NA
traders$direct <- traders$trader.q51
traders$trader.q52[traders$trader.q52 == 999] <- NA
traders$coop_mcc <- traders$trader.q52
traders$trader.q53[traders$trader.q53 == 999] <- NA
traders$private_mcc <- traders$trader.q53
traders$trader.q54[traders$trader.q54 == 999] <- NA
traders$processor <- traders$trader.q54

to_plot <- traders[c("ID","modern","traditional", "to_shops","direct","coop_mcc", "private_mcc","processor" )]

to_plot_means <- data.frame(c(colMeans(to_plot[,4:8], na.rm=T), colMeans(to_plot[to_plot$modern==T,4:8], na.rm=T), colMeans(to_plot[to_plot$traditional==T,4:8], na.rm=T)))
to_plot_means$outlet <- names(c(colMeans(to_plot[,4:8], na.rm=T), colMeans(to_plot[to_plot$modern==T,4:8], na.rm=T), colMeans(to_plot[to_plot$traditional==T,4:8], na.rm=T)))
to_plot_means$type <- "all"
to_plot_means$type[6:10] <- "modern"
to_plot_means$type[11:15] <- "traditional"
names(to_plot_means)[1] <- "liters_av"
to_plot_means$outlet <- ordered(to_plot_means$outlet,levels = c("processor","coop_mcc","private_mcc","to_shops","direct"))
                                
pdf("/home/bjvca/data/projects/PIMDVC/presentations/trader_sold_to.pdf")
ggplot(to_plot_means, aes(type, liters_av)) +   theme_bw() +
  geom_bar(aes(fill = outlet), position = "dodge", stat="identity")  +
  coord_flip() + theme(axis.text=element_text(size=12) 
    
df <- data.frame(c("C","SW"))

df <- cbind(df,tapply(traders$trader.q67>0,traders$shed, mean))
df <- cbind(df,tapply(traders$trader.q70==3,traders$shed, mean))
df <- cbind(df,tapply(traders$trader.q71=="Yes",traders$shed, mean))
df <- cbind(df,tapply(traders$trader.q61>0 | traders$trader.q62>0 | traders$trader.q63>0 ,traders$shed, mean))
df <- cbind(df,tapply(traders$trader.q69>0,traders$shed, mean))
df <- cbind(df,tapply(traders$trader.q97=="Yes",traders$shed, mean))


names(df) <- c("shed","has mobile","uses boda","adapted boda","uses milk cans","uses seeves","keeps records")
library(ggradar)



ggradar(df)     axis.title=element_text(size=14,face="bold"))
dev.off()

traders$agreement <- (traders$trader.q82==1 | traders$trader.q82==1 | traders$trader.q85b== 1 | traders$trader.q85b==2)

df <- data.frame(c("C","SW"))

df <- cbind(df,tapply(traders$trader.q67>0,traders$shed, mean))
df <- cbind(df,tapply(traders$trader.q70==3,traders$shed, mean))
df <- cbind(df,tapply(traders$trader.q71=="Yes",traders$shed, mean))
df <- cbind(df,tapply(traders$trader.q61>0 | traders$trader.q62>0 | traders$trader.q63>0 ,traders$shed, mean))
df <- cbind(df,tapply(traders$trader.q69>0,traders$shed, mean))
df <- cbind(df,tapply(traders$trader.q97=="Yes",traders$shed, mean))
library(ggradar)

names(df) <- c("shed","has mobile","uses boda","adapted boda","uses milk cans","uses seeves","keeps records")

pdf("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/trader_inov.pdf")
ggradar(df, font.radar = "Times",legend.text.size = 15) + scale_colour_grey(start = .2, end = .6)
dev.off()

traders$traders <- FALSE
traders$traders[traders$trader.q16==1 | traders$trader.q16==3 ] <- TRUE
traders$transporters <- FALSE
traders$transporters[traders$trader.q16==2 | traders$trader.q16==3 ] <- TRUE
traders$trader.q36[traders$trader.q36==999] <- NA
traders$trader.q48[traders$trader.q48==999] <- NA
traders$trader.q37[traders$trader.q37==999] <- NA
traders$trader.q49[traders$trader.q49==999] <- NA
traders$trader.q33[traders$trader.q33==999] <- NA
traders$trader.q46[traders$trader.q46==999] <- NA
### how many farmers
tapply(traders$trader.q36[traders$traders ],traders$shed[traders$traders ] ,mean, na.rm=T)[1]
tapply(traders$trader.q36[traders$transporters ],traders$shed[traders$transporters ] ,mean, na.rm=T)

tapply(traders$trader.q48[traders$traders ],traders$shed[traders$traders ] ,mean, na.rm=T)
tapply(traders$trader.q48[traders$transporters ],traders$shed[traders$transporters ] ,mean, na.rm=T)
### how much liters collected
tapply(traders$trader.q37[traders$traders ],traders$shed[traders$traders ] ,mean, na.rm=T)
tapply(traders$trader.q37[traders$transporters ],traders$shed[traders$transporters ] ,mean, na.rm=T)

tapply(traders$trader.q49[traders$traders ],traders$shed[traders$traders ] ,mean, na.rm=T)
tapply(traders$trader.q49[traders$transporters ],traders$shed[traders$transporters ] ,mean, na.rm=T)
### prices
tapply(traders$trader.q33[traders$traders ],traders$shed[traders$traders ] ,mean, na.rm=T)
tapply(traders$trader.q33[traders$transporters ],traders$shed[traders$transporters ] ,mean, na.rm=T)

tapply(traders$trader.q46[traders$traders ],traders$shed[traders$traders ] ,mean, na.rm=T)
tapply(traders$trader.q46[traders$transporters ],traders$shed[traders$transporters ] ,mean, na.rm=T)

traders$trader.q72[traders$trader.q72>998] <- NA

traders$coop <- (traders$trader.q73 == 3 | traders$trader.q73 == 4)
summary(lm(trader.q64~coop+shed, data=traders))
### trader uses milk cans only
prop.table(table(traders$use_milkcan_only, traders$shed),2)

traders$trader.q61 <- as.numeric(as.character(traders$trader.q61))
traders$trader.q62 <- as.numeric(as.character(traders$trader.q62))
traders$trader.q63 <- as.numeric(as.character(traders$trader.q63))
traders$trader.q61[is.na(traders$trader.q61)] <- 0
traders$trader.q62[is.na(traders$trader.q62)] <- 0
traders$trader.q63[is.na(traders$trader.q63)] <- 0

traders$nr_cans <- traders$trader.q61 + traders$trader.q62 + traders$trader.q63

tapply(traders$nr_cans, traders$shed, mean)
#taken a loan to invest in dairy business
prop.table(table(traders$trader.q102 == "Yes", traders$shed),2)
traders$credit <- traders$trader.q102 == "Yes"
#average amount
traders$trader.q101 <- as.numeric(as.character(traders$trader.q101))
tapply(traders$trader.q101[traders$trader.q102 == "Yes"], traders$shed[traders$trader.q102 == "Yes"], mean)
#where obtained?
##coop
 prop.table(table(traders$trader.q99.1[traders$trader.q102 == "Yes"] == TRUE, traders$shed[traders$trader.q102 == "Yes"]),2)
##bank
 prop.table(table(traders$trader.q99.2[traders$trader.q102 == "Yes"] == TRUE, traders$shed[traders$trader.q102 == "Yes"]),2)
##friends
 prop.table(table(traders$trader.q99.3[traders$trader.q102 == "Yes"] == TRUE, traders$shed[traders$trader.q102 == "Yes"]),2)
##vsla
 prop.table(table(traders$trader.q99.4[traders$trader.q102 == "Yes"] == TRUE, traders$shed[traders$trader.q102 == "Yes"]),2)

traders$boda <- traders$trader.q70=="3" | traders$trader.q70=="4"

## regressions
##indicator that trader is integrated in export led value chain
cats <- paste("trader.q", 50:57, sep ="")
traders[cats] <- lapply(traders[cats] , function(x)  replace(x, x==999,0) )

traders$export_ind <- rowSums(traders[paste("trader.q", 52:54, sep ="")])/rowSums(traders[cats] ) == 1
traders$export_ind[is.na(traders$export_ind)] <- 0 
traders$coop <- traders$trader.q30=="Yes"
traders$specialization <- traders$trader.q16==2 & traders$trader.q17=="No" & traders$trader.q19=="No"
traders$competition <- traders$trader.q24
#this has many missings, put missings on median
traders$competition[is.na(traders$competition)] <- median(traders$competition, na.rm=T)
traders$collusion <- traders$trader.q26==1 |traders$trader.q27==1


summary(lm(coop~export_ind,data=traders))
summary(lm(credit~export_ind,data=traders))
summary(lm(use_milkcan_only~export_ind,data=traders))
summary(lm(boda~export_ind,data=traders))
prop.test(table(traders$shed,traders$boda),1)
prop.test(table(traders$shed,traders$coop),1)
prop.test(table(traders$shed,traders$credit),1)
prop.test(table(traders$shed,traders$use_milkcan_only),1)



mean(traders$coop[traders$export_ind==F], na.rm=T)
mean(traders$credit[traders$export_ind==F], na.rm=T)
mean(traders$use_milkcan_only[traders$export_ind==F], na.rm=T)
mean(traders$boda[traders$export_ind==F], na.rm=T)



### dependent variable
traders$edu_sec <- traders$trader.q5  %in% c(4,5,6)
traders$age<- traders$trader.q3
traders$hh_size<- traders$trader.q8
traders$fem <- traders$trader.q4=="Female"
traders$is_head <- traders$trader.q6=="Yes"
traders$trader.q86 <- as.numeric(as.character(traders$trader.q86))
traders$trader.q91 <- as.numeric(as.character(traders$trader.q91))
traders$trader.q88 <- as.numeric(as.character(traders$trader.q88))
traders$access_finance <- traders$trader.q98=="Yes"
traders$experience <- 2018- as.numeric(substr(traders$trader.q21,5,8))

traders$indep <- traders$trader.q14==1
traders[c("trader.q86","trader.q88", "trader.q91")] <- lapply(traders[c("trader.q86", "trader.q88","trader.q91")], function(x) replace(x,x==999,NA) )
traders$margin <- NA
traders$margin[!is.na(traders$trader.q86)] <- traders$trader.q86[!is.na(traders$trader.q86)]*traders$trader.q88[!is.na(traders$trader.q86)]
traders$margin[!is.na(traders$trader.q91)] <- traders$trader.q91[!is.na(traders$trader.q91)]
traders$margin[traders$margin>650 ] <- NA




### regressions
r1 <-lm(coop~export_ind+age+edu_sec +fem+is_head + hh_size+experience+access_finance + district , data=traders)
r2 <-lm(credit~export_ind+age+edu_sec +fem+is_head + hh_size+experience+access_finance + district , data=traders)
r3 <-lm(use_milkcan_only~export_ind+age+edu_sec +fem+is_head + hh_size +experience+access_finance +district, data=traders)
r4 <-lm(boda~export_ind+age+edu_sec +fem+is_head + hh_size +experience+access_finance +district,data=traders)

stargazer(r1,r2,r3,r4)

library(MatchIt)
traders_cpy <- traders
traders <- traders[complete.cases(traders[c("ID","export_ind","age","edu_sec","fem","is_head","hh_size","experience","access_finance","district")]),]
traders <- traders[c("ID","export_ind","age","edu_sec","fem","is_head","hh_size","experience","access_finance","district")]

set.seed(12345) #matchit randomly picks one of the control subjects that falls within the caliper interval around the treated subject, so set seed to be able to replicate
match.it <- matchit(export_ind~age+edu_sec +fem+is_head + hh_size+experience+access_finance + district, data = traders, caliper=.05,method="nearest")
a <- summary(match.it)

df.match <- match.data(match.it)[1:ncol(traders)]
summary(lm(coop~export_ind, data = merge(df.match,traders_cpy[c("ID","coop")])))



dta <- merge(df.match,traders_cpy[c("ID","coop")])
res <- t.test(dta$coop[dta$export_ind==1],dta$coop[dta$export_ind==0],paired=TRUE)
res$estimate
res$stder
dta <- merge(df.match,traders_cpy[c("ID","credit")])
res <- t.test(dta$credit[dta$export_ind==1],dta$credit[dta$export_ind==0],paired=TRUE)
res$estimate
res$stder
dta <- merge(df.match,traders_cpy[c("ID","use_milkcan_only")])
res <- t.test(dta$use_milkcan_only[dta$export_ind==1],dta$use_milkcan_only[dta$export_ind==0],paired=TRUE)
res$estimate
res$stder
dta <- merge(df.match,traders_cpy[c("ID","boda")])
res <- t.test(dta$boda[dta$export_ind==1],dta$boda[dta$export_ind==0],paired=TRUE)
res$estimate
res$stder



dta <- merge(df.match,traders_cpy[c("ID","credit")])
t.test(dta$credi[dta$export_ind==1],dta$credit[dta$export_ind==0],paired=TRUE)
dta <- merge(df.match,traders_cpy[c("ID","use_milkcan_only")])
t.test(dta$use_milkcan_only[dta$export_ind==1],dta$use_milkcan_only[dta$export_ind==0],paired=TRUE)
dta <- merge(df.match,traders_cpy[c("ID","boda")])
t.test(dta$boda[dta$export_ind==1],dta$boda[dta$export_ind==0],paired=TRUE)


summary(lm(coop~export_ind, data = merge(df.match,traders_cpy[c("ID","coop")])))
summary(lm(credit~export_ind, data = merge(df.match,traders_cpy[c("ID","credit")])))
summary(lm(use_milkcan_only~export_ind, data = merge(df.match,traders_cpy[c("ID","use_milkcan_only")])))
summary(lm(boda~export_ind, data = merge(df.match,traders_cpy[c("ID","boda")])))


