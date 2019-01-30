#create maps
library(ggplot2)
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
ggplot(to_plot_means, aes(type, liters_av)) +   
  geom_bar(aes(fill = outlet), position = "dodge", stat="identity")  +
  coord_flip() + theme(axis.text=element_text(size=12),
    
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
ggradar(df, font.radar = "Times",legend.text.size = 15)
dev.off()







