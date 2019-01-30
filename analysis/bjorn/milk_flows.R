farmers <- read.csv("/home/bjvca/data/projects/PIMDVC/data/public/farmers.csv")
traders <- read.csv("/home/bjvca/data/projects/PIMDVC/data/public/traders.csv")
traders$trader.q39[traders$trader.q39==999] <- NA
traders$trader.q40[traders$trader.q40==999] <- NA
traders$trader.q41[traders$trader.q41==999] <- NA
traders$trader.q42[traders$trader.q42==999] <- NA
traders$trader.q43[traders$trader.q43==999] <- NA
traders$trader.q38[traders$trader.q38==999] <- NA
traders$trader.q44[traders$trader.q44==999] <- NA
traders <- subset(traders,trader.q16!=3)
#1 is trader - 2 is transporter
traders$subcats <- paste(traders$trader.q16, traders$shed, sep="_")
tapply(traders$trader.q39, c(traders$subcats), mean, na.rm=T)
tapply(traders$trader.q40+traders$trader.q41, traders$subcats, mean, na.rm=T)
#processors
tapply(traders$trader.q42, c(traders$subcats), mean, na.rm=T)
#milk shops
tapply(traders$trader.q38 + traders$trader.q43 + traders$trader.q44, c(traders$subcats), mean, na.rm=T)
#institutions
tapply(traders$trader.q43, c(traders$subcats), mean, na.rm=T)
#restaurants
tapply(traders$trader.q44, c(traders$subcats), mean, na.rm=T)

mccs <- read.csv("/home/bjvca/data/projects/PIMDVC/data/public/mccs.csv")

table(mccs$mcc.q29.1[mccs$shed=="C"])














###########################sankey graphs########################################
library(networkD3)
agents <- read.csv("/home/bjvca/data/projects/PIMDVC/analysis/bjorn/nodes.csv", header = FALSE)
names(agents) <- "name"

links <- read.csv("/home/bjvca/data/projects/PIMDVC/analysis/bjorn/links.csv", header = FALSE)
names(links) <- c("source","target","value","group")


sank <- list(agents, links)
names(sank) <- c("nodes","links")

sankeyNetwork(Links = sank$links, Nodes = sank$nodes, Source = "source",LinkGroup="group",
             Target = "target", Value = "value", NodeID = "name",
             units = "Litres ", fontSize = 24, nodeWidth = 30)

### seperate graphs
sank <- list(agents, links[links$group=="C",])
names(sank) <- c("nodes","links")
C_net <- sankeyNetwork(Links = sank$links, Nodes = sank$nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
              units = "Litres ", fontSize = 24, nodeWidth = 30)


sank <- list(agents, links[links$group=="SW",])
names(sank) <- c("nodes","links")
SW_net <- sankeyNetwork(Links = sank$links, Nodes = sank$nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
              units = "Litres ", fontSize = 24, nodeWidth = 30)

saveNetwork(C_net, "/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/C_flow.html", selfcontained = TRUE)
saveNetwork(SW_net, "/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/SW_flow.html", selfcontained = TRUE)

ggsave("/home/bjvca/data/projects/PIMDVC/paper/dairy/innovations/C_flow.pdf", plot = C_net, device = "pdf", path = NULL,
  scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
  dpi = 300, limitsize = TRUE,)

