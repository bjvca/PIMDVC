###question: is it true that farmers keep cattle as a bride price? Do farmers with more female children have more cows?
farmers <- read.csv("/home/bjvca/data/projects/PIMDVC/data/public/farmers.csv")

var <- "gender"
sel <- paste(paste("hh_head.HH.hh_member",1:20, sep="."),var,sep="..")
sel_hh <- c("ID",sel)
source <- farmers[sel_hh]
names(source)[2:21] <- sub("(..gender)","",(names(source)[2:21]))
gender <- reshape( source, varying=names(source)[2:21],direction="long",idvar="ID", sep=".HH.hh_member.")
names(gender)[3] <- "gender"

var <- "q5b" ### relation to HH head
sel <- paste(paste("hh_head.HH.hh_member",1:20, sep="."),var,sep="..")
sel_hh <- c("ID",sel)
source <- farmers[sel_hh]
names(source)[2:21] <- sub("(..q5b)","",(names(source)[2:21]))
relation <- reshape( source, varying=names(source)[2:21],direction="long",idvar="ID", sep=".HH.hh_member.")
names(relation)[3] <- "relation"

var <- "q5c" ### marrital status
sel <- paste(paste("hh_head.HH.hh_member",1:20, sep="."),var,sep="..")
sel_hh <- c("ID",sel)
source <- farmers[sel_hh]
names(source)[2:21] <- sub("(..q5c)","",(names(source)[2:21]))
married <- reshape( source, varying=names(source)[2:21],direction="long",idvar="ID", sep=".HH.hh_member.")
names(married)[3] <- "married"

var <- "q5d" ### age
sel <- paste(paste("hh_head.HH.hh_member",1:20, sep="."),var,sep="..")
sel_hh <- c("ID",sel)
source <- farmers[sel_hh]
names(source)[2:21] <- sub("(..q5d)","",(names(source)[2:21]))
age <- reshape( source, varying=names(source)[2:21],direction="long",idvar="ID", sep=".HH.hh_member.")
names(age)[3] <- "age"

all <- merge(merge(merge(gender,relation, by=c("ID","time")),married, by=c("ID","time")),age, by=c("ID","time") )


#keep only children below 12 and children
all$age <- as.numeric(all$age)
all <- subset(all, age < 12 & (relation == 3 | relation == 4) & married ==2)


all$counter <- 1
all$ind_fem <- all$gender == "Female"


all[all$ID == "F_2",]

children <- aggregate(all[c("counter","ind_fem")],list(all$ID),sum)
names(children)[names(children)=="Group.1"] <- "ID"
children$share_fem <- children$ind_fem/children$counter

#### Now merge to herd size 
farmers[c("hh_head.HH.cattle_ownership.q23", "hh_head.HH.cattle_ownership.q25","hh_head.HH.cattle_ownership.q27","hh_head.HH.cattle_ownership.q29", "hh_head.HH.cattle_ownership.q31","hh_head.HH.cattle_ownership.q33")] <- lapply(farmers[c("hh_head.HH.cattle_ownership.q23", "hh_head.HH.cattle_ownership.q25","hh_head.HH.cattle_ownership.q27","hh_head.HH.cattle_ownership.q29", "hh_head.HH.cattle_ownership.q31","hh_head.HH.cattle_ownership.q33")], function(x) replace(x, x == 999, NA) )
farmers$herdsize <- rowSums(farmers[c("hh_head.HH.cattle_ownership.q23", "hh_head.HH.cattle_ownership.q25","hh_head.HH.cattle_ownership.q27","hh_head.HH.cattle_ownership.q29", "hh_head.HH.cattle_ownership.q31","hh_head.HH.cattle_ownership.q33")], na.rm=T)

farmers <- merge(farmers, children, by = "ID", all.x=T)
farmers$share_fem[is.na(farmers$share_fem)] <- 0

summary(lm(herdsize~share_fem,data=farmers))
### this is significant, but has the wrong sign...


