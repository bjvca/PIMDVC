farmers <- read.csv("/home/bjvca/data/projects/PIMDVC/data/public/farmers.csv")
 
##nr of hh members
t.test(farmers$hh_head.HH.q4~farmers$shed)
##nr of hh members
prop.test(table(farmers$shed,farmers$hh_head.HH.q6 %in% c(3:6)))

farmers$hh_head.HH.distance.q10[farmers$hh_head.HH.distance.q10 == 999] <- NA
t.test(farmers$hh_head.HH.distance.q10~farmers$shed)

farmers$hh_head.HH.distance.q9[farmers$hh_head.HH.distance.q9 == 999] <- NA
t.test(farmers$hh_head.HH.distance.q9~farmers$shed)

farmers$hh_head.HH.distance.q14[farmers$hh_head.HH.distance.q14 == 999] <- NA
t.test(farmers$hh_head.HH.distance.q14~farmers$shed)


## access to electricity
prop.test(table(farmers$shed,farmers$hh_head.HH.Housing.q19!=1))



### land
farmers$hh_head.HH.land.q21[farmers$hh_head.HH.land.q21 == 999] <- NA
t.test(farmers$hh_head.HH.land.q21~farmers$shed)

