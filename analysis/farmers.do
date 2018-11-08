****************************
**	PIM
**	DAIRY FARMERS

eststo clear
clear

gl dir "C:\Users\u0107600\Dropbox\Uganda-Dairy"

 use "$dir\Data\farmers.dta", clear


 
 ***clean data
 rename hh_headHHhh_member_count hh_mems
 
 foreach n of numlist 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20{
 rename hh_headHHhh_member`n'q5d	hhmem`n'_age
 rename hh_headHHhh_member`n'q5c	hhmem`n'_marital
 rename hh_headHHhh_member`n'q5b	hhmem`n'_relhhhead
 rename hh_headHHhh_member`n'gender	hhmem`n'_gender
 }

replace hhmem1_age=. if  hhmem1_age==999

 
 foreach n of numlist  2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20{
replace hhmem`n'_age="." if  hhmem`n'_age=="999"| hhmem`n'_age=="n/a"
destring hhmem`n'_age, replace
 }

gen hhmem1_married=0
replace hhmem1_married=1 if hhmem1_marital==1

  foreach n of numlist  2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20{
gen hhmem`n'_married=.
replace hhmem`n'_married=0 if hhmem`n'_marital!="n/a"
replace hhmem`n'_married=1 if hhmem`n'_marital=="1"
 }
		  foreach n of numlist 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20{
		 order hhmem`n'_married, a(hhmem`n'_marital)
		 }
 
 
 gen hhmem1_head=0
replace hhmem1_head=1 if hhmem1_relhhhead==1

  foreach n of numlist  2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20{
gen hhmem`n'_head=.
replace hhmem`n'_head=0 if hhmem`n'_relhhhead!="n/a"
replace hhmem`n'_head=1 if hhmem`n'_relhhhead=="1"
 }
 
  gen hhmem1_spouse=0
replace hhmem1_spouse=1 if hhmem1_relhhhead==2

  foreach n of numlist  2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20{
gen hhmem`n'_spouse=.
replace hhmem`n'_spouse=0 if hhmem`n'_relhhhead!="n/a"
replace hhmem`n'_spouse=1 if hhmem`n'_relhhhead=="2"
 }
		   foreach n of numlist 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20{
		 order hhmem`n'_head hhmem`n'_spouse, a(hhmem`n'_relhhhead)
		 }
		 
		 
  gen hhmem1_male=0
replace hhmem1_male=1 if hhmem1_gender=="Male"

  foreach n of numlist  2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20{
gen hhmem`n'_male=.
replace hhmem`n'_male=0 if hhmem`n'_gender!="n/a"
replace hhmem`n'_male=1 if hhmem`n'_gender=="Male"
 }
		   foreach n of numlist 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20{
		 order hhmem`n'_male, a(hhmem`n'_gender)
		 }

**HH head characteristics		 
rename hh_headHHq6 head_edu
 gen head_edu_primary = .
 replace head_edu_primary=0 if head_edu!=.
 replace head_edu_primary=1 if head_edu==3 | head_edu==5 |head_edu==6

 gen head_edu_secon = .
 replace head_edu_secon=0 if head_edu!=.
 replace head_edu_secon=1 if head_edu==5 |head_edu==6

 gen head_edu_none = .
 replace head_edu_none=0 if head_edu!=.
 replace head_edu_none=1 if head_edu==1
 
 gen head_edu_some = .
 replace head_edu_some=0 if head_edu!=.
 replace head_edu_some=1 if head_edu>1 & head_edu<7
 
 order head_edu_*, a(head_edu)
 
  gen head_rel_angli=.
 replace  head_rel_angli =0 if hh_headHHq7!=.
 replace  head_rel_angli =1 if hh_headHHq7==1
 gen head_rel_pent=.
 replace  head_rel_pent =0 if hh_headHHq7!=.
 replace  head_rel_pent =1 if hh_headHHq7==2
  gen head_rel_cath=.
 replace  head_rel_cath =0 if hh_headHHq7!=.
 replace  head_rel_cath =1 if hh_headHHq7==3
   gen head_rel_mus=.
 replace  head_rel_mus =0 if hh_headHHq7!=.
 replace  head_rel_mus =1 if hh_headHHq7==4
 
 order head_rel*, a(hh_headHHq7)

 


  gen head_tribe_Banyankore=.
 replace  head_tribe_Banyankore =0 if hh_headHHq8!=.
 replace  head_tribe_Banyankore =1 if hh_headHHq8==2
 gen head_tribe_Banyarwanda=.
 replace  head_tribe_Banyarwanda =0 if hh_headHHq8!=.
 replace  head_tribe_Banyarwanda =1 if hh_headHHq8==6
  gen head_tribe_Banyoro=.
 replace  head_tribe_Banyoro =0 if hh_headHHq8!=.
 replace  head_tribe_Banyoro =1 if hh_headHHq8==5

 
 order head_tribe*, a(hh_headHHq8)
 
 
 foreach v of numlist 9 10 11 12 13 14 15 16{
 replace hh_headHHdistanceq`v'=. if `v'==999
 }
 *
 rename hh_headHHdistanceq9 dist_tarmac
 rename hh_headHHdistanceq10 dist_murram
 rename hh_headHHdistanceq11 dist_mcc
 rename hh_headHHdistanceq12 dist_milkshop
 rename hh_headHHdistanceq13 dist_neigh
 rename hh_headHHdistanceq14 dist_market
 rename hh_headHHdistanceq15 dist_tradecenter
 rename hh_headHHdistanceq16 dist_vilchairm
 
 *95 % have some cell phone coverage
 gen cellrec_good=. 
 replace cellrec_good=0 if hh_headHHdistanceq16b!=3
  replace cellrec_good=1 if hh_headHHdistanceq16b==1
order cellrec_good, a(hh_headHHdistanceq16b)

gen main_income_dairy=.
replace main_income_dairy=0 if hh_headHHHousingq17!=.
replace main_income_dairy=1 if hh_headHHHousingq17==2
 
gen main_income_crops=.
replace main_income_crops=0 if hh_headHHHousingq17!=.
replace main_income_crops=1 if hh_headHHHousingq17==1

order main_income_*, a(hh_headHHHousingq17)

rename hh_headHHHousingq18 hh_rooms
replace hh_rooms=. if hh_rooms==999

gen hh_solarpower=.
replace hh_solarpower=0 if hh_headHHHousingq19!=96
replace hh_solarpower=1 if hh_headHHHousingq19==3

gen hh_ironroof=.
replace hh_ironroof=0 if hh_headHHHousingq20!=96
replace hh_ironroof=1 if hh_headHHHousingq20==2

order hh_rooms hh_solarpower hh_ironroof, a( hh_headHHHousingq20)

rename hh_headHHlandq21 land_area
replace land_area=. if land_area==999

rename hh_headHHlandq21b land_areafenced

rename hh_headHHlandq22 land_area_useable
replace land_area_useable=. if land_area_useable==999

rename hh_headHHcattle_ownershipq23 cattle_localcowowned
rename hh_headHHcattle_ownershipq24 cattle_localcowprice
rename hh_headHHcattle_ownershipq25 cattle_localheiferown
rename hh_headHHcattle_ownershipq26 cattle_localheiferprice
rename hh_headHHcattle_ownershipq27 cattle_localcalvesowned
rename hh_headHHcattle_ownershipq28 cattle_localcalveprice
rename hh_headHHcattle_ownershipq29 cattle_improvedcowowned
rename hh_headHHcattle_ownershipq30 cattle_improvedcowprice
rename hh_headHHcattle_ownershipq31 cattle_improvedheiferown
rename hh_headHHcattle_ownershipq32 cattle_improvedheiferprice
rename hh_headHHcattle_ownershipq33 cattle_improvedcalvesown
rename hh_headHHcattle_ownershipq34 cattle_improvedcalvesprice
rename hh_headHHcattle_ownershipq34b cattle_heardsizelastyear
rename hh_headHHcattle_ownershipq34c cattle_heardsizelastyearimproved


foreach v of varlist cattle_localcowprice cattle_localheiferprice cattle_localcalveprice cattle_improvedcowprice cattle_improvedheiferprice cattle_improvedcalvesprice cattle_heardsizelastyearimproved{
replace `v'="." if `v'=="999"|`v'=="n/a"
destring `v', replace
}

foreach v of varlist cattle_*{
replace `v'=. if `v'==999
}

rename hh_headHHdairy_ouputq35 dry_localcows
rename hh_headHHdairy_ouputq37  dry_localcows_abv8
rename hh_headHHdairy_ouputq38 dry_localcows_liquid
rename hh_headHHdairy_ouputq39 dry_exotics
rename hh_headHHdairy_ouputq41 dry_exotics_abv8
rename hh_headHHdairy_ouputq42 dry_exotics_liqud
rename hh_headHHdairy_ouputq43 dry_price
rename hh_headHHdairy_ouputtotal dry_total

rename hh_headHHdairy_ouputqX1 dry_lowestpric
rename hh_headHHdairy_ouputqX2 dry_highestpric
rename hh_headHHdairy_ouputqX3 dry_sellday
rename hh_headHHdairy_ouputqX4 dry_consumeday
rename hh_headHHdairy_ouputqX5 dry_givenday
rename hh_headHHdairy_ouputqX6 dry_calvesday
rename hh_headHHdairy_ouputqX7 dry_processday


rename hh_headHHdairy_ouputq44 rainy_localcows
rename hh_headHHdairy_ouputq46  rainy_localcows_abv8
rename hh_headHHdairy_ouputq47 rainy_localcows_liquid
rename hh_headHHdairy_ouputq48 rainy_exotics
rename hh_headHHdairy_ouputq50 rainy_exotics_abv8
rename hh_headHHdairy_ouputq51 rainy_exotics_liqud
rename hh_headHHdairy_ouputq52 rainy_price
rename hh_headHHdairy_ouputtotal2 rainy_total

rename hh_headHHdairy_ouputq53 rainy_lowestpric
rename hh_headHHdairy_ouputq54 rainy_highestpric
rename hh_headHHdairy_ouputq55 rainy_sellday
rename hh_headHHdairy_ouputq56 rainy_consumeday
rename hh_headHHdairy_ouputq57 rainy_givenday
rename hh_headHHdairy_ouputq58 rainy_calvesday
rename hh_headHHdairy_ouputq59 rainy_processday

rename hh_headHHsalesq701 sales_directneighbor
rename hh_headHHsalesq702 sales_tradetoretail
rename hh_headHHsalesq703 sales_tradetomcc
rename hh_headHHsalesq704 sales_tradetomilkshop
rename hh_headHHsalesq705 sales_transptoretail
rename hh_headHHsalesq706 sales_transptomcc
rename hh_headHHsalesq707 sales_transptomilkshop
rename hh_headHHsalesq708 sales_directmcc
rename hh_headHHsalesq709 sales_directmilkshop
rename hh_headHHsalesq7096 sales_other

rename hh_headHHsalesq71 sales_neigh_trans
rename hh_headHHsalesq72 sales_neigh_num
rename hh_headHHsalesq73 sales_neigh_price
rename hh_headHHsalesq74 sales_neigh_amt
rename hh_headHHsalesq75 sales_neigh_dec
rename hh_headHHsalesq76 sales_neigh_whosold
rename hh_headHHsalesq77 sales_neigh_paym
rename hh_headHHsalesq78 sales_neigh_freqpaym
rename hh_headHHsalesq79 sales_neigh_contract
rename hh_headHHsalesq80 sales_neigh_yrs
rename hh_headHHsalesq81 sales_neigh_qual
rename hh_headHHsalesq82 sales_neigh_prem
rename hh_headHHsalesq83 sales_neigh_assis
rename hh_headHHsalesq841 sales_neigh_assis_train
rename hh_headHHsalesq842 sales_neigh_assis_input
rename hh_headHHsalesq843 sales_neigh_assis_credit

rename hh_headHHsalesq84b sales_neigh_assishowboil

rename hh_headHHsalesq85 sales_tr_retail_trans
rename hh_headHHsalesq86 sales_tr_retail_num
rename hh_headHHsalesq87 sales_tr_retail_price
rename hh_headHHsalesq88 sales_tr_retail_amt
rename hh_headHHsalesq89 sales_tr_retail_dec
rename hh_headHHsalesq90 sales_tr_retail_whosold
rename hh_headHHsalesq91 sales_tr_retail_paym
rename hh_headHHsalesq92 sales_tr_retail_freqpaym
rename hh_headHHsalesq93 sales_tr_retail_contract
rename hh_headHHsalesq94 sales_tr_retail_yrs
rename hh_headHHsalesq95 sales_tr_retail_qual
rename hh_headHHsalesq96 sales_tr_retail_prem
rename hh_headHHsalesq97 sales_tr_retail_assis
rename hh_headHHsalesq981 sales_tr_retail_assis_train
rename hh_headHHsalesq982 sales_tr_retail_assis_input
rename hh_headHHsalesq983 sales_tr_retail_assis_credit
rename hh_headHHsalesq99 sales_tr_retail_transp
rename hh_headHHsalesq100 sales_tr_retail_contain
rename hh_headHHsalesq101 sales_tr_retail_howpay
rename hh_headHHsalesq102 sales_tr_retail_amtpay


rename hh_headHHsalesq103 sales_tr_mcc_trans
rename hh_headHHsalesq104 sales_tr_mcc_num
rename hh_headHHsalesq105 sales_tr_mcc_price
rename hh_headHHsalesq106 sales_tr_mcc_amt
rename hh_headHHsalesq107 sales_tr_mcc_dec
rename hh_headHHsalesq108 sales_tr_mcc_whosold
rename hh_headHHsalesq109 sales_tr_mcc_paym
rename hh_headHHsalesq110 sales_tr_mcc_freqpaym
rename hh_headHHsalesq111 sales_tr_mcc_contract
rename hh_headHHsalesq112 sales_tr_mcc_yrs
rename hh_headHHsalesq113 sales_tr_mcc_qual
rename hh_headHHsalesq114 sales_tr_mcc_prem
rename hh_headHHsalesq115 sales_tr_mcc_assis
rename hh_headHHsalesq1161 sales_tr_mcc_assis_train
rename hh_headHHsalesq1162 sales_tr_mcc_assis_input
rename hh_headHHsalesq1163 sales_tr_mcc_assis_credit
rename hh_headHHsalesq117 sales_tr_mcc_transp
rename hh_headHHsalesq118 sales_tr_mcc_contain
rename hh_headHHsalesq119 sales_tr_mcc_howpay
rename hh_headHHsalesq120 sales_tr_mcc_amtpay


rename hh_headHHsalesq121 sales_tr_mklshp_trans
rename hh_headHHsalesq122 sales_tr_mklshp_num
rename hh_headHHsalesq123 sales_tr_mklshp_price
rename hh_headHHsalesq124 sales_tr_mklshp_amt
rename hh_headHHsalesq125 sales_tr_mklshp_dec
rename hh_headHHsalesq126 sales_tr_mklshp_whosold
rename hh_headHHsalesq127 sales_tr_mklshp_paym
rename hh_headHHsalesq128 sales_tr_mklshp_freqpaym
rename hh_headHHsalesq129 sales_tr_mklshp_contract
rename hh_headHHsalesq130 sales_tr_mklshp_yrs
rename hh_headHHsalesq131 sales_tr_mklshp_qual
rename hh_headHHsalesq132 sales_tr_mklshp_prem
rename hh_headHHsalesq133 sales_tr_mklshp_assis
rename hh_headHHsalesq1341 sales_tr_mklshp_assis_train
rename hh_headHHsalesq1342 sales_tr_mklshp_assis_input
rename hh_headHHsalesq1343 sales_tr_mklshp_assis_credit
rename hh_headHHsalesq135 sales_tr_mklshp_transp
rename hh_headHHsalesq136 sales_tr_mklshp_contain
rename hh_headHHsalesq137 sales_tr_mklshp_howpay
rename hh_headHHsalesq138 sales_tr_mklshp_amtpay



rename hh_headHHsalesq139 sales_trans_retail_trans
rename hh_headHHsalesq140 sales_trans_retail_num
rename hh_headHHsalesq141 sales_trans_retail_price
rename hh_headHHsalesq142 sales_trans_retail_amt
rename hh_headHHsalesq143 sales_trans_retail_dec
rename hh_headHHsalesq144 sales_trans_retail_whosold
rename hh_headHHsalesq145 sales_trans_retail_paym
rename hh_headHHsalesq146 sales_trans_retail_freqpaym
rename hh_headHHsalesq147 sales_trans_retail_contract
rename hh_headHHsalesq148 sales_trans_retail_yrs
rename hh_headHHsalesq149 sales_trans_retail_qual
rename hh_headHHsalesq150 sales_trans_retail_prem
rename hh_headHHsalesq151 sales_trans_retail_assis
rename hh_headHHsalesq1521 sales_trans_retail_assis_train
rename hh_headHHsalesq1522 sales_trans_retail_assis_input
rename hh_headHHsalesq1523 sales_trans_retail_assis_credit
rename hh_headHHsalesq153 sales_trans_retail_transp
rename hh_headHHsalesq154 sales_trans_retail_transpcost
rename hh_headHHsalesq155 sales_trans_retail_contain
rename hh_headHHsalesq156 sales_trans_retail_testlact
rename hh_headHHsalesq157 sales_trans_retail_testalc
rename hh_headHHsalesq157b sales_trans_retail_transptype
rename hh_headHHsalesq157c sales_trans_retail_transpcostrec


rename hh_headHHsalesq158 sales_trans_mcc_trans
rename hh_headHHsalesq159 sales_trans_mcc_num
rename hh_headHHsalesq160 sales_trans_mcc_price
rename hh_headHHsalesq161 sales_trans_mcc_amt
rename hh_headHHsalesq162 sales_trans_mcc_dec
rename hh_headHHsalesq163 sales_trans_mcc_whosold
rename hh_headHHsalesq164 sales_trans_mcc_paym
rename hh_headHHsalesq165 sales_trans_mcc_freqpaym
rename hh_headHHsalesq166 sales_trans_mcc_contract
rename hh_headHHsalesq167 sales_trans_mcc_yrs
rename hh_headHHsalesq168 sales_trans_mcc_qual
rename hh_headHHsalesq169 sales_trans_mcc_prem
rename hh_headHHsalesq170 sales_trans_mcc_assis
rename hh_headHHsalesq1711 sales_trans_mcc_assis_train
rename hh_headHHsalesq1712 sales_trans_mcc_assis_input
rename hh_headHHsalesq1713 sales_trans_mcc_assis_credit
rename hh_headHHsalesq172 sales_trans_mcc_transp
rename hh_headHHsalesq173 sales_trans_mcc_transpcost
rename hh_headHHsalesq174 sales_trans_mcc_contain
rename hh_headHHsalesq175 sales_trans_mcc_testlact
rename hh_headHHsalesq176 sales_trans_mcc_testalc
rename hh_headHHsalesq176b sales_trans_mcc_transptype
rename hh_headHHsalesq176c sales_trans_mcc_transpcostrec



rename hh_headHHsalesR1 sales_trans_mklshp_trans
rename hh_headHHsalesR2 sales_trans_mklshp_num
rename hh_headHHsalesR3 sales_trans_mklshp_price
rename hh_headHHsalesR4 sales_trans_mklshp_amt
rename hh_headHHsalesR5 sales_trans_mklshp_dec
rename hh_headHHsalesR6 sales_trans_mklshp_whosold
rename hh_headHHsalesR7 sales_trans_mklshp_paym
rename hh_headHHsalesR8 sales_trans_mklshp_freqpaym
rename hh_headHHsalesR9 sales_trans_mklshp_contract
rename hh_headHHsalesR10 sales_trans_mklshp_yrs
rename hh_headHHsalesR11 sales_trans_mklshp_qual
rename hh_headHHsalesR12 sales_trans_mklshp_prem
rename hh_headHHsalesR13 sales_trans_mklshp_assis
rename hh_headHHsalesR141 sales_trans_mklshp_assis_train
rename hh_headHHsalesR142 sales_trans_mklshp_assis_input
rename hh_headHHsalesR143 sales_trans_mklshp_assis_credit
rename hh_headHHsalesR15 sales_trans_mklshp_transp
rename hh_headHHsalesR16 sales_trans_mklshp_transpcost
rename hh_headHHsalesR17 sales_trans_mklshp_contain
rename hh_headHHsalesR18 sales_trans_mklshp_testlact
rename hh_headHHsalesR19 sales_trans_mklshp_testalc
rename hh_headHHsalesR19b sales_trans_mklshp_transptype
rename hh_headHHsalesR19c sales_trans_mklshp_transpcostrec


rename hh_headHHsalesR20 sales_direct_mcc_trans
rename hh_headHHsalesR21 sales_direct_mcc_num
rename hh_headHHsalesR23 sales_direct_mcc_price
rename hh_headHHsalesR24 sales_direct_mcc_amt
rename hh_headHHsalesR25 sales_direct_mcc_dec
rename hh_headHHsalesR26 sales_direct_mcc_whosold
rename hh_headHHsalesR27 sales_direct_mcc_paym
rename hh_headHHsalesR28 sales_direct_mcc_freqpaym
rename hh_headHHsalesR29 sales_direct_mcc_contract
rename hh_headHHsalesR30 sales_direct_mcc_yrs
rename hh_headHHsalesR31 sales_direct_mcc_qual
rename hh_headHHsalesR32 sales_direct_mcc_prem
rename hh_headHHsalesR33 sales_direct_mcc_assis
rename hh_headHHsalesR341 sales_direct_mcc_assis_train
rename hh_headHHsalesR342 sales_direct_mcc_assis_input
rename hh_headHHsalesR343 sales_direct_mcc_assis_credit
rename hh_headHHsalesR35 sales_direct_mcc_transp
rename hh_headHHsalesR36 sales_direct_mcc_transpcost
rename hh_headHHsalesR37 sales_direct_mcc_contain
rename hh_headHHsalesR38 sales_direct_mcc_testlact
rename hh_headHHsalesR39 sales_direct_mcc_testalc



rename hh_headHHsalesR40 sales_direct_mlkshp_trans
rename hh_headHHsalesR41 sales_direct_mlkshp_num
rename hh_headHHsalesR42 sales_direct_mlkshp_price
rename hh_headHHsalesR43 sales_direct_mlkshp_amt
rename hh_headHHsalesR44 sales_direct_mlkshp_dec
rename hh_headHHsalesR45 sales_direct_mlkshp_whosold
rename hh_headHHsalesR46 sales_direct_mlkshp_paym
rename hh_headHHsalesR47 sales_direct_mlkshp_freqpaym
rename hh_headHHsalesR48 sales_direct_mlkshp_contract
rename hh_headHHsalesR49 sales_direct_mlkshp_yrs
rename hh_headHHsalesR50 sales_direct_mlkshp_qual
rename hh_headHHsalesR51 sales_direct_mlkshp_prem
rename hh_headHHsalesR52 sales_direct_mlkshp_assis
rename hh_headHHsalesR531 sales_direct_mlkshp_assis_train
rename hh_headHHsalesR532 sales_direct_mlkshp_assis_input
rename hh_headHHsalesR533 sales_direct_mlkshp_assis_credit
rename hh_headHHsalesR54 sales_direct_mlkshp_transp
rename hh_headHHsalesR55 sales_direct_mlkshp_transpcost
rename hh_headHHsalesR56 sales_direct_mlkshp_contain
rename hh_headHHsalesR57 sales_direct_mlkshp_testlact
rename hh_headHHsalesR58 sales_direct_mlkshp_testalc

rename hh_headHHq177 nosale_unable
rename hh_headHHq178 nosale_unable_time
rename hh_headHHq179 nosale_unable_why
rename hh_headHHq1801 nosale_unable_bongo
rename hh_headHHq1802 nosale_unable_ghee
rename hh_headHHq1803 nosale_unable_give
rename hh_headHHq1804 nosale_unable_animal
rename hh_headHHq1805 nosale_unable_throw

rename hh_headHHfood_safetyq181 safet_wheremilked
rename hh_headHHfood_safetyq182 safet_wheremilkedfloor
rename hh_headHHfood_safetyq183 safet_cleantimes
rename hh_headHHfood_safetyq184 safet_cleanhow
rename hh_headHHfood_safetyq185 safet_utterclean
rename hh_headHHfood_safetyq186 safet_uttercleanhow
rename hh_headHHfood_safetyq187 safet_milkcream
rename hh_headHHfood_safetyq188 safet_uttertreat
rename hh_headHHfood_safetyq189 safet_cowstand
rename hh_headHHfood_safetyq1901 safet_contain_glass
rename hh_headHHfood_safetyq1902 safet_contain_plasjar
rename hh_headHHfood_safetyq1903 safet_contain_plasbuck
rename hh_headHHfood_safetyq1904 safet_contain_steelbuc
rename hh_headHHfood_safetyq1905 safet_contain_jerryc
rename hh_headHHfood_safetyq1906 safet_contain_steelcan



rename hh_headHHfood_safetyq191 safet_cowsleep
rename hh_headHHfood_safetyq192 safet_handwash
rename hh_headHHfood_safetyq193 safet_washcontain
rename hh_headHHfood_safetyq194 safet_drycontain

rename hh_headHHfood_safetyS1 safet_prob_ticks
rename hh_headHHfood_safetyS2 safet_prob_fever

rename hh_headHHfood_safetyq195 safet_spraycow
rename  hh_headHHfood_safetyq1961 safet_feed_free
rename  hh_headHHfood_safetyq1962 safet_feed_paddock 
rename hh_headHHfood_safetyq1963 safet_feed_tether
rename  hh_headHHfood_safetyq1964 safet_feed_zerograze
rename  hh_headHHfood_safetyq1965 safet_feed_freesup

rename hh_headHHfood_safetyq198 safet_watersource
rename hh_headHHfood_safetyq198b safet_watersource_prob

rename hh_headHHanimal_healthq199 anhealth_vac
rename  hh_headHHanimal_healthq200 anhealth_vacwho
rename  hh_headHHanimal_healthq201  anhealth_vvetdist
rename hh_headHHanimal_healthq202 anhealth_vetmeddist
rename  hh_headHHanimal_healthq203 anhealth_vet12mo
rename  hh_headHHanimal_healthq204 anhealth_vetnum
rename  hh_headHHanimal_healthq205 anhealth_vetcost

rename hh_headHHtrainingq206 training
rename hh_headHHtrainingq207 training_who


rename hh_headHHtraininglabor1 labor_admale
rename  hh_headHHtraininglabor2 labor_adfemale
rename  hh_headHHtraininglabor3 labor_childmale
rename  hh_headHHtraininglabor4 labor_childfemale

rename hh_headHHtrainingq209  labor_admale_milkam
rename  hh_headHHtrainingq210 labor_admale_milkpm
rename  hh_headHHtrainingq211 labor_admale_market
rename  hh_headHHtrainingq212 labor_admale_feed
rename  hh_headHHtrainingq213 labor_admale_cleaning

rename hh_headHHtrainingq214  labor_adfemale_milkam
rename  hh_headHHtrainingq215 labor_adfemale_milkpm
rename  hh_headHHtrainingq216 labor_adfemale_market
rename  hh_headHHtrainingq217 labor_adfemale_feed
rename  hh_headHHtrainingq218 labor_adfemale_cleaning

rename hh_headHHtrainingq219 labor_childmale_milkam
rename  hh_headHHtrainingq220 labor_childmale_milkpm
rename  hh_headHHtrainingq221 labor_childmale_market
rename  hh_headHHtrainingq222 labor_childmale_feed
rename  hh_headHHtrainingq223 labor_childmale_cleaning


rename hh_headHHtrainingq224 labor_childfemale_milkam
rename  hh_headHHtrainingq225 labor_childfemale_milkpm
rename  hh_headHHtrainingq226 labor_childfemale_market
rename  hh_headHHtrainingq227 labor_childfemale_feed
rename  hh_headHHtrainingq228 labor_childfemale_cleaning

rename hh_headHHtrainingq229 labor_hired
rename  hh_headHHtrainingq230 labor_hired_hours
rename  hh_headHHtrainingq231 labor_hired_cost

rename hh_headHHq2321 whycattle_wealth
rename  hh_headHHq2322 whycattle_prestig
rename  hh_headHHq2323  whycattle_prodmilk
rename hh_headHHq2324  whycattle_prodmeat
rename hh_headHHq2325 whycattle_bridep

rename hh_headHHrecallq233 tenyrs_cows
rename  hh_headHHrecallq233b start_whenstart
rename  hh_headHHrecallq234 start_exoticsnum
rename  hh_headHHrecallq235  start_localnum
rename hh_headHHrecallq236 start_milkperday
rename hh_headHHrecallq237 start_sellperday
rename  hh_headHHrecallq238 start_selltrader

rename hh_headHHlivestock_assetsq239 assets_milkshed
rename  hh_headHHlivestock_assetsq240 assets_milkshed_roof
rename  hh_headHHlivestock_assetsq241 assets_barn
rename  hh_headHHlivestock_assetsq242 assets_trough
rename  hh_headHHlivestock_assetsq243 assets_steelcan
rename  hh_headHHlivestock_assetsq244 assets_steelcannum
rename  hh_headHHlivestock_assetsq245 assets_steelbucsteelbuck
rename  hh_headHHlivestock_assetsq246 assets_steelbucsteelbucknum 
rename  hh_headHHlivestock_assetsq247 assets_jerryc 
rename  hh_headHHlivestock_assetsq248 assets_jerrycnum 
rename  hh_headHHlivestock_assetsq249 assets_plasbuck 
rename  hh_headHHlivestock_assetsq250 assets_plasbucknum
rename  hh_headHHlivestock_assetsq251 assets_mobilenum
rename  hh_headHHlivestock_assetsq252 assets_motonum
rename  hh_headHHlivestock_assetsq253 assets_bikenum
rename  hh_headHHlivestock_assetsq254 assets_carsnum
rename  hh_headHHlivestock_assetsq255 assets_radionum
rename  hh_headHHlivestock_assetsq256 assets_waterpumpnum
rename  hh_headHHlivestock_assetsq257 assets_pangasnum
rename  hh_headHHlivestock_assetsq258 assets_wheelbnum
rename  hh_headHHlivestock_assetsq259 assets_sicklenum
rename  hh_headHHlivestock_assetsq260 assets_hayforknum
rename  hh_headHHlivestock_assetsq261 assets_tarpaulinsnum
rename  hh_headHHlivestock_assetsq262 assets_cuttingmachnum
rename  hh_headHHlivestock_assetsq263 assets_horsepipnum
rename  hh_headHHlivestock_assetsq264 assets_sprayersnum

rename  hh_headHHcooperativeq265 coop_
rename  hh_headHHcooperativeq266 coop_num
rename  hh_headHHcooperativeq267 coop_nondairy

rename  hh_headHHaccess_financeq268 fina_ac
rename  hh_headHHaccess_financeq2691 fina_coop
rename  hh_headHHaccess_financeq2692 fina_bank
rename  hh_headHHaccess_financeq2693 fina_friend
rename  hh_headHHaccess_financeq2694 fina_vsla

rename  hh_headHHaccess_financeq270 fina_loan
rename  hh_headHHaccess_financeq271 fina_loanamt
rename  hh_headHHaccess_financeq272 fina_inv
rename  hh_headHHaccess_financeq2731 fina_inv_impbred
rename  hh_headHHaccess_financeq2732 fina_inv_local
rename  hh_headHHaccess_financeq2733 fina_inv_buycan
rename  hh_headHHaccess_financeq2734 fina_inv_treatan
rename  hh_headHHaccess_financeq2735 fina_inv_hirelabor
rename  hh_headHHaccess_financeq2736 fina_inv_artinf
rename  hh_headHHaccess_financeq2737 fina_inv_othermilk
foreach v of varlist fina_*{
replace `v'="." if `v'=="n/a"
destring `v', replace
}

rename hh_headHHchild hh_childU5
rename hh_headHHL1 hh_childU5_bf
rename  hh_headHHL2 hh_childU5_milk7d
rename  hh_headHHL3 hh_childU5_milklit
foreach v of varlist hh_childU5*{
replace `v'="." if `v'=="n/a"
destring `v', replace
}
foreach v of varlist hh_headHHconsumption*{
replace `v'="." if `v'=="999"|`v'=="n/a"
destring `v', replace
}
*
















