****************************
**	PIM
**	DAIrY FArMErS

eststo clear
clear

gl dir "C:\Users\u0107600\Dropbox\Uganda-Dairy"

* use "$dir\Data\farmers.dta", clear
 use "$dir\analysis\farmers_insheet.dta", clear
*insheet using "https://raw.githubusercontent.com/bjvca/PIMDVC/master/data/public/farmers.csv", clear

 
 ***clean data
 rename hh_headhhhh_member_count hh_mems
 
 foreach n of numlist 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20{
 rename hh_headhhhh_member`n'q5d	hhmem`n'_age
 rename hh_headhhhh_member`n'q5c	hhmem`n'_marital
 rename hh_headhhhh_member`n'q5b	hhmem`n'_relhhhead
 rename hh_headhhhh_member`n'gender	hhmem`n'_gender
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

**hh head characteristics		 
rename hh_headhhq6 head_edu
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
 replace  head_rel_angli =0 if hh_headhhq7!=.
 replace  head_rel_angli =1 if hh_headhhq7==1
 gen head_rel_pent=.
 replace  head_rel_pent =0 if hh_headhhq7!=.
 replace  head_rel_pent =1 if hh_headhhq7==2
  gen head_rel_cath=.
 replace  head_rel_cath =0 if hh_headhhq7!=.
 replace  head_rel_cath =1 if hh_headhhq7==3
   gen head_rel_mus=.
 replace  head_rel_mus =0 if hh_headhhq7!=.
 replace  head_rel_mus =1 if hh_headhhq7==4
 
 order head_rel*, a(hh_headhhq7)

 


  gen head_tribe_Banyankore=.
 replace  head_tribe_Banyankore =0 if hh_headhhq8!=.
 replace  head_tribe_Banyankore =1 if hh_headhhq8==2
 gen head_tribe_Banyarwanda=.
 replace  head_tribe_Banyarwanda =0 if hh_headhhq8!=.
 replace  head_tribe_Banyarwanda =1 if hh_headhhq8==6
  gen head_tribe_Banyoro=.
 replace  head_tribe_Banyoro =0 if hh_headhhq8!=.
 replace  head_tribe_Banyoro =1 if hh_headhhq8==5

 
 order head_tribe*, a(hh_headhhq8)
 
 
 foreach v of numlist 9 10 11 12 13 14 15 16{
 replace hh_headhhdistanceq`v'=. if hh_headhhdistanceq`v'==999
 }
 *
 rename hh_headhhdistanceq9 dist_tarmac
 rename hh_headhhdistanceq10 dist_murram
 rename hh_headhhdistanceq11 dist_mcc
 rename hh_headhhdistanceq12 dist_milkshop
 rename hh_headhhdistanceq13 dist_neigh
 rename hh_headhhdistanceq14 dist_market
 rename hh_headhhdistanceq15 dist_tradecenter
 rename hh_headhhdistanceq16 dist_vilchairm
 
 *95 % have some cell phone coverage
 gen cellrec_good=. 
 replace cellrec_good=0 if hh_headhhdistanceq16b!=3
  replace cellrec_good=1 if hh_headhhdistanceq16b==1
order cellrec_good, a(hh_headhhdistanceq16b)

gen main_income_dairy=.
replace main_income_dairy=0 if hh_headhhhousingq17!=.
replace main_income_dairy=1 if hh_headhhhousingq17==2
 
gen main_income_crops=.
replace main_income_crops=0 if hh_headhhhousingq17!=.
replace main_income_crops=1 if hh_headhhhousingq17==1

order main_income_*, a(hh_headhhhousingq17)

rename hh_headhhhousingq18 hh_rooms
replace hh_rooms=. if hh_rooms==999

gen hh_solarpower=.
replace hh_solarpower=0 if hh_headhhhousingq19!=96
replace hh_solarpower=1 if hh_headhhhousingq19==3

gen hh_ironroof=.
replace hh_ironroof=0 if hh_headhhhousingq20!=96
replace hh_ironroof=1 if hh_headhhhousingq20==2

order hh_rooms hh_solarpower hh_ironroof, a( hh_headhhhousingq20)

rename hh_headhhlandq21 land_area
replace land_area=. if land_area==999

rename hh_headhhlandq21b land_areafenced

rename hh_headhhlandq22 land_area_useable
replace land_area_useable=. if land_area_useable==999

rename hh_headhhcattle_ownershipq23 cattle_localcowowned
rename hh_headhhcattle_ownershipq24 cattle_localcowprice
rename hh_headhhcattle_ownershipq25 cattle_localheiferown
rename hh_headhhcattle_ownershipq26 cattle_localheiferprice
rename hh_headhhcattle_ownershipq27 cattle_localcalvesowned
rename hh_headhhcattle_ownershipq28 cattle_localcalveprice
rename hh_headhhcattle_ownershipq29 cattle_improvedcowowned
rename hh_headhhcattle_ownershipq30 cattle_improvedcowprice
rename hh_headhhcattle_ownershipq31 cattle_improvedheiferown
rename hh_headhhcattle_ownershipq32 cattle_improvedheiferprice
rename hh_headhhcattle_ownershipq33 cattle_improvedcalvesown
rename hh_headhhcattle_ownershipq34 cattle_improvedcalvesprice
rename hh_headhhcattle_ownershipq34b cattle_herdsizelastyear
rename hh_headhhcattle_ownershipq34c cattle_herdsizelastyearimproved


foreach v of varlist cattle_localcowprice cattle_localheiferprice cattle_localcalveprice cattle_improvedcowprice cattle_improvedheiferprice cattle_improvedcalvesprice cattle_herdsizelastyearimproved{
replace `v'="." if `v'=="999"|`v'=="n/a"
destring `v', replace
}

foreach v of varlist cattle_*{
replace `v'=. if `v'==999
}

rename hh_headhhdairy_ouputq35 dry_localcows
rename hh_headhhdairy_ouputq37  dry_localcows_abv8
rename hh_headhhdairy_ouputq38 dry_localcows_liquid
rename hh_headhhdairy_ouputq39 dry_exotics
rename hh_headhhdairy_ouputq41 dry_exotics_abv8
rename hh_headhhdairy_ouputq42 dry_exotics_liqud
rename hh_headhhdairy_ouputq43 dry_price
rename hh_headhhdairy_ouputtotal dry_total

rename hh_headhhdairy_ouputqx1 dry_lowestpric
rename hh_headhhdairy_ouputqx2 dry_highestpric
rename hh_headhhdairy_ouputqx3 dry_sellday
rename hh_headhhdairy_ouputqx4 dry_consumeday
rename hh_headhhdairy_ouputqx5 dry_givenday
rename hh_headhhdairy_ouputqx6 dry_calvesday
rename hh_headhhdairy_ouputqx7 dry_processday

foreach v of varlist dry_localcows_abv8 dry_localcows_liquid dry_lowestpric dry_highestpric dry_sellday dry_exotics_abv8 dry_exotics_liqud dry_price dry_total dry_consumeday dry_givenday dry_calvesday dry_processday{
replace `v'="." if `v'=="999"|`v'=="n/a"|`v'=="99"
destring `v', replace
}

foreach v of varlist dry_localcows dry_exotics{
replace `v'=. if `v'==999

}
gen dry_localcows_liquid_total= dry_localcows_liquid*dry_localcows
gen dry_exotics_liqud_total=dry_exotics_liqud*dry_exotics

gen dry_total_liq=dry_localcows_liquid_total+dry_exotics_liqud_total
replace  dry_total_liq=dry_localcows_liquid_total if dry_exotics_liqud_total==. & dry_total_liq==.
replace  dry_total_liq=dry_exotics_liqud_total if  dry_localcows_liquid_total==. & dry_total_liq==.


order dry_localcows_liquid_total dry_exotics_liqud_total dry_total_liq, a (dry_processday)


rename hh_headhhdairy_ouputq44 rainy_localcows
rename hh_headhhdairy_ouputq46  rainy_localcows_abv8
rename hh_headhhdairy_ouputq47 rainy_localcows_liquid
rename hh_headhhdairy_ouputq48 rainy_exotics
rename hh_headhhdairy_ouputq50 rainy_exotics_abv8
rename hh_headhhdairy_ouputq51 rainy_exotics_liqud
rename hh_headhhdairy_ouputq52 rainy_price
rename hh_headhhdairy_ouputtotal2 rainy_total

rename hh_headhhdairy_ouputq53 rainy_lowestpric
rename hh_headhhdairy_ouputq54 rainy_highestpric
rename hh_headhhdairy_ouputq55 rainy_sellday
rename hh_headhhdairy_ouputq56 rainy_consumeday
rename hh_headhhdairy_ouputq57 rainy_givenday
rename hh_headhhdairy_ouputq58 rainy_calvesday
rename hh_headhhdairy_ouputq59 rainy_processday

foreach v of varlist rainy_localcows_abv8 rainy_localcows_liquid rainy_consumeday rainy_exotics_abv8 rainy_exotics_liqud rainy_price rainy_total rainy_lowestpric rainy_highestpric rainy_sellday rainy_givenday rainy_calvesday rainy_processday{
replace `v'="." if `v'=="999"|`v'=="n/a"|`v'=="99"
destring `v', replace
}
foreach v of varlist rainy_localcows rainy_exotics{
replace `v'=. if `v'==999

}
gen rainy_localcows_liquid_total= rainy_localcows_liquid*rainy_localcows
gen rainy_exotics_liqud_total=rainy_exotics_liqud*rainy_exotics

gen rainy_total_liq=rainy_localcows_liquid_total+rainy_exotics_liqud_total
replace  rainy_total_liq=rainy_localcows_liquid_total if rainy_exotics_liqud_total==. & rainy_total_liq==.
replace  rainy_total_liq=rainy_exotics_liqud_total if  rainy_localcows_liquid_total==. & rainy_total_liq==.


order rainy_localcows_liquid_total rainy_exotics_liqud_total rainy_total_liq, a (rainy_processday)


rename hh_headhhsalesq701 sales_directneighbor
rename hh_headhhsalesq702 sales_tradetoretail
rename hh_headhhsalesq703 sales_tradetomcc
rename hh_headhhsalesq704 sales_tradetomilkshop
rename hh_headhhsalesq705 sales_transptoretail
rename hh_headhhsalesq706 sales_transptomcc
rename hh_headhhsalesq707 sales_transptomilkshop
rename hh_headhhsalesq708 sales_directmcc
rename hh_headhhsalesq709 sales_directmilkshop
rename hh_headhhsalesq7096 sales_other

foreach v of varlist sales_* {
replace `v'="1" if `v'=="TRUE"
replace `v'="0" if `v'=="FALSE"
destring `v', replace
}


rename hh_headhhsalesq71 sales_neigh_trans
rename hh_headhhsalesq72 sales_neigh_num
rename hh_headhhsalesq73 sales_neigh_price
rename hh_headhhsalesq74 sales_neigh_amt
rename hh_headhhsalesq75 sales_neigh_dec
rename hh_headhhsalesq76 sales_neigh_whosold
rename hh_headhhsalesq77 sales_neigh_paym
rename hh_headhhsalesq78 sales_neigh_freqpaym
rename hh_headhhsalesq79 sales_neigh_contract
rename hh_headhhsalesq80 sales_neigh_yrs
rename hh_headhhsalesq81 sales_neigh_qual
rename hh_headhhsalesq82 sales_neigh_prem
rename hh_headhhsalesq83 sales_neigh_assis
rename hh_headhhsalesq841 sales_neigh_assis_train
rename hh_headhhsalesq842 sales_neigh_assis_input
rename hh_headhhsalesq843 sales_neigh_assis_credit
foreach v of varlist sales_neigh_assis_*{
replace `v'="1" if `v'=="TRUE"
replace `v'="0" if `v'=="FALSE"
destring `v', replace
}

rename hh_headhhsalesq84b sales_neigh_assishowboil

rename hh_headhhsalesq85 sales_tr_retail_trans
rename hh_headhhsalesq86 sales_tr_retail_num
rename hh_headhhsalesq87 sales_tr_retail_price
rename hh_headhhsalesq88 sales_tr_retail_amt
rename hh_headhhsalesq89 sales_tr_retail_dec
rename hh_headhhsalesq90 sales_tr_retail_whosold
rename hh_headhhsalesq91 sales_tr_retail_paym
rename hh_headhhsalesq92 sales_tr_retail_freqpaym
rename hh_headhhsalesq93 sales_tr_retail_contract
rename hh_headhhsalesq94 sales_tr_retail_yrs
rename hh_headhhsalesq95 sales_tr_retail_qual
rename hh_headhhsalesq96 sales_tr_retail_prem
rename hh_headhhsalesq97 sales_tr_retail_assis
rename hh_headhhsalesq981 sales_tr_retail_assis_train
rename hh_headhhsalesq982 sales_tr_retail_assis_input
rename hh_headhhsalesq983 sales_tr_retail_assis_credit
rename hh_headhhsalesq99 sales_tr_retail_transp
rename hh_headhhsalesq100 sales_tr_retail_contain
rename hh_headhhsalesq101 sales_tr_retail_howpay
rename hh_headhhsalesq102 sales_tr_retail_amtpay


rename hh_headhhsalesq103 sales_tr_mcc_trans
rename hh_headhhsalesq104 sales_tr_mcc_num
rename hh_headhhsalesq105 sales_tr_mcc_price
rename hh_headhhsalesq106 sales_tr_mcc_amt
rename hh_headhhsalesq107 sales_tr_mcc_dec
rename hh_headhhsalesq108 sales_tr_mcc_whosold
rename hh_headhhsalesq109 sales_tr_mcc_paym
rename hh_headhhsalesq110 sales_tr_mcc_freqpaym
rename hh_headhhsalesq111 sales_tr_mcc_contract
rename hh_headhhsalesq112 sales_tr_mcc_yrs
rename hh_headhhsalesq113 sales_tr_mcc_qual
rename hh_headhhsalesq114 sales_tr_mcc_prem
rename hh_headhhsalesq115 sales_tr_mcc_assis
rename hh_headhhsalesq1161 sales_tr_mcc_assis_train
rename hh_headhhsalesq1162 sales_tr_mcc_assis_input
rename hh_headhhsalesq1163 sales_tr_mcc_assis_credit
rename hh_headhhsalesq117 sales_tr_mcc_transp
rename hh_headhhsalesq118 sales_tr_mcc_contain
rename hh_headhhsalesq119 sales_tr_mcc_howpay
rename hh_headhhsalesq120 sales_tr_mcc_amtpay

foreach v of varlist sales_tr_mcc_assis_*{
replace `v'="1" if `v'=="TRUE"
replace `v'="0" if `v'=="FALSE"
destring `v', replace
}

rename hh_headhhsalesq121 sales_tr_mklshp_trans
rename hh_headhhsalesq122 sales_tr_mklshp_num
rename hh_headhhsalesq123 sales_tr_mklshp_price
rename hh_headhhsalesq124 sales_tr_mklshp_amt
rename hh_headhhsalesq125 sales_tr_mklshp_dec
rename hh_headhhsalesq126 sales_tr_mklshp_whosold
rename hh_headhhsalesq127 sales_tr_mklshp_paym
rename hh_headhhsalesq128 sales_tr_mklshp_freqpaym
rename hh_headhhsalesq129 sales_tr_mklshp_contract
rename hh_headhhsalesq130 sales_tr_mklshp_yrs
rename hh_headhhsalesq131 sales_tr_mklshp_qual
rename hh_headhhsalesq132 sales_tr_mklshp_prem
rename hh_headhhsalesq133 sales_tr_mklshp_assis
rename hh_headhhsalesq1341 sales_tr_mklshp_assis_train
rename hh_headhhsalesq1342 sales_tr_mklshp_assis_input
rename hh_headhhsalesq1343 sales_tr_mklshp_assis_credit
rename hh_headhhsalesq135 sales_tr_mklshp_transp
rename hh_headhhsalesq136 sales_tr_mklshp_contain
rename hh_headhhsalesq137 sales_tr_mklshp_howpay
rename hh_headhhsalesq138 sales_tr_mklshp_amtpay



rename hh_headhhsalesq139 sales_trans_retail_trans
rename hh_headhhsalesq140 sales_trans_retail_num
rename hh_headhhsalesq141 sales_trans_retail_price
rename hh_headhhsalesq142 sales_trans_retail_amt
rename hh_headhhsalesq143 sales_trans_retail_dec
rename hh_headhhsalesq144 sales_trans_retail_whosold
rename hh_headhhsalesq145 sales_trans_retail_paym
rename hh_headhhsalesq146 sales_trans_retail_freqpaym
rename hh_headhhsalesq147 sales_trans_retail_contract
rename hh_headhhsalesq148 sales_trans_retail_yrs
rename hh_headhhsalesq149 sales_trans_retail_qual
rename hh_headhhsalesq150 sales_trans_retail_prem
rename hh_headhhsalesq151 sales_trans_retail_assis
rename hh_headhhsalesq1521 sales_trans_retail_assis_train
rename hh_headhhsalesq1522 sales_trans_retail_assis_input
rename hh_headhhsalesq1523 sales_trans_retail_assis_credit
rename hh_headhhsalesq153 sales_trans_retail_transp
rename hh_headhhsalesq154 sales_trans_retail_transpcost
rename hh_headhhsalesq155 sales_trans_retail_contain
rename hh_headhhsalesq156 sales_trans_retail_testlact
rename hh_headhhsalesq157 sales_trans_retail_testalc
rename hh_headhhsalesq157b sales_trans_retail_transptype
rename hh_headhhsalesq157c sales_trans_retail_transpcostrec


rename hh_headhhsalesq158 sales_trans_mcc_trans
rename hh_headhhsalesq159 sales_trans_mcc_num
rename hh_headhhsalesq160 sales_trans_mcc_price
rename hh_headhhsalesq161 sales_trans_mcc_amt
rename hh_headhhsalesq162 sales_trans_mcc_dec
rename hh_headhhsalesq163 sales_trans_mcc_whosold
rename hh_headhhsalesq164 sales_trans_mcc_paym
rename hh_headhhsalesq165 sales_trans_mcc_freqpaym
rename hh_headhhsalesq166 sales_trans_mcc_contract
rename hh_headhhsalesq167 sales_trans_mcc_yrs
rename hh_headhhsalesq168 sales_trans_mcc_qual
rename hh_headhhsalesq169 sales_trans_mcc_prem
rename hh_headhhsalesq170 sales_trans_mcc_assis

rename hh_headhhsalesq1711 sales_trans_mcc_assis_train
rename hh_headhhsalesq1712 sales_trans_mcc_assis_input
rename hh_headhhsalesq1713 sales_trans_mcc_assis_credit
foreach v of varlist sales_trans_mcc_assis_*{
replace `v'="1" if `v'=="TRUE"
replace `v'="0" if `v'=="FALSE"
destring `v', replace
}
rename hh_headhhsalesq172 sales_trans_mcc_transp
rename hh_headhhsalesq173 sales_trans_mcc_transpcost
rename hh_headhhsalesq174 sales_trans_mcc_contain
rename hh_headhhsalesq175 sales_trans_mcc_testlact
rename hh_headhhsalesq176 sales_trans_mcc_testalc
rename hh_headhhsalesq176b sales_trans_mcc_transptype
rename hh_headhhsalesq176c sales_trans_mcc_transpcostrec



rename hh_headhhsalesr1 sales_trans_mklshp_trans
rename hh_headhhsalesr2 sales_trans_mklshp_num
rename hh_headhhsalesr3 sales_trans_mklshp_price
rename hh_headhhsalesr4 sales_trans_mklshp_amt
rename hh_headhhsalesr5 sales_trans_mklshp_dec
rename hh_headhhsalesr6 sales_trans_mklshp_whosold
rename hh_headhhsalesr7 sales_trans_mklshp_paym
rename hh_headhhsalesr8 sales_trans_mklshp_freqpaym
rename hh_headhhsalesr9 sales_trans_mklshp_contract
rename hh_headhhsalesr10 sales_trans_mklshp_yrs
rename hh_headhhsalesr11 sales_trans_mklshp_qual
rename hh_headhhsalesr12 sales_trans_mklshp_prem
rename hh_headhhsalesr13 sales_trans_mklshp_assis
rename hh_headhhsalesr141 sales_trans_mklshp_assis_train
rename hh_headhhsalesr142 sales_trans_mklshp_assis_input
rename hh_headhhsalesr143 sales_trans_mklshp_assis_credit
rename hh_headhhsalesr15 sales_trans_mklshp_transp
rename hh_headhhsalesr16 sales_trans_mklshp_transpcost
rename hh_headhhsalesr17 sales_trans_mklshp_contain
rename hh_headhhsalesr18 sales_trans_mklshp_testlact
rename hh_headhhsalesr19 sales_trans_mklshp_testalc
rename hh_headhhsalesr19b sales_trans_mklshp_transptype
rename hh_headhhsalesr19c sales_trans_mklshp_transpcostrec


rename hh_headhhsalesr20 sales_direct_mcc_trans
rename hh_headhhsalesr21 sales_direct_mcc_num
rename hh_headhhsalesr23 sales_direct_mcc_price
rename hh_headhhsalesr24 sales_direct_mcc_amt
rename hh_headhhsalesr25 sales_direct_mcc_dec
rename hh_headhhsalesr26 sales_direct_mcc_whosold
rename hh_headhhsalesr27 sales_direct_mcc_paym
rename hh_headhhsalesr28 sales_direct_mcc_freqpaym
rename hh_headhhsalesr29 sales_direct_mcc_contract
rename hh_headhhsalesr30 sales_direct_mcc_yrs
rename hh_headhhsalesr31 sales_direct_mcc_qual
rename hh_headhhsalesr32 sales_direct_mcc_prem
rename hh_headhhsalesr33 sales_direct_mcc_assis

rename hh_headhhsalesr341 sales_direct_mcc_assis_train
rename hh_headhhsalesr342 sales_direct_mcc_assis_input
rename hh_headhhsalesr343 sales_direct_mcc_assis_credit
foreach v of varlist sales_direct_mcc_assis_*{
replace `v'="1" if `v'=="TRUE"
replace `v'="0" if `v'=="FALSE"
destring `v', replace
}
rename hh_headhhsalesr35 sales_direct_mcc_transp
rename hh_headhhsalesr36 sales_direct_mcc_transpcost
rename hh_headhhsalesr37 sales_direct_mcc_contain
rename hh_headhhsalesr38 sales_direct_mcc_testlact
rename hh_headhhsalesr39 sales_direct_mcc_testalc



rename hh_headhhsalesr40 sales_direct_mlkshp_trans
rename hh_headhhsalesr41 sales_direct_mlkshp_num
rename hh_headhhsalesr42 sales_direct_mlkshp_price
rename hh_headhhsalesr43 sales_direct_mlkshp_amt
rename hh_headhhsalesr44 sales_direct_mlkshp_dec
rename hh_headhhsalesr45 sales_direct_mlkshp_whosold
rename hh_headhhsalesr46 sales_direct_mlkshp_paym
rename hh_headhhsalesr47 sales_direct_mlkshp_freqpaym
rename hh_headhhsalesr48 sales_direct_mlkshp_contract
rename hh_headhhsalesr49 sales_direct_mlkshp_yrs
rename hh_headhhsalesr50 sales_direct_mlkshp_qual
rename hh_headhhsalesr51 sales_direct_mlkshp_prem
rename hh_headhhsalesr52 sales_direct_mlkshp_assis
rename hh_headhhsalesr531 sales_direct_mlkshp_assis_train
rename hh_headhhsalesr532 sales_direct_mlkshp_assis_input
rename hh_headhhsalesr533 sales_direct_mlkshp_assis_credit
rename hh_headhhsalesr54 sales_direct_mlkshp_transp
rename hh_headhhsalesr55 sales_direct_mlkshp_transpcost
rename hh_headhhsalesr56 sales_direct_mlkshp_contain
rename hh_headhhsalesr57 sales_direct_mlkshp_testlact
rename hh_headhhsalesr58 sales_direct_mlkshp_testalc





foreach v of varlist  sales_neigh_* sales_tr_retail_* sales_tr_mcc_* sales_tr_mklshp_* sales_trans_retail_* sales_trans_mcc_* sales_trans_mklshp_* sales_direct_mcc_*  sales_direct_mlkshp_*{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"| `v'=="96"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"

destring `v', replace
}


rename hh_headhhq177 nosale_unable
rename hh_headhhq178 nosale_unable_time
rename hh_headhhq179 nosale_unable_why
rename hh_headhhq1801 nosale_unable_bongo
rename hh_headhhq1802 nosale_unable_ghee
rename hh_headhhq1803 nosale_unable_give
rename hh_headhhq1804 nosale_unable_animal
rename hh_headhhq1805 nosale_unable_throw

foreach v of varlist  nosale_*{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}

rename hh_headhhfood_safetyq181 safet_wheremilked
rename hh_headhhfood_safetyq182 safet_wheremilkedfloor
rename hh_headhhfood_safetyq183 safet_cleantimes
rename hh_headhhfood_safetyq184 safet_cleanhow
rename hh_headhhfood_safetyq185 safet_utterclean
rename hh_headhhfood_safetyq186 safet_uttercleanhow
rename hh_headhhfood_safetyq187 safet_milkcream
rename hh_headhhfood_safetyq188 safet_uttertreat
rename hh_headhhfood_safetyq189 safet_cowstand

rename hh_headhhfood_safetyq1901 safet_contain_glass
rename hh_headhhfood_safetyq1902 safet_contain_plasjar
rename hh_headhhfood_safetyq1903 safet_contain_plasbuck
rename hh_headhhfood_safetyq1904 safet_contain_steelbuc
rename hh_headhhfood_safetyq1905 safet_contain_jerryc
rename hh_headhhfood_safetyq1906 safet_contain_steelcan

foreach v of varlist safet_contain_*{
replace `v'="1" if `v'=="TRUE"
replace `v'="0" if `v'=="FALSE"
destring `v', replace
}
rename hh_headhhfood_safetyq191 safet_cowsleep
rename hh_headhhfood_safetyq192 safet_handwash
rename hh_headhhfood_safetyq193 safet_washcontain
rename hh_headhhfood_safetyq194 safet_drycontain

rename hh_headhhfood_safetys1 safet_prob_ticks
rename hh_headhhfood_safetys2 safet_prob_fever

rename hh_headhhfood_safetyq195 safet_spraycow
rename  hh_headhhfood_safetyq1961 safet_feed_free
rename  hh_headhhfood_safetyq1962 safet_feed_paddock 
rename hh_headhhfood_safetyq1963 safet_feed_tether
rename  hh_headhhfood_safetyq1964 safet_feed_zerograze
rename  hh_headhhfood_safetyq1965 safet_feed_freesup

rename hh_headhhfood_safetyq198 safet_watersource
rename hh_headhhfood_safetyq198b safet_watersource_prob

foreach v of varlist  safet_wheremilked safet_utterclean safet_uttercleanhow safet_milkcream safet_cowstand safet_drycontain safet_spraycow{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}

rename hh_headhhanimal_healthq199 anhealth_vac
rename  hh_headhhanimal_healthq200 anhealth_vacwho
rename  hh_headhhanimal_healthq201  anhealth_vvetdist
rename hh_headhhanimal_healthq202 anhealth_vetmeddist
rename  hh_headhhanimal_healthq203 anhealth_vet12mo
rename  hh_headhhanimal_healthq204 anhealth_vetnum
rename  hh_headhhanimal_healthq205 anhealth_vetcost

foreach v of varlist anhealth_vvetdist anhealth_vetmeddist{
replace `v'=. if `v'==999|`v'==99
}


foreach v of varlist anhealth_vac anhealth_vacwho anhealth_vet12mo anhealth_vetnum anhealth_vetcost{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}
rename hh_headhhtrainingq206 training
rename hh_headhhtrainingq207 training_who
foreach v of varlist training*{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}

rename hh_headhhtraininglabor1 labor_admale
rename  hh_headhhtraininglabor2 labor_adfemale
rename  hh_headhhtraininglabor3 labor_childmale
rename  hh_headhhtraininglabor4 labor_childfemale
foreach v of varlist labor_*{
replace `v'="1" if `v'=="TRUE"
replace `v'="0" if `v'=="FALSE"
destring `v', replace
}
rename hh_headhhtrainingq209  labor_admale_milkam
rename  hh_headhhtrainingq210 labor_admale_milkpm
rename  hh_headhhtrainingq211 labor_admale_market
rename  hh_headhhtrainingq212 labor_admale_feed
rename  hh_headhhtrainingq213 labor_admale_cleaning

rename hh_headhhtrainingq214  labor_adfemale_milkam
rename  hh_headhhtrainingq215 labor_adfemale_milkpm
rename  hh_headhhtrainingq216 labor_adfemale_market
rename  hh_headhhtrainingq217 labor_adfemale_feed
rename  hh_headhhtrainingq218 labor_adfemale_cleaning

foreach v of varlist labor_admale_* labor_adfemale_*{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"| `v'=="99"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}



rename hh_headhhtrainingq219 labor_childmale_milkam
rename  hh_headhhtrainingq220 labor_childmale_milkpm
rename  hh_headhhtrainingq221 labor_childmale_market
rename  hh_headhhtrainingq222 labor_childmale_feed
rename  hh_headhhtrainingq223 labor_childmale_cleaning


rename hh_headhhtrainingq224 labor_childfemale_milkam
rename  hh_headhhtrainingq225 labor_childfemale_milkpm
rename  hh_headhhtrainingq226 labor_childfemale_market
rename  hh_headhhtrainingq227 labor_childfemale_feed
rename  hh_headhhtrainingq228 labor_childfemale_cleaning

foreach v of varlist labor_childmale_* labor_childfemale_*{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}
foreach v of varlist  labor_a*market* labor_c*market* labor_a*cleaning* labor_c*cleaning*{
replace `v'=. if `v'>9
}

foreach v of varlist labor_a*milk* labor_c*milk*{
replace `v'=. if `v'>3
}
foreach v of varlist labor_a*feed* labor_c*feed*{
replace `v'=. if `v'>24
}
rename hh_headhhtrainingq229 labor_hired
rename  hh_headhhtrainingq230 labor_hired_hours
rename  hh_headhhtrainingq231 labor_hired_cost

foreach v of varlist labor_hired*{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}

rename hh_headhhq2321 whycattle_wealth
rename  hh_headhhq2322 whycattle_prestig
rename  hh_headhhq2323  whycattle_prodmilk
rename hh_headhhq2324  whycattle_prodmeat
rename hh_headhhq2325 whycattle_bridep
foreach v of varlist whycattle_*{
replace `v'="1" if `v'=="TRUE"
replace `v'="0" if `v'=="FALSE"
destring `v', replace
}
rename hh_headhhrecallq233 start_tenyrs_cows
rename  hh_headhhrecallq233b start_whenstart
rename  hh_headhhrecallq234 start_exoticsnum
rename  hh_headhhrecallq235  start_localnum
rename hh_headhhrecallq236 start_milkperday
rename hh_headhhrecallq237 start_sellperday
rename  hh_headhhrecallq238 start_selltrader

foreach v of varlist start_tenyrs_cows start_whenstart start_exoticsnum start_localnum start_milkperday {
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98" | `v'=="99"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}

replace start_sellperday=. if start_sellperday==999|start_sellperday==98 | start_sellperday==99


gen farmdairy_year=substr(start_whenstart, 7, 4)
destring farmdairy_year, replace

gen farmdairy_year_count=2018-farmdairy_year


rename hh_headhhlivestock_assetsq239 assets_milkshed
rename  hh_headhhlivestock_assetsq240 assets_milkshed_roof
rename  hh_headhhlivestock_assetsq241 assets_barn
rename  hh_headhhlivestock_assetsq242 assets_trough
rename  hh_headhhlivestock_assetsq243 assets_steelcan
rename  hh_headhhlivestock_assetsq244 assets_steelcannum
rename  hh_headhhlivestock_assetsq245 assets_steelbucsteelbuck
rename  hh_headhhlivestock_assetsq246 assets_steelbucsteelbucknum 
rename  hh_headhhlivestock_assetsq247 assets_jerryc 
rename  hh_headhhlivestock_assetsq248 assets_jerrycnum 
rename  hh_headhhlivestock_assetsq249 assets_plasbuck 
rename  hh_headhhlivestock_assetsq250 assets_plasbucknum
rename  hh_headhhlivestock_assetsq251 assets2_mobilenum
rename  hh_headhhlivestock_assetsq252 assets2_motonum
rename  hh_headhhlivestock_assetsq253 assets2_bikenum
rename  hh_headhhlivestock_assetsq254 assets2_carsnum
rename  hh_headhhlivestock_assetsq255 assets2_radionum
rename  hh_headhhlivestock_assetsq256 assets2_waterpumpnum
rename  hh_headhhlivestock_assetsq257 assets2_pangasnum
rename  hh_headhhlivestock_assetsq258 assets2_wheelbnum
rename  hh_headhhlivestock_assetsq259 assets2_sicklenum
rename  hh_headhhlivestock_assetsq260 assets2_hayforknum
rename  hh_headhhlivestock_assetsq261 assets2_tarpaulinsnum
rename  hh_headhhlivestock_assetsq262 assets2_cuttingmachnum
rename  hh_headhhlivestock_assetsq263 assets2_horsepipnum
rename  hh_headhhlivestock_assetsq264 assets2_sprayersnum


foreach v of varlist assets_*{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98" | `v'=="99"| `v'=="96"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}
foreach v of varlist assets2*{
replace `v'=. if `v'==999|`v'==98 | `v'==99| `v'==96

}


rename  hh_headhhcooperativeq265 coop_
rename  hh_headhhcooperativeq266 coop_num
rename  hh_headhhcooperativeq267 coop_nondairy

foreach v of varlist coop_*{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}


rename  hh_headhhaccess_financeq268 fina_ac
rename  hh_headhhaccess_financeq2691 fina_coop
rename  hh_headhhaccess_financeq2692 fina_bank
rename  hh_headhhaccess_financeq2693 fina_friend
rename  hh_headhhaccess_financeq2694 fina_vsla

rename  hh_headhhaccess_financeq270 fina_loan
rename  hh_headhhaccess_financeq271 fina_loanamt
rename  hh_headhhaccess_financeq272 fina_inv
rename  hh_headhhaccess_financeq2731 fina_inv_impbred
rename  hh_headhhaccess_financeq2732 fina_inv_local
rename  hh_headhhaccess_financeq2733 fina_inv_buycan
rename  hh_headhhaccess_financeq2734 fina_inv_treatan
rename  hh_headhhaccess_financeq2735 fina_inv_hirelabor
rename  hh_headhhaccess_financeq2736 fina_inv_artinf
rename  hh_headhhaccess_financeq2737 fina_inv_othermilk
foreach v of varlist fina_*{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"
replace `v'="1" if `v'=="Yes"|`v'=="TRUE"
replace `v'="0" if `v'=="No"|`v'=="FALSE"
destring `v', replace
}

rename hh_headhhchild hh_childU5
rename hh_headhhl1 hh_childU5_bf
rename  hh_headhhl2 hh_childU5_milk7d
rename  hh_headhhl3 hh_childU5_milklit
foreach v of varlist hh_childU5*{
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}
rename  hh_headhhconsumption*  hh_headhhcons_*
rename v555 hh_headhhcons_sweetpotatoe_valu

foreach v of varlist hh_headhhcons_* {
replace `v'="." if `v'=="999"|`v'=="n/a"| `v'=="98"
replace `v'="1" if `v'=="Yes"
replace `v'="0" if `v'=="No"
destring `v', replace
}
*

exit


*****************
*	DESCrIPTIVVES

sum hh_mems hhmem*_age hhmem*_married hhmem*_head hhmem*_spouse hhmem*_male

gen head_age=. 
replace head_age =hhmem1_age if hhmem1_relhhhead==1
replace head_age =hhmem2_age if hhmem2_relhhhead=="1" & head_age==.
replace head_age =hhmem3_age if hhmem3_relhhhead=="1" &head_age==.
replace head_age =hhmem4_age if hhmem4_relhhhead=="1" &head_age==.
replace head_age =hhmem5_age if hhmem5_relhhhead=="1" &head_age==.
replace head_age =hhmem6_age if hhmem6_relhhhead=="1" &head_age==.
replace head_age =hhmem7_age if hhmem7_relhhhead=="1" &head_age==.
replace head_age =hhmem8_age if hhmem8_relhhhead=="1" &head_age==.
replace head_age =hhmem9_age if hhmem9_relhhhead=="1" &head_age==.
gen head_married=. 
replace head_married =hhmem1_married if hhmem1_relhhhead==1
replace head_married =hhmem2_married if hhmem2_relhhhead=="1" & head_married==.
replace head_married =hhmem3_married if hhmem3_relhhhead=="1" &head_married==.
replace head_married =hhmem4_married if hhmem4_relhhhead=="1" &head_married==.
replace head_married =hhmem5_married if hhmem5_relhhhead=="1" &head_married==.
replace head_married =hhmem6_married if hhmem6_relhhhead=="1" &head_married==.
replace head_married =hhmem7_married if hhmem7_relhhhead=="1" &head_married==.
replace head_married =hhmem8_married if hhmem8_relhhhead=="1" &head_married==.
replace head_married =hhmem9_married if hhmem9_relhhhead=="1" &head_married==.

gen head_male=. 
replace head_male =hhmem1_male if hhmem1_relhhhead==1
replace head_male =hhmem2_male if hhmem2_relhhhead=="1" & head_male==.
replace head_male =hhmem3_male if hhmem3_relhhhead=="1" &head_male==.
replace head_male =hhmem4_male if hhmem4_relhhhead=="1" &head_male==.
replace head_male =hhmem5_male if hhmem5_relhhhead=="1" &head_male==.
replace head_male =hhmem6_male if hhmem6_relhhhead=="1" &head_male==.
replace head_male =hhmem7_male if hhmem7_relhhhead=="1" &head_male==.
replace head_male =hhmem8_male if hhmem8_relhhhead=="1" &head_male==.
replace head_male =hhmem9_male if hhmem9_relhhhead=="1" &head_male==.


sum head_age head_married head_male
sum head_edu_primary head_edu_some head_edu_none

sum head_rel_angli head_rel_pent head_rel_cath head_rel_mus

sum head_tribe_Banyankore head_tribe_Banyarwanda head_tribe_Banyoro

sum dist_tarmac dist_murram dist_mcc dist_milkshop dist_neigh dist_market dist_tradecenter dist_vilchairm
sum cellrec_good

sum main_income_dairy main_income_crops

sum hh_rooms hh_solarpower hh_ironroof land_area
tab land_areafenced 
sum land_area_useable

gen cattle_total=cattle_localcowowned+ cattle_localheiferown +cattle_localcalvesowned +cattle_improvedcowowned +cattle_improvedheiferown +cattle_improvedcalvesown
sum cattle*


foreach l in dry_ rainy_ {
*gen `l'total_liters= `l'localcows_liquid_total+`l'exotics_liqud_total
gen `l'income=(`l'sellday*`l'price) if `l'sellday!=0
}

sum dry_*

sum rainy_*

foreach l in  localcows localcows_abv8 localcows_liquid exotics exotics_abv8 exotics_liqud price total lowestpric highestpric sellday consumeday givenday calvesday processday total_liq income{
ttest dry_`l'==rainy_`l', unp
}


sum sales_directneighbor sales_tradetoretail sales_tradetomcc sales_tradetomilkshop sales_transptoretail sales_transptomcc sales_transptomilkshop sales_directmcc sales_directmilkshop sales_other
gen sales_totaldiff= sales_directneighbor +sales_tradetoretail+ sales_tradetomcc+ sales_tradetomilkshop+ sales_transptoretail+ sales_transptomcc +sales_transptomilkshop+ sales_directmcc +sales_directmilkshop+ sales_other


sum sales_neigh_* sales_tr_retail_* sales_tr_mcc_* sales_tr_mklshp_* sales_trans_retail_* sales_trans_mcc_* sales_trans_mklshp_* sales_direct_mcc_*  sales_direct_mlkshp_*

sum nosale*

sum safet* anhealth* training*

sum labor*

foreach v of varlist labor_admale_milkam labor_admale_milkpm labor_admale_market labor_admale_feed labor_admale_cleaning labor_adfemale_milkam labor_adfemale_milkpm labor_adfemale_market labor_adfemale_feed labor_adfemale_cleaning labor_childmale_milkam labor_childmale_milkpm labor_childmale_market labor_childmale_feed labor_childmale_cleaning labor_childfemale_milkam labor_childfemale_milkpm labor_childfemale_market labor_childfemale_feed labor_childfemale_cleaning{
gen `v'_0=`v'
replace `v'_0=0 if `v'==.
}
gen labor_hh_total=labor_admale_milkam_0+ labor_admale_milkpm_0+ labor_admale_market_0+ labor_admale_feed_0 +labor_admale_cleaning_0 +labor_adfemale_milkam_0+ labor_adfemale_milkpm_0 +labor_adfemale_market_0 +labor_adfemale_feed_0+ labor_adfemale_cleaning_0 +labor_childmale_milkam_0+ labor_childmale_milkpm_0 +labor_childmale_market_0+ labor_childmale_feed_0 +labor_childmale_cleaning_0+ labor_childfemale_milkam_0 +labor_childfemale_milkpm_0 +labor_childfemale_market_0+ labor_childfemale_feed_0+ labor_childfemale_cleaning_0

gen labor_hired_hourrate=labor_hired_cost/labor_hired_hours 

sum whycattle*

sum start*
gen start_dairy_year=substr(start_whenstart, 7, 4)
destring start_dairy_year, replace

gen start_dairy_year_count=18-start_dairy_year if start_dairy_year<19
replace start_dairy_year_count=100-start_dairy_year+18 if start_dairy_year>18
sum start_dairy_year_count if start_dairy_year_count<10

sum farmdairy_year_count
sum assets*

sum coop* fina*

replace hh_childU5_milklit=. if hh_childU5_milklit>50
sum hh_childU5*


foreach v of varlist hh_headhhcons_maize_val hh_headhhcons_milk_valu hh_headhhcons_millet_va hh_headhhcons_rice_valu hh_headhhcons_cassava_v hh_headhhcons_sweetpotatoe_valu hh_headhhcons_beans_val hh_headhhcons_gnuts_val hh_headhhcons_fruits_va hh_headhhcons_veg_value hh_headhhcons_sugar_val hh_headhhcons_cooking_oil_ hh_headhhcons_bongo_val hh_headhhcons_soap_valu hh_headhhcons_airtime_v{
gen `v'2=`v'
replace `v'2=0 if `v'==.
}

gen hh_headhhconsumption_totalval=hh_headhhcons_maize_value2+ hh_headhhcons_milk_value2+ hh_headhhcons_millet_value2+ hh_headhhcons_rice_value2+ hh_headhhcons_cassava_valu2+ hh_headhhcons_sweetpotatoe_valu2 +hh_headhhcons_beans_value2 +hh_headhhcons_gnuts_value2 +hh_headhhcons_fruits_value2+ hh_headhhcons_veg_value2+ hh_headhhcons_sugar_value2+ hh_headhhcons_cooking_oil_2+ hh_headhhcons_bongo_value2+ hh_headhhcons_soap_value2+ hh_headhhcons_airtime_valu2

sum hh_headhhconsumption*









