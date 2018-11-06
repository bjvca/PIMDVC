****************************
**	PIM
**	DAIRY MCCs

eststo clear
clear

gl dir "C:\Users\u0107600\Dropbox\Uganda-Dairy"

 use "$dir\Data\mccs.dta", clear


 
 ***clean data
rename mccq4 gender

 rename mccq5 edu
 gen edu_primary = .
 replace edu_primary=0 if edu!=.
 replace edu_primary=1 if edu==3 | edu==5 |edu==6

 gen edu_secon = .
 replace edu_secon=0 if edu!=.
 replace edu_secon=1 if edu==5 |edu==6

 gen edu_none = .
 replace edu_none=0 if edu!=.
 replace edu_none=1 if edu==1
 
 gen edu_some = .
 replace edu_some=0 if edu!=.
 replace edu_some=1 if edu>1 & edu<7
 
 order edu_*, a(edu)

 
 gen role_manager=0
 replace role_manager=1 if mccq6=="Manager"
 
  order role_manager, a(mccq6)
  
 gen type_private=0
 replace type_private=1 if mccq8==1
 gen type_coop=0
  replace type_coop=1 if mccq8==3
  
 gen type_subcollect=0
 replace type_subcollect=1 if mccq9=="Yes"

  gen type_otherpts=0 if mccq11=="No"
 replace type_otherpts=1 if mccq11=="Yes"

 order type_*, a (mccq11)
 
  rename mccq12 type_otherpts_no
 replace type_otherpts_no=". " if type_otherpts_no=="n/a" 
 destring type_otherpts_no, replace
 

 
   rename mccq13 type_otherpts_cap
 replace type_otherpts_cap=". " if type_otherpts_cap=="n/a" |type_otherpts_cap=="999" 
 destring type_otherpts_cap, replace

 rename mccq14 num_workers

 rename mccq15 mem_only
 
  rename mccq16 nonmem_num
  replace nonmem_num="." if nonmem_num=="999"|nonmem_num=="n/a"
  destring nonmem_num, replace

  rename mccq17 nonmem_lit
  replace nonmem_lit="." if nonmem_lit=="999"|nonmem_lit=="n/a"
  destring nonmem_lit, replace 
 
  rename mccq18 mem_perday
  replace mem_perday=. if mem_perday==999
  
   rename mccq19 mem_perday_lit
  replace mem_perday_lit="." if mem_perday_lit=="999"|mem_perday_lit=="n/a"
  destring mem_perday_lit, replace 
 
    rename mccq20 milk_prem
	gen milk_prem_sometimes=0
	replace milk_prem_sometimes=1 if milk_prem==1|milk_prem==2
 
    rename mccq21 milk_prem_lq
	gen milk_prem_lq_sometimes=0
	replace milk_prem_lq_sometimes=1 if milk_prem_lq==1|milk_prem_lq==2

    rename mccq22 milk_prem_large
	replace milk_prem_large="." if milk_prem_large=="n/a"
	destring milk_prem_large, replace
	
 order milk_prem_*,a(milk_prem)
 
 rename mccq23 capacity_liters
 
  rename mccq24 capacity_dry
 rename mccq25 capacity_rain

 rename mccq281 pay_ondel
  rename  mccq282 pay_daily
  rename mccq283 pay_week
  rename mccq284  pay_twicemon
  rename mccq285 pay_month
 
 rename mccq291 sell_wholesa
 rename mccq292 sell_brokers
 rename mccq293 sell_process
 rename mccq294 sell_agent_proc
 rename mccq295 sell_agent_coop
 rename mccq296 sell_indptrad 
 rename mccq297 sell_retail
 rename mccq298 sell_milkshop
 rename mccq299 sell_instis
 
gen sell_jesa=.
replace sell_jesa=0 if mccq30=="1"|mccq30=="2"|mccq30=="3"
replace sell_jesa=1 if mccq30=="1"

gen sell_brooks=.
replace sell_brooks=0 if mccq30=="1"|mccq30=="2"|mccq30=="3"
replace sell_brooks=1 if mccq30=="2"
 
 
 rename mccq31 sell_rejected
 replace sell_rejected="1" if sell_rejected=="Yes"
 replace sell_rejected="0" if sell_rejected=="No"
 destring  sell_rejected, replace
 
 rename mccq32 sell_rejected_num
 replace sell_rejected_num="." if sell_rejected_num=="n/a"|sell_rejected_num=="999"
 destring  sell_rejected_num, replace

  rename mccq33 sell_rejected_res

 gen traders_choice_alot=0
 replace traders_choice_alot=1 if mccq34==1
 
 gen traders_choice_litte=0
 replace traders_choice_litte=1 if mccq34==2
 

 gen trademilk_year=mccq35
 tostring trademilk_year, replace
  destring trademilk_year, replace
replace trademilk_year=(21185-trademilk_year)/365
replace trademilk_year=round(trademilk_year,1) 

 order sell_jesa sell_brooks traders_choice_alot traders_choice_litte trademilk_year, a(mccq35)
 
 rename mccq36 start_cap
 replace start_cap=. if start_cap==999
  rename mccq37 start_sup
 replace start_sup=. if start_sup==999
  rename mccq38 start_sup_cap
 replace start_sup_cap=. if start_sup_cap==999

   rename mccq39 price_dry
 replace price_dry=. if price_dry==999
   rename mccq40 price_rain
 replace price_rain=. if price_rain==999
   rename mccq41 liters_dry
 replace liters_dry=. if liters_dry==999
   rename mccq42 liters_rain
 replace liters_rain=. if liters_rain==999

 rename mccq431 dest_kampalasub
 
 rename mccq44 supplies_milkcans
 rename mccq45  supplies_jerrycan
  rename mccq46 supplies_scale
  rename mccq47 supplies_lactom
  rename mccq48   supplies_wareho
rename mccq49  supplies_trucks
 rename mccq50 supplies_mobile
  rename mccq51 supplies_moto
 foreach v of varlist supplies_milkcans supplies_jerrycan supplies_trucks{
 replace `v'=. if `v'==999
 }
 
 
rename mccq52a services_trainmilkprod
rename mccq52b services_trainmilkmyg
rename mccq52c services_credit
rename mccq52d services_equip
rename mccq52e services_milkcans
rename mccq52f services_vetser
rename mccq52g services_transp
rename mccq52h services_cooler
rename mccq52i services_crossbred
rename mccq52j services_meds


rename mccq53a whysell_alwpurch
rename mccq53b whysell_hpirce 
rename mccq53c whysell_loans
rename mccq53d  whysell_buyercomes
rename mccq53e  whysell_train
rename mccq53f  whysell_payments
rename mccq53g  whysell_nochoice
rename mccq53h whysell_nochecks

rename mccq54 testmilk
rename mccq551 testmilk_visual
rename mccq552 testmilk_lact
rename mccq553 testmilk_alco

rename mccq56 milkrej
rename mccq57  transp_tokampa

rename mccq58a transp_tokampa_owntruck
rename mccq58b transp_tokampa_hiredtruck
rename mccq58c transp_tokampa_proctruck

foreach v of varlist transp_tokampa_*{
replace `v'="." if   transp_tokampa=="No"
}

destring transp_tokampa_owntruck transp_tokampa_hiredtruck transp_tokampa_proctruck, replace
rename mccq59 transp_liter
replace transp_liter=. if transp_liter==999
rename mccq60 transp_buyer
replace transp_buyer=. if transp_buyer==999

exit
*****Descriptives
tab gender
sum edu_* role_manager trademilk_year

sum type_private type_coop type_subcollect
sum type_otherpts type_otherpts_no type_otherpts_cap num_workers
tab mem_only
sum  nonmem_num nonmem_lit mem_perday mem_perday_lit
sum milk_prem_sometimes milk_prem_lq_sometimes
sum capacity_liters capacity_dry capacity_rain pay_ondel pay_daily pay_week pay_twicemon pay_month

sum sell_wholesa sell_brokers sell_process sell_agent_proc sell_agent_coop sell_indptrad sell_retail sell_milkshop sell_rejected sell_rejected_num sell_rejected_res sell_jesa sell_brooks

sum supplies_milkcans supplies_jerrycan supplies_scale supplies_lactom supplies_wareho supplies_trucks supplies_mobile supplies_moto

sum whysell_alwpurch whysell_hpirce whysell_loans whysell_buyercomes whysell_train whysell_payments whysell_nochoice whysell_nochecks

