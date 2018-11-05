****************************
**	PIM
**	DAIRY TRADERS

eststo clear
clear

gl dir "C:\Users\u0107600\Dropbox\Uganda-Dairy"

 use "$dir\Data\traders.dta", clear


 
 ***clean data
 
 rename traderq3 age
 replace age ="." if age =="999"| age=="n/a"
 destring age, replace
 
 rename traderq4 gender
  
 rename traderq5 edu
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
 
 gen hh_head=.
 replace hh_head=0 if traderq6=="No"
 replace hh_head=1 if traderq6=="Yes"
 
 
 gen hh_head_child=.
 replace hh_head_child=0 if hh_head!=.
 replace hh_head_child=1 if traderq7=="3"
  order hh_head*, a(traderq6)

 rename traderq8 hh_size
 replace hh_size="." if hh_size=="KINONI"
 destring hh_size, replace 
 
replace traderq9=. if traderq9>95
 gen rel_angli=.
 replace  rel_angli =0 if traderq9!=.
 replace  rel_angli =1 if traderq9==1
 gen rel_pent=.
 replace  rel_pent =0 if traderq9!=.
 replace  rel_pent =1 if traderq9==2
  gen rel_cath=.
 replace  rel_cath =0 if traderq9!=.
 replace  rel_cath =1 if traderq9==3
   gen rel_mus=.
 replace  rel_mus =0 if traderq9!=.
 replace  rel_mus =1 if traderq9==4
 
 order rel*, a(traderq9)
 
 replace traderq10=. if traderq10==96
 gen eth_banyankore=.
 replace eth_banyankore=0 if traderq10!=.
 replace eth_banyankore=1 if traderq10==2
 gen eth_banyarwanda=.
 replace eth_banyarwanda=0 if traderq10!=.
 replace eth_banyarwanda=1 if traderq10==5
 
 order eth_*, a(traderq10)
 
gen indp_collect=.
replace indp_collect=0 if traderq14!="."
replace indp_collect=1 if traderq14=="1"
 
 
rename traderq16 trader_transport
replace trader_transport="." if trader_transport=="n/a"
destring trader_transport, replace
gen trade_only=.
replace trade_only=0 if trader_transport!=.
replace trade_only=1 if trader_transport==1
gen trans_only=.
replace trans_only=0 if trader_transport!=.
replace trans_only=1 if trader_transport==2
gen trade_trans=.
replace trade_trans=0 if trader_transport!=.
replace trade_trans=1 if trader_transport==3

order trade_only trans_only  trade_trans, a(trader_transport)

rename traderq17 otherdairyprod

rename traderq19 otherprod

gen trade_year=substr(traderq21, 7, 4)
gen tradedairy_year=substr(traderq22, 7, 4)
destring tradedairy_year, replace

gen tradedairy_years_count=2018-tradedairy_year

order trade_year tradedairy_year tradedairy_years_count, a(traderq22)

rename traderq24 competit_now
rename traderq25 competit_first 

replace competit_first=. if competit_first==999
replace competit_now=. if competit_now==999

gen agreenever_traders=.
replace agreenever_traders=0 if traderq26=="1"|traderq26=="2"|traderq26=="3"
replace agreenever_traders=1 if traderq26=="2"

gen agreenever_trans=.
replace agreenever_trans=0 if traderq27=="1"|traderq27=="2"|traderq27=="3"
replace agreenever_trans=1 if traderq27=="2"
order agreen*, a(competit_first)

gen shareinfo_never=.
replace shareinfo_never=0 if traderq28=="1"|traderq28=="2"|traderq28=="3"
replace shareinfo_never=1 if traderq28=="2"

gen shareinfo_alw=.
replace shareinfo_alw=0 if traderq28=="1"|traderq28=="2"|traderq28=="3"
replace shareinfo_alw=1 if traderq28=="1"

order shareinfo_*, a(traderq28)


rename traderq30 coop
replace coop="." if coop=="9"
replace coop="1" if coop=="Yes"
replace coop="0" if coop=="No"
destring coop, replace

rename traderq31 training
replace training="." if training=="150"
replace training="1" if training=="Yes"
replace training="0" if training=="No"
destring training, replace


**dry season
rename traderq33 dry_price

gen dry_from_indiv=0
replace dry_from_indiv=1 if traderq35==1

rename traderq36 dry_suppliers
replace dry_suppliers=. if dry_suppliers==999

rename traderq37 dry_avg_liters
replace dry_avg_liters=. if dry_avg_liters==999

rename traderq38 dry_avg_soldshop
replace dry_avg_soldshop=. if dry_avg_soldshop==999

rename traderq39 dry_avg_soldindiv
replace dry_avg_soldindiv=. if dry_avg_soldindiv==999


rename traderq40 dry_avg_soldcoop
replace dry_avg_soldcoop=. if dry_avg_soldcoop==999


rename traderq41 dry_avg_soldpriv
replace dry_avg_soldpriv=. if dry_avg_soldpriv==999

rename traderq42 dry_avg_soldproc
replace dry_avg_soldproc=. if dry_avg_soldproc==999

rename traderq43 dry_avg_soldinsti
replace dry_avg_soldinsti=. if dry_avg_soldinsti==999

rename traderq44 dry_avg_soldrest
replace dry_avg_soldrest=. if dry_avg_soldrest==999

rename traderq45 dry_avg_soldother
replace dry_avg_soldother=. if dry_avg_soldother==999


**rainy seaons
rename traderq46 rain_price

gen rain_from_indiv=0
replace rain_from_indiv=1 if traderq47==1

rename traderq48 rain_suppliers
replace rain_suppliers=. if rain_suppliers==999

rename traderq49 rain_avg_liters
replace rain_avg_liters=. if rain_avg_liters==999

rename traderq50 rain_avg_soldshop
replace rain_avg_soldshop=. if rain_avg_soldshop==999

rename traderq51 rain_avg_soldindiv
replace rain_avg_soldindiv=. if rain_avg_soldindiv==999


rename traderq52 rain_avg_soldcoop
replace rain_avg_soldcoop=. if rain_avg_soldcoop==999


rename traderq53 rain_avg_soldpriv
replace rain_avg_soldpriv=. if rain_avg_soldpriv==999

rename traderq54 rain_avg_soldproc
replace rain_avg_soldproc=. if rain_avg_soldproc==999

rename traderq55 rain_avg_soldinsti
replace rain_avg_soldinsti="." if rain_avg_soldinsti=="999"|rain_avg_soldinsti=="n/a"
destring rain_avg_soldinsti,replace

rename traderq56 rain_avg_soldrest
replace rain_avg_soldrest="." if rain_avg_soldrest=="999"|rain_avg_soldrest=="n/a"
destring rain_avg_soldrest,replace

rename traderq57 rain_avg_soldother
replace rain_avg_soldother="." if rain_avg_soldother=="999"|rain_avg_soldother=="n/a"
destring rain_avg_soldother,replace

gen milk_nevrej=.
replace milk_nevrej=0 if traderq58=="2"|traderq58=="1"|traderq58=="3"
replace milk_nevrej=1 if traderq58=="2"
order milk_nevrej, a(traderq58)

rename traderq59 rej_reason
replace rej_reason="." if rej_reason=="n/a"|rej_reason=="96"
destring rej_reason, replace

foreach v of varlist traderq601 traderq602 traderq603 traderq604 traderq605 traderq6096{
replace `v'="." if `v'=="n/a"
destring `v', replace
}

rename  traderq601 rej_takeback
rename  traderq602 rej_makeghee
rename  traderq603 rej_tryagain
rename  traderq604 rej_give
rename traderq605  rej_throw
rename traderq6096  rej_other


rename traderq61 own_50l
rename traderq62 own_25l
rename traderq63 own_10l
rename traderq64 own_jerrycan
rename traderq65 own_moto
rename traderq66 own_bike
rename traderq67 own_mobile
replace own_mobile="." if own_mobile=="Yes"
destring own_mobile, replace
rename traderq68 own_measu
rename traderq69 own_sieves

gen trans_boda=.
replace trans_boda=0 if traderq70==1|traderq70==2|traderq70==3|traderq70==4
replace trans_boda=1 if traderq70==3

gen trans_bike=.
replace trans_bike=0 if traderq70==1|traderq70==2|traderq70==3|traderq70==4
replace trans_bike=1 if traderq70==2
 
 order trans_boda trans_bike,a(traderq70)
 
 rename traderq72 trans_cap_lit
 replace trans_cap_lit ="." if trans_cap_lit=="999"|trans_cap_lit=="Yes"
 destring trans_cap_lit, replace
 
 replace traderq73="." if traderq73=="Yes"
 destring traderq73, replace
 
 gen main_buyer_indiv=.
 replace main_buyer_indiv=0 if traderq73!=.
  replace main_buyer_indiv=1 if traderq73==1

  gen main_buyer_coop=.
 replace main_buyer_coop=0 if traderq73!=.
  replace main_buyer_coop=1 if traderq73==3
  
  order main_buy*, a(traderq73)
  
  rename traderq74 yrs_delivering
  replace yrs_delivering="." if yrs_delivering=="09:05:00.000+03"
  destring yrs_delivering, replace 
    replace yrs_delivering=. if yrs_delivering==999

  rename traderq75 milk_tested
  replace milk_tested=". " if milk_tested=="n/a"
   replace milk_tested="1 " if milk_tested=="Yes"
   replace milk_tested="0 " if milk_tested=="No"
	destring  milk_tested, replace
  
  
  rename traderq76 milk_tested_lacto
    replace milk_tested_lacto=". " if milk_tested_lacto=="n/a"
   replace milk_tested_lacto="1 " if milk_tested_lacto=="Yes"
   replace milk_tested_lacto="0 " if milk_tested_lacto=="No"
	destring  milk_tested_lacto, replace
  
  rename traderq77 milk_tested_alcohol
    replace milk_tested_alcohol=". " if milk_tested_alcohol=="n/a"
    replace milk_tested_alcohol="1 " if milk_tested_alcohol=="Yes"
   replace milk_tested_alcohol="0 " if milk_tested_alcohol=="No"
	destring  milk_tested_alcohol, replace
   
  gen paid_cash_trans=.
  replace paid_cash_trans=0 if traderq80=="1"|traderq80=="2"
  replace paid_cash_trans=1 if traderq80=="1"

  gen paid_weekly_trans=.
  replace paid_weekly_trans=0 if paid_cash_trans!=.
  replace paid_weekly_trans=1 if traderq81=="3"
  
  order paid_*, a(traderq81)
  
  rename traderq82 trans_agreement
replace trans_agreement="." if trans_agreement=="n/a"
destring trans_agreement, replace
replace trans_agreement=. if trans_agreement==98

   gen paid_cash_buy=.
  replace paid_cash_buy=0 if traderq83=="1"|traderq83=="2"
  replace paid_cash_buy=1 if traderq83=="1"
  
  gen paid_cash_trade=.
  replace paid_cash_trade=0 if traderq83b=="1"|traderq83b=="2"
  replace paid_cash_trade=1 if traderq83b=="1"

  gen paid_weekly_trade=.
  replace paid_weekly_trade=0 if paid_cash_trade!=.
  replace paid_weekly_trade=1 if traderq84=="3"
  
  order paid_*, a(traderq84)

  
rename traderq85 trade_agree_sell
replace trade_agree_sell="." if trade_agree_sell=="n/a"
destring trade_agree_sell, replace


rename traderq85b trade_agree_buy
replace trade_agree_buy="." if trade_agree_buy=="n/a"
destring trade_agree_buy, replace

rename traderq86 trans_price_liter
replace trans_price_liter="." if trans_price_liter=="999"|trans_price_liter=="n/a"
destring trans_price_liter, replace

rename traderq87 trans_vol_liter
replace trans_vol_liter="." if trans_vol_liter=="999"|trans_vol_liter=="n/a"
destring trans_vol_liter, replace

rename traderq88 trans_km_liter
replace trans_km_liter="." if trans_km_liter=="999"|trans_km_liter=="n/a"|trans_km_liter=="8.300000000000001"
destring trans_km_liter, replace

rename traderq91 trade_price_liter
replace trade_price_liter="." if trade_price_liter=="999"|trade_price_liter=="n/a"
destring trade_price_liter, replace

rename traderq92 trade_vol_liter
replace trade_vol_liter="." if trade_vol_liter=="999"|trade_vol_liter=="n/a"|trade_vol_liter=="Yes"
destring trade_vol_liter, replace

rename traderq93 trade_km_liter
replace trade_km_liter="." if trade_km_liter=="999"|trade_km_liter=="n/a"|trade_km_liter=="Yes"
destring trade_km_liter, replace


rename traderq94 contain_wash
rename traderq95 contain_wash_how
rename traderq96 contain_wash_dry
rename traderq97 finance_records
rename traderq98 finance_acess
rename traderq991 finance_coop
rename traderq992 finance_bank
rename traderq993 finance_friend
rename traderq994 finance_vsla

foreach v of varlist finance*{
replace `v'="." if `v'=="n/a"
destring `v', replace
}

replace finance_coop="." if finance_coop=="No"
destring finance_coop, replace

gen finance_loan=.
replace finance_loan=0 if traderq100!="98"
replace finance_loan=1 if traderq100=="Yes"

rename traderq101 finance_loan_amt
replace finance_loan_amt="." if finance_loan_amt=="n/a"
destring finance_loan_amt, replace

gen finance_loan_invest=.
replace finance_loan_invest=0 if finance_loan==1
replace finance_loan_invest=1 if traderq102=="Yes"

exit

****DESCRIPTIVES

tab gender
sum edu_* tradedairy_years_count



sum trade_only trans_only trade_trans 
tab otherdairyprod
tab  otherprod
sum competit_first competit_now
sum agreenever_trans agreenever_traders shareinfo_alw shareinfo_never coop training
sum dry_price
tab traderq35
sum  dry_suppliers dry_avg_liters
sum dry_avg_soldshop dry_avg_soldindiv dry_avg_soldcoop dry_avg_soldpriv dry_avg_soldproc dry_avg_soldinsti dry_avg_soldrest
sum rain_price
tab traderq47
sum  rain_suppliers rain_avg_liters
sum rain_avg_soldshop rain_avg_soldindiv rain_avg_soldcoop rain_avg_soldpriv rain_avg_soldproc rain_avg_soldinsti rain_avg_soldrest

tab milk_nevrej
tab rej_reason
sum rej_takeback rej_makeghee rej_tryagain rej_give rej_throw rej_other
sum own_50l own_25l own_10l own_jerrycan own_moto own_bike own_mobile own_measu own_sieves
sum trans_boda trans_bike
sum trans_boda trans_bike trans_cap_lit
sum   trans_cap_lit if trans_boda==1
sum   trans_cap_lit if trans_bike==1
sum main_buyer_indiv main_buyer_coop yrs_delivering
sum milk_tested milk_tested_lacto milk_tested_alcohol





sum paid_cash_trade paid_weekly_trade trade_agree_sell trade_agree_buy trade_price_liter trade_vol_liter trade_km_liter

sum paid_cash_trans paid_weekly_trans trans_agreement trans_price_liter trans_vol_liter trans_km_liter

tab contain_wash
tab contain_wash_how
tab contain_wash_dry
tab finance_records
tab finance_acess
tab finance_loan
tab finance_loan_amt
