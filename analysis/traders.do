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
replace trade_trans=0 if trade_trans!=.
replace trade_trans=1 if trade_trans==3

order trade_only trans_only  trade_trans, a(trader_transport)

rename traderq17 otherdairyprod

rename traderq19 otherprod

gen trade_year=substr(traderq21, 7, 4)
gen tradedairy_year=substr(traderq22, 7, 4)
destring tradedairy_year, replace

order trade_year tradedairy_year, a(traderq22)

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

