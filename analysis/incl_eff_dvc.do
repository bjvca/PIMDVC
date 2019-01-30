****************************
**	PIM
**	DAIRY FARMERS

eststo clear
clear

gl dir "C:\Users\u0107600\Dropbox\Uganda-Dairy"
*gl dir "C:\Users\Liz\Dropbox\Uganda-Dairy"

* use "$dir\Data\farmers.dta", clear
 use "$dir\analysis\tables\farmers_cleaned.dta", clear
*insheet using "https://raw.githubusercontent.com/bjvca/PIMDVC/master/data/public/farmers.csv", clear
cd "$dir\analysis\tables"
 
 
 
 
 
***Inclusivity and Efficiency of the Dairy Value Chain in Uganda



*Who is in the value chain?

		

	* Only use those observations who sell to one outlet
	drop if sales_oneoutlet==0

*Small group (67) sell to more than one outlet, we test to see if they are different than those who sell to only 1 outlet
ttest dry_total_liq,by(sales_oneoutlet)
ttest rainy_total_liq ,by(sales_oneoutlet)
ttest dry_price ,by(sales_oneoutlet)
ttest rainy_price ,by(sales_oneoutlet)
*Only difference are those who sell to more than one outlet have a higher price in the rainy season (sig at 0.04)
ttest main_income_dairy, by( sales_oneoutlet)
*Those who sell to more than one outlet are less likely to have dairy as their main source of income (sig at 0.098)



		*Characteristics of those who only sell to neighbors vs others
		*Dry season
		foreach v of varlist	dry_localcows dry_localcows_abv8 dry_localcows_liquid dry_exotics dry_exotics_abv8 dry_exotics_liqud dry_price dry_total dry_lowestpric dry_highestpric dry_sellday	dry_total_liq dry_income{
		ttest `v', by(sales_directneighbor)
		}
	/*	- less local cows milked ***
		- less local cows over 8 yrs ***
		- no diff in prod of local cows
		- less exotics cows milkd ***
		- less local cows over 8 yrs ***
		- no diff in prod of exotic cows
		- higher price **
		- no diff in total produc per cow per day
		- higher "lowest price "***
		- higher "highest price"***
		- sell less per day ***
		- less liters ***
		- less income ***
		*/
		
		
		*Rainy season
		foreach v of varlist	 rainy_localcows rainy_localcows_abv8 rainy_localcows_liquid rainy_exotics rainy_exotics_abv8 rainy_exotics_liqud rainy_price rainy_total rainy_lowestpric rainy_highestpric rainy_sellday	rainy_total_liq rainy_income{ 
		ttest `v', by(sales_directneighbor)
		}
	/*	- less local cows milked ***
		- less local cows over 8 yrs ***
		- no diff in prod of local cows
		- less exotics cows milkd ***
		- less local cows over 8 yrs ***
		- less prod of exotic cows *
		- higher price **
		- no diff in total produc per cow per day
		- higher "lowest price "***
		- higher "highest price"***
		- sell less per day ***
		- less liters ***
		- less income ***
		*/
		
		
		***Selling to neighbors are smaller producers with less cows. 

		
				*Assests 
		foreach v of varlist	 assets_milkshed assets_milkshed_roof assets_barn assets_trough assets_steelcan assets_steelcannum assets_steelbucsteelbuck assets_steelbucsteelbucknum assets_jerryc assets_jerrycnum assets_plasbuck assets_plasbucknum{ 
		ttest `v', by(sales_directneighbor)
		}
		***Those selling to neighbors less likely to have steel buckets, more likely ot have jerry cans and plastic containers, but if they do have these then they have less on average that those who sell to other outlets
		
		
			**Asset index
			foreach v of varlist assets_steelbucsteelbucknum assets_jerrycnum assets_plasbucknum{
			gen `v'_nom=0
			replace `v'_nom=`v' if `v'!=. 
			}
			gen asset_milkcoll=assets_steelbucsteelbucknum_nom +assets_jerrycnum_nom+ assets_plasbucknum_nom
			summarize asset_milkcoll, detail	
			*Median=3
			
			gen asset_abvmedian=0 
			replace asset_abvmedian=1 if asset_milkcoll>3
			
		ttest asset_milkcoll, by(sales_directneighbor)
		**less assets if sell to neighbor
		
		
		
		**Education
ttest head_edu , by(sales_directneighbor)		
ttest head_edu_primary , by(sales_directneighbor)		
ttest head_edu_some , by(sales_directneighbor)
*Those who sell to neighbor are more likely to have had some education, however no diff if main income is dairy
ttest head_edu_some if main_income_dairy ==1, by(sales_directneighbor)

			
		**Consumption 
		ttest hh_headhhconsumption_totalval, by(sales_directneighbor)
		*less consumption value if selling to neighbor ***
		
	***Table Comparing each season between the 4 different selling groups
	
	***Characteristics
	eststo clear
estpost sum hh_mems hh_head_female head_edu_primary dist_tarmac dist_murram dist_mcc dist_neigh dist_market if sales_directneighbor==1
eststo char1
estpost sum   hh_mems hh_head_female head_edu_primary dist_tarmac dist_murram dist_mcc dist_neigh dist_market if sales_tradetomcc==1
eststo char2
estpost sum   hh_mems hh_head_female head_edu_primary dist_tarmac dist_murram dist_mcc dist_neigh dist_market if sales_transptomcc==1
eststo char3
estpost sum  hh_mems hh_head_female head_edu_primary dist_tarmac dist_murram dist_mcc dist_neigh dist_market  if sales_directmcc==1
eststo char4
esttab char1 char2 char3 char4  using char.tex,replace  collabels(\multicolumn{1}{c}{{Mean}} \multicolumn{1}{c}{{Sd}}  \multicolumn{1}{l}{{Obs}}) cells("mean(fmt(2))  sd(fmt(1))  count(fmt(0))") label nonumber f noobs alignment(S) booktabs
		
	
	foreach v of varlist sales_directneighbor sales_tradetomcc sales_transptomcc sales_directmcc{

estpost sum dist_tarmac dist_murram dist_mcc dist_neigh dist_market if `v'==1
eststo dist_`v'
}
esttab dist_sales_directneighbor dist_sales_tradetomcc dist_sales_transptomcc dist_sales_directmcc using dist_sales.tex,replace  collabels(\multicolumn{1}{c}{{Mean}} \multicolumn{1}{c}{{Sd}}  \multicolumn{1}{l}{{Obs}}) cells("mean(fmt(1))  sd(fmt(1))  count(fmt(0))") label nonumber f noobs alignment(S) booktabs


	
	
	***GENDER
	
		foreach v of varlist sales_directneighbor sales_tradetomcc sales_transptomcc sales_directmcc{
		ttest hh_head_female, by(`v')
		}
		**The only sales outlet with less female headed households is direct to MCC (sig at 0.015)
		
		foreach v of varlist sales_neigh_price sales_tr_mcc_price sales_trans_mcc_price sales_direct_mcc_price{
		ttest `v', by(hh_head_female)
		}		
		*But the women that do sell direct to MCC, get a significantly better price (0.04), other outlets price is the same
		
		
			eststo clear
	local sales sales_neigh_ sales_tr_mcc_ sales_trans_mcc_ sales_direct_mcc_
	foreach l of local sales {
	 	 
		 foreach v of varlist `l'trans `l'num `l'price `l'amt   `l'yrs `l'qual_nevrej `l'prem {	
	  ttest `v', by(hh_head_female)
	
	 }
	 }
 
		
		




							**density plots
							{
							foreach v of varlist  sales_neigh_price  sales_tr_mcc_price   sales_trans_mcc_price  sales_direct_mcc_price {
							kdensity `v' , generate(x_`v'  d_`v') n(1549)
							}
							gen zero = 0

							
							foreach v of varlist  sales_neigh_price  sales_tr_mcc_price   sales_trans_mcc_price  sales_direct_mcc_price {
							

							. twoway  rarea d_`v' zero x_`v',  by(shed) ///
									ytitle("Smoothed density") ///
									xtitle("`v'")  
									graph export `v'.png, width(500) replace

								}
								
								
							. twoway  rarea d_sales_direct_mcc_price zero x_sales_direct_mcc_price, color("red") by(shed) ///
									ytitle("Smoothed density") ///
									xtitle("Price Sell Direct to MCC") 

							. twoway   rarea  d_sales_neigh_price zero x_sales_neigh_price , color("blue") by(shed) ///
									ytitle("Smoothed density") ///
									xtitle("Price Sell Direct to Neighbor") 



							. twoway   rarea d_sales_tr_mcc_price zero x_sales_tr_mcc_price, color("purple") by(shed) ///
									ytitle("Smoothed density") ///
									xtitle("Price Sell Trd to MCC")    
								
								}
	
	** Dry Season
	
	eststo clear
estpost sum dry_localcows dry_localcows_abv8 dry_localcows_liquid dry_exotics dry_exotics_abv8 dry_exotics_liqud dry_price dry_total dry_lowestpric dry_highestpric dry_sellday dry_consumeday dry_givenday dry_calvesday dry_processday dry_localcows_liquid_total dry_exotics_liqud_total dry_total_liq dry_income if sales_directneighbor==1
eststo dry1
estpost sum dry_localcows dry_localcows_abv8 dry_localcows_liquid dry_exotics dry_exotics_abv8 dry_exotics_liqud dry_price dry_total dry_lowestpric dry_highestpric dry_sellday dry_consumeday dry_givenday dry_calvesday dry_processday dry_localcows_liquid_total dry_exotics_liqud_total dry_total_liq dry_income if sales_tradetomcc==1
eststo dry2
estpost sum dry_localcows dry_localcows_abv8 dry_localcows_liquid dry_exotics dry_exotics_abv8 dry_exotics_liqud dry_price dry_total dry_lowestpric dry_highestpric dry_sellday dry_consumeday dry_givenday dry_calvesday dry_processday dry_localcows_liquid_total dry_exotics_liqud_total dry_total_liq dry_income if sales_transptomcc==1
eststo dry3
estpost sum dry_localcows dry_localcows_abv8 dry_localcows_liquid dry_exotics dry_exotics_abv8 dry_exotics_liqud dry_price dry_total dry_lowestpric dry_highestpric dry_sellday dry_consumeday dry_givenday dry_calvesday dry_processday dry_localcows_liquid_total dry_exotics_liqud_total dry_total_liq dry_income if sales_directmcc==1
eststo dry4


esttab dry1 dry2 dry3 dry4  using dry_sales.tex,replace  collabels(\multicolumn{1}{c}{{Mean}} \multicolumn{1}{c}{{Sd}}  \multicolumn{1}{l}{{Obs}}) cells("mean(fmt(1))  sd(fmt(1))  count(fmt(0))") label nonumber f noobs alignment(S) booktabs

*rainy season

	
	eststo clear
estpost sum rainy_localcows rainy_localcows_abv8 rainy_localcows_liquid rainy_exotics rainy_exotics_abv8 rainy_exotics_liqud rainy_price rainy_total rainy_lowestpric rainy_highestpric rainy_sellday rainy_consumeday rainy_givenday rainy_calvesday rainy_processday rainy_localcows_liquid_total rainy_exotics_liqud_total rainy_total_liq rainy_income if sales_directneighbor==1
eststo rainy1
estpost sum  rainy_localcows rainy_localcows_abv8 rainy_localcows_liquid rainy_exotics rainy_exotics_abv8 rainy_exotics_liqud rainy_price rainy_total rainy_lowestpric rainy_highestpric rainy_sellday rainy_consumeday rainy_givenday rainy_calvesday rainy_processday rainy_localcows_liquid_total rainy_exotics_liqud_total rainy_total_liq rainy_income if sales_tradetomcc==1
eststo rainy2
estpost sum  rainy_localcows rainy_localcows_abv8 rainy_localcows_liquid rainy_exotics rainy_exotics_abv8 rainy_exotics_liqud rainy_price rainy_total rainy_lowestpric rainy_highestpric rainy_sellday rainy_consumeday rainy_givenday rainy_calvesday rainy_processday rainy_localcows_liquid_total rainy_exotics_liqud_total rainy_total_liq rainy_income if sales_transptomcc==1
eststo rainy3
estpost sum  rainy_localcows rainy_localcows_abv8 rainy_localcows_liquid rainy_exotics rainy_exotics_abv8 rainy_exotics_liqud rainy_price rainy_total rainy_lowestpric rainy_highestpric rainy_sellday rainy_consumeday rainy_givenday rainy_calvesday rainy_processday rainy_localcows_liquid_total rainy_exotics_liqud_total rainy_total_liq rainy_income if sales_directmcc==1
eststo rainy4


esttab rainy1 rainy2 rainy3 rainy4  using rainy_sales.tex,replace  collabels(\multicolumn{1}{c}{{Mean}} \multicolumn{1}{c}{{Sd}}  \multicolumn{1}{l}{{Obs}}) cells("mean(fmt(1))  sd(fmt(1))  count(fmt(0))") label nonumber f noobs alignment(S) booktabs

		
	eststo clear
	local sales sales_neigh_ sales_tr_mcc_ sales_trans_mcc_ sales_direct_mcc_
	foreach l of local sales {
	 	 estpost sum `l'trans `l'num `l'price `l'amt `l'dec `l'freqpaym_dir `l'freqpaym_wk  `l'freqpaym_mon `l'contract_no `l'yrs `l'qual_nevrej `l'prem `l'assis_train `l'assis_input `l'assis_credit 	
	 eststo   `l'
	 }
esttab sales_neigh_ sales_tr_mcc_ sales_trans_mcc_ sales_direct_mcc_ using sales.tex,replace  collabels(\multicolumn{1}{c}{{Mean}} \multicolumn{1}{c}{{Sd}}  \multicolumn{1}{l}{{Obs}}) cells("mean(fmt(1))  sd(fmt(1))  count(fmt(0))") label nonumber f noobs alignment(S) booktabs
 
eststo clear	 

foreach v of varlist sales_directneighbor sales_tradetomcc sales_transptomcc sales_directmcc{

estpost sum main_income_dairy if `v'==1

}
		
		
*****************************************
*
*
*			TRADERS
*****************************************

 use "$dir\analysis\tables\traders_cleaned.dta", clear
 
 
 ***Those who mainly sell to coops vs indp vs procc
 sum main_buyer_indp main_buyer_coop main_buyer_proc
 
 eststo clear
 
 foreach v of varlist main_buyer_indp main_buyer_coop main_buyer_proc{
 estpost sum  dry_price  dry_avg_liters   if `v'==1 
 eststo dry_`v'
 }
  
 foreach v of varlist main_buyer_indp main_buyer_coop main_buyer_proc{
 estpost sum  rain_price  rain_avg_liters   if `v'==1 
eststo rain_`v'
 } 
  
 esttab dry_main_buyer_indp dry_main_buyer_coop dry_main_buyer_proc rain_main_buyer_indp rain_main_buyer_coop rain_main_buyer_proc using dry_rain_trader.tex,replace  collabels(\multicolumn{1}{c}{{Mean}} \multicolumn{1}{c}{{Sd}}  \multicolumn{1}{l}{{Obs}}) cells("mean(fmt(1))  sd(fmt(1))  count(fmt(0))") label nonumber f noobs alignment(S) booktabs

 ttest  trans_boda==trans_bike if main_buyer_indp==1
  ttest  trans_boda==trans_bike if main_buyer_coop==1
  ttest  trans_boda==trans_bike if main_buyer_proc==1

  
  
   foreach v of varlist trans_boda trans_bike{
 estpost sum  rain_price  rain_avg_liters   if `v'==1 
eststo rain_`v'
 } 
   foreach v of varlist trans_boda trans_bike{
 estpost sum  dry_price  dry_avg_liters   if `v'==1 
eststo dry`v'
 } 
 
  esttab rain_trans_boda rain_trans_bike drytrans_bike drytrans_boda using transp.tex,replace  collabels(\multicolumn{1}{c}{{Mean}} \multicolumn{1}{c}{{Sd}}  \multicolumn{1}{l}{{Obs}}) cells("mean(fmt(1))  sd(fmt(1))  count(fmt(0))") label nonumber f noobs alignment(S) booktabs

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  