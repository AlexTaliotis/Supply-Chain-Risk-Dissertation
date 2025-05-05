ssc install estout, replace
eststo clear

clear
cd "C:\Users\chill\Documents\DISSERTATION\STATA outputs"
use "Regression data two variables trial 4.dta", clear

* ———————————————————————————————— 
* 2) Rename the 9 Xit* vars by hand
* ———————————————————————————————— 
rename XitFordMot~y        Xit_FordMotorCompany
rename XitTesla            Xit_Tesla
rename XitGeneralMot~s     Xit_GeneralMotors
rename XitBMWdat~s         Xit_BMW
rename XitHondad~s         Xit_Honda
rename XitMerced~s         Xit_MercedesBenz
rename XitNissand~o        Xit_Nissan
rename XitMazdad~o         Xit_Mazda
rename XitToyotad~o        Xit_Toyota

* ———————————————————————————————— 
* 3) Rename the 9 Cost* vars by hand
* ———————————————————————————————— 
rename FordMotorC~M        Cost_FordMotorCompany
rename TeslaCostofRa~r     Cost_Tesla
rename GeneralMot~r        Cost_GeneralMotors
rename BMWCostofRawM~n     Cost_BMW
rename HondaCostofRa~r     Cost_Honda
rename MercedesBe~i        Cost_MercedesBenz
rename NissanCostofRa~r    Cost_Nissan
rename MazdaCostofRa~r     Cost_Mazda
rename ToyotaCostofRa~r    Cost_Toyota

* check that you now have exactly nine of each
ds Xit_*   // should list Xit_FordMotorCompany … Xit_Toyota
ds Cost_*  // should list Cost_FordMotorCompany … Cost_Toyota


* ———————————————————————————————— 
* 4) Rename the 9 dit* vars by hand
* ———————————————————————————————— 
rename ditFordMot~y         dit_FordMotorCompany
rename ditTesla             dit_Tesla
rename ditGeneralMot~s      dit_GeneralMotors
rename ditBMWdat~s          dit_BMW
rename ditHondad~s          dit_Honda
rename ditMerced~s          dit_MercedesBenz
rename ditNissand~o         dit_Nissan
rename ditMazdad~o          dit_Mazda
rename ditToyotad~o         dit_Toyota

* check that you now have exactly nine of each
ds dit_*   // should list dit_FordMotorCompany … dit_Toyota


* ———————————————————————————————— 
* 5) Now reshape to long
* ———————————————————————————————— 
reshape long Xit_ dit_ Cost_, i(Quarter) j(company) string

* rename the molten variables
rename Xit_    Xit
rename dit_    dit
rename Cost_   unit_cost

* you should now have (#quarters × #firms) obs
count


* ———————————————————————————————— 
* 6) Make quarterly date and firm ID
* ———————————————————————————————— 
gen dateq = quarterly(Quarter, "YQ")
format dateq %tq
encode company, gen(company_id)


* ———————————————————————————————— 
* 7) Declare panel & check
* ———————————————————————————————— 
xtset company_id dateq
count            // should say (#quarters × #firms)


* ———————————————————————————————— 
* 8) Inspect
* ———————————————————————————————— 
*Discussion of distribution of data
list in 1/16    // first two quarters × all firms
summ Xit dit unit_cost


*———————————————————————————————
* 9) run pooled OLS
*———————————————————————————————
reg unit_cost Xit dit, robust
eststo m1

*POLS with controls
*reg unit_cost Xit dit PPIControl, robust
*reg unit_cost Xit dit PPIControl EmploymentCostIndexControl, robust

reg unit_cost Xit dit PPIControl  EmploymentCostIndexControl CPIUSControls, robust
eststo m2

*———————————————————————————————
* 10) Firm‐quarter fixed effects
*———————————————————————————————
*xtreg unit_cost Xit dit i.dateq, fe robust
*with clustered se. not recomended in this case as i am bootstrapping
xtreg unit_cost Xit dit i.dateq , fe vce(cluster company_id)
eststo m3



*———————————————————————————————
* 11) FE via OLS with firm & quarter dummies
*———————————————————————————————
*cluster for company gives you valid standard errors in the presence of arbitrary within‑firm serial correlation.
reg unit_cost Xit dit i.company_id i.dateq, vce(cluster company_id)
eststo m4

esttab m4 using "Appendix_Table1.rtf", replace rtf ///
    title("Appendix A1: Effect of Supply-Chain Risk on Unit Cost") ///
    label b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N, fmt(%9.0g) label("Observations")) ///
    addnote("Standard errors in parentheses, clustered by company_id.")

*comments: Focus on coefficient of Japaneese firms for delay: They are more reliant on JIT system so a delay results to a higher and significant cost reduction with toyota being the highest at -204

/*
*FE firm & Quarter + Controls
*robust is incorrect
reg unit_cost Xit dit i.company_id i.dateq, robust
*controls get ommited because of colinearity - not usefull
reg unit_cost Xit dit i.company_id i.dateq PPIControl  EmploymentCostIndexControl CPIUSControls, robust
*/


*———————————————————————————————
* 12) Bootstrap that exact same regression
*———————————————————————————————
bootstrap _b[Xit] _b[dit] _se[Xit] _se[dit], reps(1000) seed(12345): ///
    reg unit_cost Xit dit i.company_id i.dateq
	eststo m5	

esttab m5 using "Appendix_Table2.rtf", replace rtf ///
    title("Appendix A2: Bootstrap estimates of unit cost on supply‐chain risk") ///
    label                                         /// use variable labels
    b(3) se(3)                                    /// three decimals
    star(* 0.10 ** 0.05 *** 0.01)                 /// significance stars
    stats(N, fmt(%9.0g) label("Observations"))    /// N at bottom
    addnote("Bootstrap SEs in parentheses; 1,000 reps, seed=12345.") 
*———————————————————————————————
* 13) Create combined table (now reporting controls)
*———————————————————————————————
esttab m1 m2 m3 m4 m5 using "results_tableq22.rtf", replace ///
    b(3) se(2) star(* 0.10 ** 0.05 *** 0.01)     /// formatting
    keep(Xit dit PPIControl EmploymentCostIndexControl CPIUSControls) ///
    varlabels( ///
        Xit                        "Substitutability (Xit)"    ///
        dit                        "Delay (dit)"                ///
        PPIControl                 "PPI Control"                ///
        EmploymentCostIndexControl "ECI Control"                ///
        CPIUSControls              "CPI Control"                ///
    ) ///
    mtitles( ///
      "Pooled OLS"    /// m1
      "With Controls" /// m2
      "FE (xtreg)"    /// m3
      "FE OLS"        /// m4
      "Bootstrap"     /// m5
    ) ///
    title("Table 1. Effect of Supply‐Chain Risk on Unit Cost") ///
    label ///
    nodepvar	
	
*———————————————————————————————
* 14) Repeat regression with yearly dummies to see whatsapp
*———————————————————————————————
*create dummies
gen datey = 1960 + floor(dateq/4)
format datey %ty

*less singificant dit more significant Xit
xtreg unit_cost Xit dit i.company_id i.datey, vce(cluster company_id)
eststo m_year
*with controls both are significant -they don't get completely soaked up y dummies
xtreg unit_cost Xit dit i.company_id i.datey  PPIControl  EmploymentCostIndexControl CPIUSControls, vce(cluster company_id)
eststo m_year


* ————————————————————————————————
* Exclude COVID-era by dropping on the string Quarter
* ————————————————————————————————


    * drop all 2020Q1–2020Q4 and 2021Q1–2021Q4
drop if inlist(Quarter,                             ///
        "2020Q1", "2020Q2", "2020Q3", "2020Q4"              ///
        "2021Q1", "2021Q2", "2021Q3", "2021Q4")

    * re-declare panel (only necessary if you changed dateq/company_id)
xtset company_id dateq

    * re-run your quarterly FE
    xtreg unit_cost Xit dit i.company_id i.dateq, ///
        vce(cluster company_id)
    eststo m_nocovid




