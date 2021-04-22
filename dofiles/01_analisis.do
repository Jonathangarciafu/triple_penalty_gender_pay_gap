/*******************************************************************************
* Título:		Gender Pay Gap
* Autor:		Jonathan Garcia
********************************************************************************
	
*** Outline:
	0. Set initial configurations and globals
	1. RUNNING ID AND VARIABLE DOFILE, AND APPENDING THE PANEL 
	2. GENERATING WA, TIME, POPULATION AND NEW ID
	3. DECLARING THE PANEL
	4. CLEANING VARIABLES
	5. DESCRIPTIVE STATISTICS
	6. DETERMINANTS OF MONTHLY WAGES
			6.1 TEST FOR APPROPIATNESS OF THE ESTIMATOR
	7. BLINDER-OXACA DESCOMPOSITION 
	8. TRANSITONS JOB BASED CLASSIFICATION
	
	Input:	
	Output: 
	
*******************************************************************************/

********************************************************************************
*** 0. Set initial configurations and globals
********************************************************************************

*** 0.0 Install required packages:	
	local packages ietoolkit estout outreg2 oaxaca
		
	foreach pgks in `packages' {	
	  				
		capture which `pgks'
		
		if (_rc != 111) {
			display as text in smcl "Paquete {it:`pgks'} está instalado "
		}
		
		else {
			display as error in smcl `"Paquete {it:`pgks'} necesita instalarse."'
			
			capture ssc install `pgks', replace
			
			if (_rc == 601) {
				display as error in smcl `"Package `pgks' is not found at SSC;"' _newline ///
				`"Please check if {it:`pgks'} is spelled correctly and whether `pgks' is indeed a user-written command."'
			}
			
			else {
				display as result in smcl `"Paquete `pgks' ha sido instalado."'
			}
		}
	}
	
*** 0.1 General settings:		
	ieboilstart, versionnumber(14.2) custom(set varabbrev on) veryquietly
	`r(version)'
	
*** 0.2 Settings user's paths and other globals
	// Main path
	if ("`c(username)'" == "maximiliano") {
		global project 		"D:/Documents/GitHub/research-projects/gender-wage-gap-penalty"
	}	
	
	if ("`c(username)'" == "Jonathan G") {
		global project 		"D:\Dropbox\Papers\Triple Penalty\gender-wage-gap-penalty"
	}		
	
	// Paths
	global dofiles		"${project}/dofiles"
	global datasets		"${project}/data"
	global rawdata 		"${datasets}/raw"
	global workingdata 	"${datasets}/final"
	global outputs		"${project}/manuscript/tables"
	global programs		"${dofiles}/ado"

	// Export configurations
	global stars1	"label nolines nogaps fragment nomtitle nonumbers noobs nodep star(* 0.10 ** 0.05 *** 0.01) collabels(none) booktabs b(3) se(3)"
	global stars2	"label nolines nogaps fragment nomtitle nonumbers nodep star(* 0.10 ** 0.05 *** 0.01) collabels(none) booktabs r2 b(3) se(3)"	
	
	// Run custom commands 
	run "${programs}/xttrans2.do"
	
****************************************************************************
*** 1. RUNNING ID AND VARIABLE DOFILE, AND APPENDING THE PANEL 
****************************************************************************		

*** Load data 
	foreach num of numlist 20121 20122 20123 20124 {
		use "${rawdata}/ECH`num'", clear
		qui do "${dofiles}/02_construct_vars.do"
		save "${workingdata}/ECH`num'n", replace
	}

*** Appending dataset
	use "${workingdata}/ECH20121n", clear
	sort idindv trimestre

	foreach num of numlist 20122 20123 20124 {
		append using "${workingdata}/ECH`num'n"
	}

*** Save data
	save "${workingdata}/append2012.dta", replace 
	 
*************************************************************************
*** 2. GENERATING WA, TIME, POPULATION AND NEW ID
*************************************************************************

*** Load data
	use "${workingdata}/append2012.dta", clear
	
*** GENERATIONG WORKING AGE DUMMY
	gen WA=(s07p10>= 14 & s07p10<= 65) if s07p10!=.
	label variable WA "Population in the working age"

*** DROPING THOSE NOT IN THE WORKING AGE TO REDUCE ERROR MARGIN***
	drop if WA==0 

*** GENERATING TIME IN YEARS AND QUARTERS*
	gen time=trimestre
	tostring time, gen(str_time)
	gen year=substr(str_time,-2,2)
	destring year, replace  
	recode year (12=2012) 

	gen quarter=substr(str_time,-3,1)
	destring quarter, replace

*** GENERATING TIME VARIABLE FOR THE PANEL*
	gen qdate= yq( year, quarter)
	format qdate %tq

*** GENERATE NEW UNIQUE ID*
	sort idindvmse qdate s07p18 job_sts
	egen ID=group(idindvmse)
	label variable ID "Unique Panel ID"
	sort ID qdate
	bys ID: ge np=_n
	tab np

	order ID year quarter np

*** NEW ID FOR PANEL APPENDED*
	egen dupid=group(ID qdate)
	duplicates report dupid

	sort dupid s07p18 job_sts 
	quietly by dupid:  gen dupidt = cond(_N==1,0,_n)

	drop if dupidt==2  //2 observations deleted

*** Save dataset
	save "${workingdata}/append2012.dta", replace

******************************************************************
*** 3. DECLARING THE PANEL
******************************************************************

*** Load data
	use "${workingdata}/append2012.dta", clear

*** Declare panel
	sort ID qdate
	xtset ID qdate

*** Save data
	save "${workingdata}/append2012.dta", replace

******************************************************************************
*** 4. CLEANING VARIABLES
******************************************************************************
  
*** Load data
	use "${workingdata}/append2012.dta", clear

*** Generate variables 
	// Quarter dummies for fixed effects
	tab quarter, gen(quarterdummy_)

	// Replacing values
	local indepvar2 formcost s11p45 s11p11_1 s11p11_2 s11p33 s07p16 
	foreach x of local indepvar2 {
		replace `x'=0 if `x'==.
		replace `x'=0 if `x'==9
	}
	
*** Renaming and generating variables
	// Inschool wishjob
	rename s07p16 inschool
	rename s11p45 wishjob

	// Age
	rename s07p10 age
	ge age2=age^2
	label var age "age"
	label var age2 "age squared"

	// unemployed or inactive t-1 period
	gsort ID quarter
	bys ID: ge lagstatus=(job_sts2[_n-1]==5 | job_sts2[_n-1]==6)
	label var lagstatus "Previous quarter unemployed, inactive or OLF"

	// Number of workers
	rename s11p33 numworkers
	
	// Tipo contrato
	rename s11p31 typecontract

	// Mother and father
	ge motandfat=(mother==1 | father==1)
	
	// Mother and father and informal
	ge motandfatinfo=(motandfat==1 & status_form==1)

	// participate in the labor force
	ge lfp=(job_sts2<5)

	// HOURLY INCOM**
	gen hourincome=totalincome/totalhrs
	label var hourincome "Hourly Income of both primary and secondary occupation"
	gen lhourincome=log(hourincome)
	
	// Average age of the couple 
	bys idhh quarter: egen avg_age_couple=mean(age) if s07p08==1 | s07p08==2
	bys idhh quarter: egen couple_age=max(avg_age_couple)
	
	
*** Save dataset
	save "${workingdata}/append2012.dta", replace

******************************************************************************
*** 5. DESCRIPTIVE STATISTICS
******************************************************************************

*** Load data
	use "${rawdata}/ECH20124.dta", clear
	qui do "${dofiles}/02_construct_vars.do"

	gen WA=(s07p10>= 14 & s07p10<= 65) if s07p10!=.
	label variable WA "Population in the working age" 

	tab sex [aw=fajustexproyeccion]

*** occupaied
	ge occupaied=(status_occ==1 | status_occ==2) & WA==1
	replace occupaied=. if WA==0
  
  
	foreach x of varlist WA occupaied status_form area_residence younghw youngw2 older industry ecosect educl occatg{
		bys sex: tab `x' [aw=fajustexproyeccion]
	}
	
*** Hours worked last week
	rename s11p39c hrswrk

	foreach x of varlist hrswrk totalincome {
		bys sex: sum `x'
	}	

*** REMOVING OUTLIER FROM INCOME
	preserve
		sum 	totalincome if totalincome>0 , detail 
		drop if totalincome < r(p5) | totalincome > r(p95) 
		bys 	sex: sum totalincome
	restore

******************************************************************
*** 6. DETERMINANTS OF MONTHLY WAGES
******************************************************************	
	  
*** Load data
	use "${workingdata}/append2012.dta", clear
  
*** OLS
	eststo clear 
	eststo: reg lnpw mother job_sts2 married noch father lagstatus age age2 sex area_residence totalhrs i.socioeconomic_strata i.educl i.industry remittances pension schoolprograms HHsize inschool totalfwh i.quarter, r
	
*** OLS female
	eststo: reg lnpw mother job_sts2 married noch lagstatus age age2 area_residence totalhrs i.socioeconomic_strata i.educl i.industry remittances pension schoolprograms HHsize inschool totalfwh i.quarter if sex==1, r

*** OLS male
	eststo: reg lnpw job_sts2 married noch father lagstatus age age2 area_residence totalhrs i.socioeconomic_strata i.educl i.industry remittances pension schoolprograms HHsize inschool totalfwh i.quarter if sex==0, r

*** PANEL FIXED-EFFECTS
	eststo: xtreg lnpw mother job_sts2 noch father lagstatus age age2 i.industry totalhrs totalfwh  HHsize pension remittances schoolprograms inschool i.quarter, fe i(ID) 

*** FOR FEMALE FIXED-EFFECTS**
	eststo: xtreg lnpw  mother job_sts2  noch lagstatus age age2 i.industry totalhrs totalfwh  HHsize pension remittances schoolprograms inschool i.quarter if sex==1, fe i(ID) 

*** FOR MALE FIXED-EFFECTS**
	eststo: xtreg lnpw job_sts2 noch father lagstatus age age2 i.industry totalhrs totalfwh  HHsize pension remittances schoolprograms inschool i.quarter if sex==0, fe i(ID) 

	// Label vars
	label var sex 		"Mujer"
	label var mother 	"Madre"
	label var married 	"Casado/a"
	label var noch 		"Cantidad de hijos/as"
	label var father 	"Padre"
	label var lagstatus	"Interrupción laboral"
		
	// Export
	esttab using "${outputs}/mincer.tex", replace		///
		keep(sex mother married noch father lagstatus) order(sex mother married noch father lagstatus) ${stars2}

		******************************************************************
		*** 6.1 TEST APPROPIATNESS OF THE ESTIMATOR
		******************************************************************	
		
		*** RANDOM EFFECTS 
			eststo: xtreg lnpw mother job_sts2 noch father lagstatus age age2 i.industry totalhrs totalfwh  HHsize pension remittances schoolprograms inschool i.quarter, re i(ID) 	
				
		*** Haussman test 
				
			hausman est4 est7, sigmamore  // Test the appropriateness of the random-effects estimator 
	
******************************************************************
*** 7. BLINDER-OXACA DESCOMPOSITION 
******************************************************************
	
*** Load data
	use "${workingdata}/append2012.dta", clear
	
*** ALL SAMPLE 	
	eststo clear 
	preserve
		keep if  ltotalincome>0
		eststo: oaxaca totalincome age educl industry, by(sex) noisily
	restore
	
*** corrected for self-selection
	preserve
		keep if  ltotalincome>0
		eststo:  oaxaca ltotalincome age educl industry, by(sex) model1(heckman, select(age marital_status noch) twostep)
	restore
	 
*** mother and father
	preserve
		keep if motandfat==1 & ltotalincome>0
		eststo: oaxaca ltotalincome age educl industry, by(sex) noisily model1(heckman, select(age marital_status noch) twostep)
		eststo: oaxaca totalincome age educl industry, by(sex) noisily
	restore
		
*** mother and father and informal
	preserve
		keep if motandfatinfo==1 & ltotalincome>0
		eststo: oaxaca ltotalincome age educl industry, by(sex) noisily model1(heckman, select(age marital_status noch) twostep)
		eststo: oaxaca totalincome age educl industry, by(sex) noisily
	restore
	 
	// Export
	esttab 	est1 est4 est6 est2 est3 est5 using "${outputs}/oaxaca.tex", replace		///
			stats(N, fmt(%9.0f %9.2f) label("Observaciones")) 							///
			keep(group_1 group_2 difference endowments coefficients interaction) 		///
			${stars2}																	///
			coeflabels(group_1 "Hombre/Padre Salario" group_2 "Mujer/Madre Salario" difference "Diferencias" endowments "Dotación" coefficients "Coeficientes" interaction "Interacción")
			
******************************************************************
*** 8. TRANSITONS JOB BASED CLASSIFICATION
******************************************************************

*** Load data
	use "${workingdata}/append2012.dta", clear

	gen informtt=1 	if np==1 & status_form==0  		//Informal individual in t 
	gen formtt=1 	if np==1 & status_form==1  		//Formal individual in t
	label val job_sts2
   
	recode job_sts2 (3=4) (4=3)
	
*** METHOD TO OBTAIN MARKOV TRANSITION MATRIX  
	fillin ID qdate   /*Making a complete rectangularization of the database to obtain Markov Chain */

	forvalues x = 0/1 {
		xttrans2 job_sts2 if sex == `x'
		matrix m1 = r(table)
		matrix list m1 
		
		matrix rownames m1 = r1 r2 r3 r4 r5 r6
		matrix colnames m1 = c2 c3 c4 c5 c6 c7 
	  
		preserve 
			clear
			svmat m1, names(col) 
			
			tostring *, format("%2.1f") replace force
			gen c1 = ""
			order c1, first 
			
			replace c1 = "Trabajador asalariado informal"			if _n == 1
			replace c1 = "Trabajador asalariado formal" 			if _n == 2
			replace c1 = "Trabajador cuentapropista informal" 		if _n == 3			
			replace c1 = "Trabajador cuentapropista formal" 		if _n == 4
			replace c1 = "Desempleado" 								if _n == 5
			replace c1 = "Fuera de la fuerza laboral"				if _n == 6

			// Export to LaTeX
			replace c`c(k)' =  c`c(k)' + "\\"
		
			foreach col of varlist c1-c6 {
				replace `col' = `col' + "&"
			}
		
			// Export to TeX
			outsheet using "${outputs}/markow_sex_`x'.tex", 		////
					 nonames noquote nolabel replace
		restore   
	}
	
*** TRANSITION BY MOTHER AND FATHER*
	keep if motandfat==1
	
	forvalues x = 0/1 {
		xttrans2 job_sts2 if sex == `x'
		matrix m1 = r(table)
		matrix list m1 
		
		matrix rownames m1 = r1 r2 r3 r4 r5 r6
		matrix colnames m1 = c2 c3 c4 c5 c6 c7 
	  
		preserve 
			clear
			svmat m1, names(col) 
			
			tostring *, format("%2.1f") replace force
			gen c1 = ""
			order c1, first 
			
			replace c1 = "Trabajador asalariado informal"			if _n == 1
			replace c1 = "Trabajador asalariado formal" 			if _n == 2
			replace c1 = "Trabajador cuentapropista informal" 		if _n == 3			
			replace c1 = "Trabajador cuentapropista formal" 		if _n == 4
			replace c1 = "Desempleado" 								if _n == 5
			replace c1 = "Fuera de la fuerza laboral"				if _n == 6

			// Export to LaTeX
			replace c`c(k)' =  c`c(k)' + "\\"
		
			foreach col of varlist c1-c6 {
				replace `col' = `col' + "&"
			}
		
			// Export to TeX
			outsheet using "${outputs}/markow_motandfat_`x'.tex", 		////
					 nonames noquote nolabel replace
		restore   
	}
	
*** End
			