*******************************************************************************************************************
*****  									 “THE GENDER PAY GAP IN NICARAGUA”	 							      *****
*****                              			  by: MACHO RATON                                             *****
*******************************************************************************************************************


******************************************************************
***         1. CREATE UNIQUE ID AND MERGIND DB                 *** 
******************************************************************


**1.2 Create unique ID for databases starting in 2012**

gen str4 s01p02str = string( s01p02 ,"%04.0f")  /*CODE: MNUNICIPIO, 4 DIGITS*/
gen str2 s01p06str= string( s01p06 ,"%02.0f")  /*CODE: AREA, 2 DIGITS; URBAN OR RURAL*/
gen str4 idstr = string( id ,"%04.0f")		   /*CODE: ID, 4 DIGITS; ID represents the unique identificator for selected HH in a PSU*/
gen str2 s07p00str = string( s07p00 ,"%02.0f") /*CODE: Person Code, INDIVIDUAL INDENTIFICATOR, 2 DIGITS*/
gen str2 s07p09str = string( s07p09 ,"%02.0f") /*Gender*/
gen str4 s07p11cstr = string( s07p11c ,"%04.0f") /*CODE: year of born, 4 DIGITS; represent the year of born, when a individual is dropped, is assigned same id for hh*/
gen str2 s07p11bstr = string( s07p11b ,"%02.0f") /*MONTH OF BIRTH*/
gen str2 s07p11astr = string( s07p11a ,"%02.0f") /*DAY OF BIRTH*/
gen str1 s07p08str = string( s07p08 ,"%01.0f") /*RELATION WITH THE HH*/
gen str2 s05p04str = string( s05p04 ,"%02.0f") /* no people in the HH*/
gen str1 s01p09str = string( s01p09 ,"%01.0f") /*UPM/estrato*/
gen str1 s07p18str = string(s07p18, "%01.0f") /*Marital status*/
gen str3 idupmstr = string(idupm, "%03.0f") /*UPMid*/
gen str2 s07p15astr = string( s07p15a ,"%02.0f") /* L of instruction*/
gen str1 s07p15bstr = string( s07p15b ,"%01.0f") /* year of instruction*/




*Creating a HH ID 
egen idhh=concat( s01p02str idupmstr  s01p06str  s01p09str   id  )
					  *MUN    *idupm   		AREA     strato   hhid



*UNIQUE INDIVIDUAL ID INCLUDES VARIABLES ABOVE*
*Different versions to check which one fit the best

egen idindv=concat( s01p02str idupmstr  s01p06str s07p08str  s07p00str s07p09str  s01p09str  s07p11astr  s07p11cstr  )
					  *MUN    *idupm   		AREA     HH realt indvcod     gender   UPM/estrato  day b    	year 
duplicates report idindv
sort idindv
quietly by idindv:  gen dupidind = cond(_N==1,0,_n)


*plus month
egen idindvm=concat( s01p02str idupmstr  s01p06str s07p08str  s07p00str s07p09str  s01p09str  s07p11astr s07p11bstr s07p11cstr  )
					   *MUN   *idupm 		AREA     HH realt indvcod     gender   UPM/estrato  day b    	m         year 
duplicates report idindvm
sort idindvm
quietly by idindvm:  gen dupidindm = cond(_N==1,0,_n)

*maritalstatus

egen idindvms=concat( s01p02str idupmstr  s01p06str s07p08str  s07p00str s07p09str  s01p09str  s07p11astr s07p11bstr s07p11cstr s07p18str )
					    *MUN    *idupm   AREA     HH realt indvcod     gender   UPM/estrato  day b    	m         year        MS        
duplicates report idindvms
sort idindvms
quietly by idindvms:  gen dupidindms = cond(_N==1,0,_n)


*plus educlevel and year

egen idindvmse=concat( s01p02str idupmstr  s01p06str s07p08str  s07p00str s07p09str  s01p09str  s07p11astr s07p11bstr s07p11cstr s07p18str s07p15astr s07p15b  )
					    *MUN      *idupm  	AREA     HH realt indvcod     gender   UPM/estrato  day b    	m         year        MS          ed        ye
duplicates report idindvmse
sort idindvmse
quietly by idindvmse:  gen dupidindmse = cond(_N==1,0,_n)






    ******************************************************************
    ***                    2. CREATING STATUS                      *** 
    ******************************************************************


** 2.1 Create a variable of labour market status using ILO definiton of unemployment icls19**

	generate status_occ = .
	

/* Occupied employment if she/he did any work of any type for pay, profit, barter or home use during last week, 
and the total number of hours worked each day last week in all activities is at least equal to 1.*/

	replace status_occ = 1 if s11p01 == 1  /*Declared that worked at least 1 hour the last week*/
	replace status_occ = 1 if s11p27 <= 7  /*worked as employe, own-account, member of a cooperative, other family work */
	replace status_occ = 1 if s11p02 <= 9  /*Declared that conducted last week at least any of activities such as: farming, sell products, make products to sell, provide services, among others
	meeting with the criteria stablished in the 19th ICLS that includes:
	
	(a) own-use production work comprising production of goods and services for own final use;

	(b) employment work comprising work performed for others in exchange for pay or profit;

	(c) unpaid trainee work comprising work performed for others without pay to acquire workplace experience or skills;

	(d) volunteer work comprising non-compulsory work performed for others without pay;

	(e) other work activities (not defined in this resolution).*/

	
	tab status_occ, m

more

	
/* Occupied if she/he did have a job or own farm or enterprise to which she/he will definitely return to.*/


	replace status_occ = 1 if s11p03== 1
	
	
/* Unemployed (standard definition of ilo) if she/he did not do any work of any type for pay, profit, 
barter or home use during last week, and she/he did not have a job or own farm or enterprise to which 
she/he will definitely return to, but she/he was available for work last week and she/he had taken 
any steps within the past 4 weeks to look for work. */

	gen realseek=s11p07
	replace realseek=2 if s11p11_9==9  /*Double check question, after asking if the person seek for a job during the last week, the survey asked what do they did, we change the value to not seek for a job*/
	
	replace status_occ = 2 if status_occ==. & (realseek==1 |s11p11_1==1 |s11p11_2==2 |s11p11_3==3 |s11p11_4==4 |s11p11_5==5 |s11p11_6==6 |s11p11_7==7 |s11p11_8==8 ) & s11p11_9!=9 & s11p10==1
	

*Inactive if neither occupied nor unemployed * before 2010 the active population include people over 10 years up to 60 year, the retirment age*

	codebook s11p12
	replace status_occ = 3 if status_occ ==. & s11p11_9==9 /*inactive, did not conduct any actions to activly seek for a job*/
	replace status_occ = 3 if s07p10<14 /*inactive in age*/
    replace status_occ = 3 if status_occ ==. & s11p12<=7  /*inactive*/

label define  status_occ_lb 1 "Occupied" 2 "Unemployed" 3 "Inactive population"
label values status_occ status_occ_lb

tab status_occ, m

**TEST UNEMPLOYMENT RATE WITH OFFICIAL STATISTICS**
ge status_test=.
replace status_test=1 if status_occ==1
replace status_test=2 if status_occ==2
tab status_test [aw= fajustexproyeccion]

more
 
******************************************************************
***               3. LABOUR FORCE                            *** 
******************************************************************
	
	gen LF=1 if (s07p10 >= 14 & s07p10<= 65) & s11p12==.     /*Labour force definition using Nicaragua parameter. ILO standar 15 and 64*/
	replace LF=0 if LF!=1
 
*********************************************************************
***            4. INDUSTRY CLASSIFICATION                      *** 
******************************************************************


*RECODE BASED ON THE INTERNATIONAL CLASSIFICATION

gen industry=0
replace indust=1 if s11p26>=111 & s11p26<=200
replace indust=1 if s11p26>=500 & s11p26<=600

replace indust=2 if s11p26>=1300 & s11p26<=1430
replace indust=2 if s11p26>=1500 & s11p26<=4000
replace indust=2 if s11p26>=4005 & s11p26<=4200

replace indust=3 if s11p26>=4500 & s11p26<=4600

replace indust=4 if s11p26>=5000 & s11p26<=5300
replace indust=4 if s11p26>=5500 & s11p26<=6000

replace indust=5 if s11p26>=6010 & s11p26<=6450
replace indust=5 if s11p26>=6500 & s11p26<=6800
replace indust=5 if s11p26>=7000 & s11p26<=7500
replace indust=5 if s11p26>=9500 & s11p26<=9700
replace indust=5 if s11p26==9800 


replace indust=6 if s11p26>=8000 & s11p26<=8100
replace indust=6 if s11p26>=8500 & s11p26<=8600
replace indust=6 if s11p26>=9000 & s11p26<=9320

replace indust=7 if s11p26>=7505 & s11p26<=7600



*9999 no declared, 9998 does not know or not clear*

**Labeling economic activities***

label define industrylb 1 "Agriculture, hunting, forestry and Fishing" ///
2 "Industry"  /// /*: Mining and quarrying; Manufacturing; Electricity, gas and water supply*/
3 "Construcction"  ///
4 "Trade, hotels and restaurants" /// /* includes:  Wholesale and retail trade; Hoteles and restaurants; 
5 "Services" /// /* Transport, storage and communication; Financial intermediation; Real estate, renting un business activities 16 "Private Households with employed persons" 17 "Extra-territorial organizations and bodies"*/
6 "Education, health and social protection" /// /* "Education" 14 "Health and social work" 15 "Other community, social and personal service activities" */
7 "Public administration and defence" 

label values indust industrylb
label variable indust "Industry International Classification"

tab industry 

more
*Generate macro-economic sectors*

gen ecosect=.
replace ecosect=1 if indust==1 
replace ecosect=2 if indust==2 | indust==3 
replace ecosect=3 if indust==4 | indust==5 | indust==6 | indust==7 

label variable ecosect "Macro-economic sectors"
label define  economicsector 1 "Primary Sector" 2 "Secondary Sector" 3 "Tertiary Sector"
label values ecosect economicsector


tab ecosect, m



	
******************************************************************
***               5. Social Security                            *** 
******************************************************************

gen ss=.
replace ss=1 if (s07p12==1 | s07p12==4) & status_occ==1
replace ss=0 if ss!=1 & status_occ==1
label variable ss "Mandatory Social Security"
label define socialsecurity 1 "Mandatory Social Security" 0 "No mandatory SS"
label values ss socialsecurity
	


	
******************************************************************
***     6. STATUS in emplyoment CLASSIFICATION                 *** 
******************************************************************
*International Classification of Status in Employment according ILO definitions*

gen ICSE=.
replace ICSE=1 if status_occ==1  & (s11p27==1 | s11p27==2) /*Salaried workers, including forman and informal*/
label variable ICSE "International Classification of Status in Employment, ILO"

		*Include those that took vacations or permise for personal tramits* * FORMULARIE MISTAKE 1/2*
	replace ICSE=1 if status_occ==1  & (s11p04>=1 & s11p04<=7) & (s12p49_1p01>0 | s12p49_1s01>0)  /* replaces*/


*Own account workers*

	* (EMPLOYEERS) Own account worker and employers employed in their own formal or informal sector enterprise*
	replace ICSE=2 if status_occ==1 & s11p27==3 
	
	*(OWN-ACCOUNT WORKERS) Own account workers engaged in the production of goods exclusively for the final use of the HH*
	replace ICSE=2 if status_occ==1 & s11p27==4 
		*Include own-accounts that did not worked last week but they will return to their jobs and the cause of their absence was lack of clients, market, personal matter or inputs* FORMULARIE MISTAKE 2/2*
	replace ICSE=2 if status_occ==1 & ICSE==. & (s11p04>=8 & s11p04<=14) & (s12p50_1p01>0 | s12p50_1s01>0)  /*66 replaces*/
	
	*(CONTRIBUTING FAMILY WORKERS) Contributing family workers in the formal or informal sector enterprise*
	replace ICSE=2 if status_occ==1 & (s11p27==6 | s11p27==7) 
	
	*MEMBER OF PRODUCERS COOPERATIVES
	replace ICSE=2 if status_occ==1 & s11p27==5
	
	
	**FINDE THE ISSUE TO MAKE IT MATHC WITH STATUS OCC
	
	
*Unemployed*
replace ICSE=3 if status_occ==2 
*OLF/inactive*
replace ICSE=4 if status_occ==3 & s11p12==.  /*Out of the labor force*/

*Inactive*
replace ICSE=. if s11p27==9  /*deleting observation that reported ignore the type of work that performs*/

*Complete inactive*
replace ICSE=4 if status_occ==3 & ICSE==.
replace ICSE=4 if status_occ==1 & s11p12!=. & ICSE==.  // people that said did not work, but will return to work however they answered s11p12 and did not answered the rest of questions for occupaied people


label define ICSEn 1 "Salaried workers" 2 "Self-employed/Own-account" 3 "Unemployed" 4 "OLF/inactive"

*label define ICSElab 1 "Informal Salaried Workers" 2 "Informal Employeers" 3 "Informal Own-account workers" 4 "Informal contributing family workers" 5 "Member of Producer Cooperatives" 6 "Unemployed" 7 "Out of the LF" 8 "Inactive"
label values ICSE ICSEn

 tab ICSE, m
 
 *******236 where do they come?***** Individuals that declared not conducted any work during the last week but decleared to have a job where to return, they absence was due to lack of input, market conditions, credict, etc. This are mostly own-account workers
 
 more

 
******************************************************************
***               7. INFORMALITY                            *** 
******************************************************************

*INFORMAL SECTOR ENTERRISE*

gen ise=0  
replace ise=1 if s11p32>1 & s11p32!=.  /*Informal sector enterprise*/
/*Economic units with non accountable books. This includes agriculture activities. ///
According to ILO, the common recommendation of excluding agiculture is due to possible differences in data collection ///
However, the Labor suvey in Nicaragua was designed to cover urban and rural areas and incomes in species are monetarized during the survey*/

label variable ise "Informal Sector enterprise, ILO definition"

 tab ise, m
******************************************************************
***      8. EMPLOYMENT JOB-BASED STATUS FOR THE MODEL          *** 
******************************************************************
gen job_sts=.

*Employees 
	/*Employees holding formal jobs weather in the formal or informal sector enterprise (Informal contract (lack of written contract) ///
	not entitle to paid annual or sick leave, casual or temporary nature of work or not coverage by SS)*/
	
		*INFORMAL*
	ge infcontrac=1 if s11p31!=1 & s11p31!=2
	replace job_sts=1 if ICSE==1 & (ise==1 | ss!=1 | infcontrac==1)
	
	*formal salaried
	replace job_sts=2 if ICSE==1  & job_sts!=1 
	
*Own account workers formal and informal*
	*EMPLOYEERS, OWN-ACCOUNT WORKERS, CONTRIBUTING FAMILY WORKERS AND MEMBERS OF PRODUCER COOPERATIVES*
	replace job_sts=3 if ICSE==2
	
	*INFORMAL OWN-ACCOUNT*
	replace job_sts=3 if (ICSE==2) & ise==1

*Unemployed*
replace job_sts=4 if ICSE==3
*OLF*
replace job_sts=5 if ICSE==4 & s11p12==. 
*Inactive*
replace job_sts=5 if ICSE==4

tab job_sts


label define job_status 1 "Informal salaried Workers" 2 "Formal Salaried Workers" 3 "Self-employed/Own-account workers" 4 "Unemployed" 5 "Out of the Labor Force/Inactive"
label values job_sts job_status 
label variable job_sts "Status in Employment, Job-based"

tab job_sts, m


**VERSION TWO, IDENTIFIED AFTER RUNNING THE MODELS AND FINFING MANY MISSING VALUES FOR SOME CLASSIFICATIONS


gen job_sts2=.

*Employees 
	/*Employees holding formal jobs weather in the formal or informal sector enterprise (Informal contract (lack of written contract) ///
	not entitle to paid annual or sick leave, casual or temporary nature of work or not coverage by SS)*/
	
		*INFORMAL*
	replace job_sts2=1 if ICSE==1 & (ise==1 | ss!=1 | infcontrac==1)
	
	*formal salaried
	replace job_sts2=2 if ICSE==1  & job_sts2!=1 
	
*Own account workers formal and informal*
	*EMPLOYEERS, OWN-ACCOUNT WORKERS, CONTRIBUTING FAMILY WORKERS AND MEMBERS OF PRODUCER COOPERATIVES*
	replace job_sts2=3 if ICSE==2
	
	*INFORMAL OWN-ACCOUNT*
	replace job_sts2=4 if (ICSE==2) & ise==1

*Unemployed*
replace job_sts2=5 if ICSE==3
*OLF*
replace job_sts2=6 if ICSE==4 & s11p12==. 
*Inactive*
replace job_sts2=6 if ICSE==4

tab job_sts2


label define job_status2 1 "Informal salaried Workers" 2 "Formal Salaried Workers" 3 "Formal Self-employed/Own-account workers" 4 "Informal Self-employed/Own-account workers" 5 "Unemployed" 6 "Out of the Labor Force/Inactive"
label values job_sts2 job_status2 
label variable job_sts2 "Status in Employment, Job-based version 2"

/*Missin values correspond to individuals that responded that did not worked the last week but they did have a job to return, most of the cases correspond to own-account workers ///
  however, in these missing values the enumerators did not continue the formularie as a occupaiedd person, those who contain more information were replace in the status classifications*/


******************************************************************
***         9. Formal/informal bynary job-based                *** 
****************************************************************** 

ge status_form=(status_occ==1 & (ise==1 | ss==0 | (s11p31!=1 & s11p31!=2)) & job_sts!=2)
replace status_form=. if job_sts>3 | job_sts==.
replace status_form=0 if job_sts==3 & (ise==0 | ss==1)
label variable status_form "Aggregated fomality status"
label define form 1 "Informal" 0 "Formal"
label values status_form form
tab status_form,m

*number of formal workers in the hh*
ge formbinary=status_form
recode formbinary (1=0) (0=1)
bys idhh: egen thhfw=sum(formbinary)  // total formal workers by hh*
bys idhh: egen maxfw=max(thhfw)  // max total fw
ge totalfwh=maxfw
replace totalfwh=totalfwh-1 if formbinary==1
label var totalfwh "Total formal workers in the HH"

  
  
		******************************************************************
		***               UNDEREMPLOYMENT                              *** 
		******************************************************************
		ge vunderempl=.
		replace vunderempl=1 if horas_40==1 & s11p43==1 & status_occ==1  // underemployed if occupaied, works less than 40 hours and want to works more hours
		replace vunderempl=0 if vunderempl!=1 & status_occ<3
		label var vunderempl "Visible underemployment"
		
		clonevar status_underempl=vunderempl
		replace status_underempl=2 if status_underempl==0
		replace status_underempl=3 if job_sts==4
		replace status_underempl=4 if job_sts==5

		label define  under_status 1 "Underemployed Workers" 2 "Non-underemployed Workers" 3 "Unemployed" 4 "Out of the Labor Force/Inactive"
		label values status_underempl under_status 
		label variable status_underempl "Status in Undermployment"
		
		
		
		
  
******************************************************************
***               10. YOUNG AND OLDER                          *** 
******************************************************************
*According to the United Nations definition with to cohort of youth*

ge younghw=1 if (s07p10>=14 & s07p10<=24) & job_sts!=. /*First cohort*/
ge youngw2=1 if (s07p10>=14 & s07p10<=29) & job_sts!=. /*Second cohort including up to 29 years old*/
ge oage=1 if (s07p10>=30 & s07p10<=59) & job_sts!=. 
ge older=1 if s07p10<=60 & job_sts!=.  // UN definition

label variable younghw "Young Workers up to 24"
label variable youngw2 "Young Workers up to 29"
label variable oage "Age Between 30 and 49"
label variable older "Workers after 50 years old"

foreach x of varlist young*{
  replace `x' = 0 if(`x'==.)
}

label define young 1 "Young" 0 "Not Young"
label values younghw young
tab younghw 

ge agegroup=0
replace agegroup=0 if youngw2==1
replace agegroup=1 if oage==1
replace agegroup=2 if older==1

label var agegroup "Groups of age"
label var agegroup "Age groups"
label define ages 0 "Young Workers up to 29" 1 "Age Between 30 and 49" 2 "Workers after 50 years old"
label value agegroup ages

******************************************************************
***      11. YEARS OF EDUCATION/ LEVEL OF EDUCATION             *** 
******************************************************************

ge educ0=1 if s07p15a==0
label variable educ0 "No education"

ge educ1=1 if s07p15a==1
replace educ1=1 if s07p15a==2 & s07p15b<6
label variable educ1 "Incomplete primary"

ge educ2=1 if s07p15a==2 & s07p15b>=6
label variable educ2 "Complete primary"

ge educ3=1 if s07p15a==3 & s07p15b<5
label variable educ3 "Incomplete ordinary secondary"

ge educ4=1 if s07p15a==3 & s07p15b>=5
label variable educ4 "Complete ordinary secondary"

ge educ5=1 if s07p15a>=4 & s07p15a<=6
label variable educ5 "Technical education"

ge educ6=1 if s07p15a>=7 & s07p15a<=9
label variable educ6 "Advance secondary or tertiary education"

gen educl=educ0

forvalues i=0/6 {
 replace educl=`i' if educ`i'==1
 }

foreach x of varlist educ*{
  replace `x' = 0 if(`x'==.)
}

label var educl "Education level"
label define educ 0 "No education" 1 "Incomplete primary" 2 "Complete primary" 3 "Incomplete ordinary secondary" 4 "Complete ordinary secondary" 5 "Technical Education" 6 "Advance secondary or tertiary education"
label value educl educ

******************************************************************
***              12. OCCUPATION CATEGORY                       *** 
******************************************************************
**Using OECD and EUROSTATA classification**

gen occatg=. 
replace occatg=3 if s11p24>=1100 & s11p24<=3999
replace occatg=2 if s11p24>=4000 & s11p24<=5999
replace occatg=1 if s11p24>=6000 & s11p24<=7999
replace occatg=0 if s11p24>=8000 & s11p24<=9999

label define  occupcateg 3 "High skilled white collar" 2 "Low skilled white collar" 1 "High skilled blue collar" 0 "Low skilled blue collar"
label values occatg occupcateg
label variable occatg "Occupational categories, based on the SOC"
tab occatg
	

******************************************************************
***              13. lABOR INCOME                       *** 
******************************************************************
foreach x of varlist s12p49* s12p50*{
  replace `x' = 0 if(`x'==. |`x'==999999 |`x'==9999999999)
}

foreach x of varlist s12p49* s12p50*{
	 summ `x' 
}

****INCOME FOR SALARIED WORKERS****

*Nominal income, salary agreed with the employer*
sum s12p49b /*Primary occupation*/
sum s12p49c /*Secondary occupation*/

foreach x of varlist s12p49b s12p49c{
  replace `x' = 0 if(`x'==. |`x'==999999 |`x'==9999999999)
  sum `x'
}


*Portion of the income substracted for taxes and social security*
ge taxpincome= s12p49b - s12p49_1p01
ge taxsincome= s12p49c - s12p49_1s01


foreach x of varlist taxpincom taxsincome{
  replace `x' = 0 if(`x'<0)
  sum `x'
}


su taxpincome
su taxsincome

*Income after deductions of taxes and SS plus other incomes such as diets, transport, housing, among others*
foreach x of varlist s12p49_1p* s12p49_1s* {
  replace `x' = 0 if(`x'==. |`x'==999999 |`x'==9999999999)
  sum `x'
}

egen pincsw= rowtotal(s12p49_1p*)
egen sincsw= rowtotal(s12p49_1s*) 
ge pincomesw= pincsw - s12p49_1p11
ge sincomesw= sincsw - s12p49_1s11

sum pincomesw
sum sincomesw


*Additional formal income* income from 13th month, pay hollidays and bonus*
ge afpincomsw= s12p49_2mp12 + s12p49_2mp13 + s12p49_2mp14  //Incomes already defined as monthly in the data base, primary activity 
ge afsincomesw= s12p49_2ms12 + s12p49_2ms13 + s12p49_2ms14 // Secondary activity

sum afpincomsw
sum afsincomesw

*Other payments in species received by the worker according to their willingness to pay in local currency*
egen  spcpincsw= rowtotal(s12p49_3p*) 
egen  spcsincsw= rowtotal(s12p49_3s*)
ge spcpincomsws= spcpincsw-s12p49_3p23
ge spcsincomsw= spcsincsw-s12p49_3s23

sum spcpincomsw
sum spcsincomsw

*TOTAL NET lABOR INCOME* 
ge totalswprincome= pincomesw + afpincomsw + spcpincomsw
ge totalswsecincome= sincomesw + afsincomesw + spcsincomsw
label variable totalswprincome "Total Net Labor Income Salaried Worker- primary occupation"
label variable totalswsecincome "Total Net Labor Income Salaried Worker- secondary occupation"

sum totalswprincome if status_occ==1
sum totalswsecincome if status_occ==1

****INCOME FOR OWN-ACCOUNT WORKERS***
*Cleaning reported income from missing values to be able to add
foreach x of varlist s12p50_*{
  replace `x' = 0 if(`x'==. |`x'==999999 |`x'==9999999999)
  sum `x'
}


*Cleaning possible duplication in declared income ///
*Earning and expenses reported probably twice money: used by the house (s12p50_1p01 s12p50_3p01) equals earnings reported (s12p50_1s01 s12p50_3s01)*
replace s12p50_1p01=0 if s12p50_1p01==s12p50_3p01
replace s12p50_1s01=0 if s12p50_1s01==s12p50_3s01


*Generating total income of Own Account workers
ge totalpowpincom=s12p50_1p01 + s12p50_2p01 + s12p50_3p01 
ge totalsowsincom= s12p50_1s01 + s12p50_2s01 + s12p50_3s01

su totalpowpincom
su totalsowsincom

label variable totalpowpincom "Total Net Labor Income Own-account Worker- primary occupation"
label variable totalsowsincom "Total Net Labor Income Own-account Worker- secondary occupation"

******************************************************************
***             14  . NON -lABOR INCOME                       *** 
******************************************************************
*Cleaning non-labor income variables
foreach x of varlist s12p51_* s12p52_* s12p53_* s12p54_*{
  replace `x' = 0 if (`x'==. |`x'==999999 |`x'==9999999999)
  sum `x'
}	
	 
egen totalnlaborinc= rowtotal(s12p51_* s12p52_* s12p53_* s12p54_*)
	
	
	**AGRICULTURE, LIVESTOCK AND FISHING INCOME**
	**Cleaning income derived from agrictulture and other primary activities*
	
foreach x of varlist s12p59_* s12p60_* s12p61_* s12p62_* s12p63_* s12p66_* s12p67_* s12p68_* s12p69_* s12p71 s12p73{
  replace `x' = 0 if (`x'==. |`x'==999999 |`x'==9999999999)
  sum `x'
}	
		

		*Generation Total Non Labor Income*
egen grosprimactinco=rowtotal(s12p59_* s12p60_* s12p61_* s12p62_* s12p63_* s12p66_* s12p67_* s12p68_* s12p69_* s12p71 s12p73)
egen intrmdcosts=rowtotal(s12p63_* s12p69_* s12p73)
ge netprimactincom=grosprimactinco-intrmdcosts


******************************************************************
***          15. TOTAL-INCOME LABOUR AND NOT LABOUR            *** 
******************************************************************
*****Income including non-labour income, addded to indentify any possible effect of formalization status in all incomes***
  
ge totalincomeww= totalswprincome + totalswsecincome + totalnlaborinc
ge totalincomeow= totalpowpincom + totalsowsincom + totalnlaborinc

su totalincomeww
su totalincomeow

		ge totalincomeww1= totalswprincome + totalswsecincome
		ge totalincomeow1= totalpowpincom + totalsowsincom 
		replace totalincomeww1=0 if totalincomeww1==.
		replace totalincomeow1=0 if totalincomeow1==.

		
		
		*****TOTAL LABOR INCOME for salaried worker and own-account workers**
		ge totalincome=totalincomeww1+totalincomeow1
		ge ltotalincome=log(totalincome)

	
	  *********************************************************
      ***  16. lOW PAY WORKERS: 2/3 median/mean wage OECD    *** 38 hour full time worker
      *********************************************************
	
*According to the literature review and different criterias used, the median of the formal 	
foreach x of varlist totalsw* totalpow*{
  replace `x' = . if (`x'==0)
  sum `x'
}	
	
foreach x of varlist s11p39*{
  replace `x' = . if (`x'==999)
  sum `x'
}	

  *Generating Median Wage full time workers*
  su totalswprincome if (job_sts==1 | job_sts==2) [aw= fajustexproyeccion]
  su totalswprincome if (job_sts==1 | job_sts==2) & s11p39a>=38 , d
  
  scalar inc_swm=r(mean)
  scalar inc_swp50=r(p50)

  scalar dir _all 
  
  ge THmean=inc_swm*(2/3)  /*Threashold 2/3 of the mean wage of full time salaried workers*/
  ge THmedian=inc_swp50*(2/3) /*Threashold 2/3 of the median wage of full time salaried workers*/
  
  
  *Generate low-pay workers*
  ge low_pay=0 & status_occ==1
  replace low_pay=1 if (job_sts==1 | job_sts==2) & totalswprincome<THmedian
  label variable low_pay "Low pay workers, OECD"
  label define lpw 1 "Low Pay" 0 "No Low Pay"
  label value low_pay lpw
  
  tab low_pay
  
 *********************************************************
 ***          17. GENERATING OTHER VARIABLES        *** 
 ********************************************************* 
	
	*Log Wages and income
		ge lnpw=ln(totalswprincome)
		label var lnpw "Log Salaried Worker Wage, primary occupation"
		ge lnpw2=ln(totalswsecincome)
		label variable lnpw2 "Log Salaried Worker Wage, secondary occupation"

		ge ltotalincomSW=ln(totalincomeww)
		label var ltotalincomSW "Log Total Income Salaried Workers"
		ge ltotalincomOW=ln(totalincomeow)
		label var ltotalincomOW "Log Total Own-account Workers"
		
	*Log of costs of formalization
	gen formcost= log(afpincomsw)
	label var formcost "Cost of being formal"
  

 *********************************************************
 ***   18. RECODE AND OTHER VARIABLES FOR THE MODEL   *** 
 ********************************************************* 	  
 recode s07p09 (1=0) (2=1)
 label define gender 0 "Male" 1 "Female" 
 label values s07p09 gender
 
 
 foreach x of varlist s07p18{
  replace `x' = . if (`x'==8 |`x'==9)
}	
 
*RENAME BEFORE THE MODEL
	rename s07p09 sex
	rename s01p09 socioeconomic_strata
	rename s07p18 marital_status
	rename s01p06 area_residence

  label var socioeconomic_strata "Socio-economic strata"
  label define socioest 1 "Low" 2 "Medium" 3 "High" 4 "Very High"
  label value socioeconomic_strata socioest
	  

	  
	  *HOURS WORKED last week primary and secondary occupation
	 foreach x of varlist s11p39a s11p39b{
  replace `x' = . if (`x'==999 |`x'==9)
}	  
	 label var s11p39a "Hours Worked Last Week Primary Occupation"
	 label var s11p39b "Hours Worked Last Week Secondary Occupation"
	  
	  
	 **TOTAL Hours worked primary and secondary occupation*/
	  clonevar  prhrsw=s11p39a
	  replace prhrsw=0 if prhrsw==.
	  clonevar sdhrsw=s11p39a
	  replace sdhrsw=0 if sdhrsw==.
	  
			*TOTAL HOURS BOTH OCCUPATIONS*
	 ge totalhrs=prhrsw+sdhrsw
	 label var totalhrs "Total Hours primary and secondary occupation"
	  
	  
	  
 *********************************************************
 ***   19. ADDITIONAL VARIABLES TO THE MODEL   *** 
 ********************************************************* 	
	
		*Number of CH - by hh count if son or daughter exist and age
	gen ch=(s07p10<13) if !missing(s07p10) // Child according law no. 2087 art.2
	
	bys idhh: egen noch=sum(ch)
	label var noch "Number of CH in the HH"
	
	*MOTHER - only way to aprox is with hh relationship wife or head of hh female with CH
	ge fhh=(s07p08==1 & sex==1)
	label var fhh "Head of household is female"
	ge mother=(fhh==1 & noch!=0 | s07p08==2 & sex==1 & noch!=0)
	label var mother "Proxy mother, headh of hh or wife with CH in the household"
	
	
		*FATHER - only way to aprox is with hh relationship wife or head of hh female with CH
	ge mhh=(s07p08==1 & sex==0)
	label var mhh "Head of household is male"
	ge father=(mhh==1 & noch!=0 | s07p08==2 & sex==0 & noch!=0)
	label var father "Proxy father, headh of hh or wife with CH in the household"
	
	
	
	*There is a new child. Dummy variable if in t-1 did not have a son or daughter and in t now they have. Do not drop no WA population yet
	
		
	*Size of the HH
	rename s10ap01 HHsize
	
	*Dummy for receiving remittances
	recode s12p54_41a (2=0)
	label define yesno 0 "No" 1 "yes" 
	label values s12p54_41a yesno
	bys idhh: egen  remittances=max(s12p54_41a & s12p54_41a!=.)
	label var remittances "HH receives remittances"
		
	*Amount of remittances  -leave in the DB also value without log
	bys idhh: egen hhremit=sum(s12p54_41)
	ge lnremittances=log(hhremit)
	replace lnremittances=0 if lnremittances==.
	label var lnremittances "Log of the total amount of remittances at HH level"
	
	*At least one member of the HH receives pension
	recode s12p54_44a (2=0)
	label values s12p54_44a yesno
	bys idhh: egen  pension=max(s12p54_44a & s12p54_44a!=.)
	label var pension "At least one member of the HH receives pension"
		
	
	*Whish to change work. Activly looking people have more chances to find a job?
	recode s11p45 (2=0)
	label values s11p45 yesno
	label var s11p45 "Wish to change of job"
	
	*Reasons for which desire to change the job*/
	label var s11p46 "Reasons why want to change job"
	replace s11p46=. if s11p46==9  // recode ignored as missing
			
	*Have done any acction to change job, focus on network 
	recode s11p11_1 (2=0)
	label values s11p11_1 yesno
	label var s11p11_1 "Consulted with network"
	
	recode s11p11_2 (2=1)
	label values s11p11_2 yesno
	label var s11p11_2 "Visited working places"

	*Time looking a job - categorical var
	label var s11p09 "Time looking for a job"
	
	 *Time without a job 
	 label var s11p16 "Time without a job"
	  
	  *Never work before 
	  recode s11p15 (2=0)
	  label values s11p15 yesno
	  label var s11p15 "Never had a job before"
	  
	  *Firm size
	 	 
	  *recode firmsize 0 to 20, 21 to 50, 51 to 100, 100 or more
ge firmsize=s11p33
recode firmsize (2=1) (3=2) (4=2) (5=2) (6=3) (7=4)
replace firmsize=0 if firmsize==.
label define firmnopeople 0 "0 workers" 1 "1 to 5 workers" 2 "6 to 50 workers" 3 "51 to 100 workers" 4 "More than 100 workers"
label values firmsize firmnopeople
	  
	   label var firmsize "Firm size categorical"
	  replace firmsize=. if s11p33==9
	  
	  
	  *Agriculture self-employed
	  label var s12p57 "Dummy: grow or harvest on a farm or plot"
	  recode s12p57 (2=0)
	  label values s12p57 yesno
	  
	  label var s12p64 "Dummy: raise animals on a farm or plot"
	  recode s12p64 (2=0)
	  label values s12p64 yesno
	  
	  ge agriselfemployeed=(job_sts==3 & s12p57==1 | s12p64==1)  // agrictulture self-employed to drop for own-account analysis
	  label var agriselfemployeed "Individual self-employed in agrictulture"
	  
	  *Currently attending school
	  label var s07p16 "Currently attending school"
	  recode s07p16 (2=0)
	  label values s07p16 yesno
	  
	  *Receives social programes at school

	  foreach x of numlist 2 3 4 5 {
	  recode s12p55_5`x'p1 (2=0)
	  label values s12p55_5`x'p1 yesno
	  replace s12p55_5`x'p1=. if s12p55_5`x'p1==9
	  bys idhh: egen schoolprogram`x'=max(s12p55_5`x'p1)
	  tab s12p55_5`x'p1
	  }	
	  
	  
	  label var schoolprogram2 "School snack"
	  
	  label var schoolprogram3 "School lunch"
	  
	  label var schoolprogram4 "School supplies"
	  
	  label var schoolprogram5 "Other school support"
	  
	  
	  *Receives any of the school programs dummy
	  ge schoolprograms=(schoolprogram2==1 | schoolprogram3==1 | schoolprogram4==1 | schoolprogram5==1)
	  label var schoolprograms "Individual in which HH receives any of the school programs"
	  
	  
	  *WTP for each school benefit received 
	  foreach x of numlist 2 3 4 5 {
	  recode s12p55_5`x'p5 (2=0)
	  label values s12p55_5`x'p5 yesno
	  replace s12p55_5`x'p5=. if s12p55_5`x'p1==9
	  bys idhh: egen schoolprogramvalue`x'=max(s12p55_5`x'p5)
	  replace schoolprogramvalue`x'=0 if schoolprogramvalue`x'==.
	  }	
	  
	  label var schoolprogramvalue2 "Value WTP of school snack"
	  
	  label var schoolprogramvalue3 "Value WTP school lunch"
	  
	  label var schoolprogramvalue4 "Value WTP school supplies"
	  
	  label var schoolprogramvalue5 "Value WTP other school support"
	  
	  
	
	*married
	gen married=(marital_status==3)
	label values married yesno
	

	  
      *********************************************************
      ***          20. DROPING UNECESARY VARIABLES         *** 
      *********************************************************
						  

**GENERATIONG WORKING AGE DUMMY**
gen WA=(s07p10>= 14 & s07p10<= 65) if s07p10!=.
label variable WA "Population in the working age"

  
  
  
***VARIABLES NECESSARY FOR THE PANEL****

	
keep mes dominio trimestre id idupm s01p01 s01p02 s01p06 s01p09 s05p04 HHsize s10ap02 s10ap03 s07p00 s07p08 s07p09 s07p10 s07p11a s07p11b s07p11c s07p12 s07p14 s07p18 status_occ status_test realseek LF industry ecosect ss ICSE ise job_sts status_form younghw youngw2 ///
educl educ0 educ1 educ2 educ3 educ4 educ5 educ6 occatg taxpincome taxsincome pincsw sincsw pincomesw sincomesw afpincomsw afsincomesw spcpincsw spcsincsw spcpincomsws spcsincomsw ///
totalswprincome totalswsecincome totalpowpincom totalsowsincom totalnlaborinc totalincomeww totalincomeow THmean THmedian low_pay netprimactincom fajustexproyeccion s12p49_1p11 s12p49_1s11 s12p49_2p15 s12p49_2s15 s12p49_3p23 s12p49_3s23 s12p54_51 s12p63_82 s12p69_106 lnpw lnpw2 ///
sex socioeconomic_strata marital_status area_residence formcost oage older agegroup s11p39a s11p39b ///
idhh idindv idindvm idindvms idindvmse ///
ch noch fhh mother firmsize remittances hhremit lnremittances pension agriselfemployeed schoolprogram2 schoolprogram3 schoolprogram4 schoolprogram5 schoolprograms /// 
schoolprogramvalue2 schoolprogramvalue3 schoolprogramvalue4 schoolprogramvalue5 ///
 HHsize  s11p45 s11p46 s11p11_1 s11p11_2 s11p09 s11p16 s11p15 s11p33 s07p16 totalfwh job_sts2 married ///
invisible visible busc_mas s11p43 s11p45 horas horas_40 ocup_40_sm s11p39a s11p39b s11p39c vunderempl status_underempl father s11p31 totalincome ltotalincome totalhrs

