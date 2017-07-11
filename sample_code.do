***************************************************************************
* Stata Workshop
* University of Pennsylvania
* 11 June 2017
* https://github.com/korykantenga/Stata_R_Workshop
***************************************************************************

**************************************************
* Housekeeping
**************************************************

* Clear Workspace
clear all
set more off

* Set Directory (change to your folder)
cd "C:\Users\krb-user45\Desktop\Temp"

*************************
* Setup
*************************
* Note: Copy Data from the website and store in folder. You will also need to 
* copy the codebook to understand what each variable is!

* Import Data and Save as Stata .dta
import delimited using "mainData.csv", clear
save myData.dta, replace

*************************
* Load Data into Stata
*************************
* Note: "import" already loaded the data. This is just to show 
* how to load a stata .dta file.

use myData.dta, clear

****************************
* Initial Variable Summary
****************************

* Show how much memory and which variables are present
desc

* Summarize all variables to look for anomalies
su

* Note: sex, race, first name, last name are missing so browse to see they are not numerical
* br

* Note: Usually data cleaning takes place at this stage. 
* Some common questions to answer:
* 1. What do you do with missings? Leave them? Delete the observation? Impute them?
* 2. If you impute, how do you impute? Replace with mean for similar people?

*****************************************************
* Data Analysis
*****************************************************

* Tabulate the number of responses where positive, negative, or no info was given
tab info_nil
tab info_pos info_neg

* Tabulate the number of responses by sound of name
bysort race: tab  pos_resp

* Since we are working with {0,1} variables, the mean is the % of positive responses
bysort race: su pos_resp

* Store the means in "local" variable
* Note: enter "help su" and you will see the list of things that are stored

quietly su pos_resp if race=="B" // quietly supresses the output
local meanB = `r(mean)' // su stores the mean in `r(mean)'

quietly su pos_resp if race=="W"
local meanW = `r(mean)'

* Calculate the difference
display `meanB'-`meanW' // display does calculations

*** The average response rate for black sounding names is 8% lower.

*** Let's restrict to observations where no information was given
quietly su pos_resp if race=="B" & info_nil==1 
local meanB = `r(mean)' 
quietly su pos_resp if race=="W" & info_nil==1 
local meanW = `r(mean)'
display `meanB'-`meanW' 

*** With no information given, the average response rates for black sounding names
* is 9% lower.


*****************************************************
* Regression Analysis
*****************************************************
*** We can confirm this using a regression!
* Note: The regression with a {0,1} gives the average difference in the outcome 
* between the 0 group and the 1 group
reg pos_resp black if info_nil==1

*** What is the average effect of positive and negative information?
reg pos_resp info_pos info_neg

*** Positive Info increases the response rate by 4% on average and negative info
*** decreases the response rate by 32%!

*** What about giving a black sounding name and different kinds of information?
reg pos_resp black info_pos info_neg blackxinfo_pos blackxinfo_neg

*** The partial effect of a black sounding name and giving negative information versus
*** not having a black sounding name and giving negative information is lower! (-33.7% + 4.5% = -32%)
*** The information is weighted more heavily for those without black sounding names
*** which may be evidence for statistical discrimination in the housing market.

