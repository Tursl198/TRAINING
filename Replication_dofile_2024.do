cd ~/Desktop/CNYCA/Replication_Mohamed_Wage23_24/Replication

log using replication.2025.02.02.txt, text replace


use "/Users/ludovicatursini/Desktop/CNYCA/Replication_Mohamed_Wage23_24/epi_cpsorg_1979_2024/epi_cpsorg_2024.dta"


**First I dive the annual weight by months. Since the CPS is a monthly survey, but orgwgt is calibrated for an annual population, dividing by 12 ensures that estimates for a single month are correctly scaled.
** I CALCULATE PERCENTILES ON WEIGhted Wage for US as a whole
gen monthly_orgwgt = orgwgt/12


svyset [pw=monthly_orgwgt]

_pctile wage [aw=monthly_orgwgt], nquantiles(100)

clear
set obs 100
gen percentile = _n  // Creates a variable for percentiles (1 to 100)
gen wage_pctile = .

forvalues i = 1/100 {
    replace wage_pctile = r(r`i') if percentile == `i'
}

export excel using "wage_percentiles_national_2024.xlsx", firstrow(variables) replace



*** FIPS code are univoqe codes associated with states and counties. We have both variables in the dataset. New York state is 36 and counties of New York City are: Bronx, Kings (Brooklyn), Queens, New York (Manhattan) and Richmond (Staten Island). Those have the codes 5, 47, 81, 61 and 85. I need to filter for those. 
clear
use "/Users/ludovicatursini/Desktop/CNYCA/Replication_Mohamed_Wage23_24/epi_cpsorg_1979_2024/epi_cpsorg_2024.dta"

gen monthly_orgwgt = orgwgt/12
keep if statefips == 36
keep if inlist(countyfips, 5, 47, 61, 81, 85)

count if missing(wage)
sum wage

** I am only keeping NYC at this moment. So I am analyzing NYC
tab wage
sum wage

distinct month

*** Setting the survey design

svyset [pw=monthly_orgwgt]

_pctile wage [aw=monthly_orgwgt], nquantiles(100)

clear
set obs 100
gen percentile = _n  // Creates a variable for percentiles (1 to 100)
gen wage_pctile = .

forvalues i = 1/100 {
    replace wage_pctile = r(r`i') if percentile == `i'
}

export excel using "wage_percentiles_city_2024.xlsx", firstrow(variables) replace


clear
log close 
