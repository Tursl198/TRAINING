setwd("~/Desktop/CNYCA/ACS2023_5years_pums/Data/csv_hny")

library(dplyr)
library(gdata)
library(gmodels)
library(haven)
library(plyr)
library(data.table)
library(openxlsx)
library(tidyr)
library (writexl)
library(survey)
library (srvyr)

#I open the ACS_5year_2023_NYS PUMS dataset downloaded on 02/23/2024 at the following link: https://www2.census.gov/programs-surveys/acs/data/pums/2023/5-Year/.
## This is the household level data for New York state. 

PUMS23_NYS <- read.csv("psam_h36.csv")

#I check the name of the variables
names(PUMS23_NYS)

##I check whether the variables that I am interested in are in the dataset
"ADJINC" %in% names(PUMS23_NYS)
"WGTP" %in% names(PUMS23_NYS)


##HINCP is the household income in the past 12 months. I need to adjust it for inflation to constant dollars. To do that I use the variable ADJIN

summary(PUMS23_NYS$HINCP)
table(PUMS23_NYS$ADJINC)


# THE CURRENT DATASET INCLUDES OBSERVATIONS FOR THE ENTIRE NEW YORK STARE. I AM ONLY INTERESTED IN THE CITY, SO I NEED TO FILTER FOR THE 5 COUNTIES
### BUT FIRST I DROP THE COLUMNS I DON'T NEED. 

PUMS23_NYS_CLEANED <- PUMS23_NYS %>% select(1:10, HINCP, WGTP)

# I proceed to filtr the column of PUMA for values corresponding to NYC, they should be 55. ( https://usa.ipums.org/usa-action/variables/CITY#comparability_section)
### With the 2023 5-year ACS/PRCS sample, the Census Bureau began reporting only 2020 PUMAs for all survey years, which they achieved by assigning pre-2022 respondents a 2020 PUMA based on their 2010 PUMAs of residence and the known relationships between 2010 and 2020 PUMAs.
## Based on this, I didn't have to adjust myself differences in PUMAs before and after 2022.
##The Bureau has provided no details on their PUMA allocation method, but IPUMS has confirmed that it produces plausible population totals for the assigned 2020 PUMAs.


table (PUMS23_NYS_CLEANED$PUMA)

NYC_PUMA_VALUES <- c(04103, 04104, 04107, 04108, 04109, 04110, 04111, 04112, 04121, 04165, 04204, 04205, 04207, 04208, 04209, 04210, 04211, 04212, 04221, 04263, 04301, 04302, 04303, 04304, 04305, 04306, 04307, 04308, 04309, 04310, 04311, 04312, 04313, 04314, 04315, 04316, 04317, 04318, 04401, 04402, 04403, 04404, 04405, 04406, 04407, 04408, 04409, 04410, 04411, 04412, 04413, 04414, 04501, 04502, 04503)


PUMS23_NYCITY <- PUMS23_NYS_CLEANED %>% filter(PUMA %in% NYC_PUMA_VALUES)


# I ADJUST THE INCOME BY INFLATION 
is.numeric(PUMS23_NYCITY$HINCP)
is.numeric(PUMS23_NYCITY$ADJINC)

## If i want to adjust all nominal values for household income to 2023 inflation asjustment factor, I need to select the value for ADJINC that corresponds to 2023, which is 1019518 and divide it by 1000000 and then multiply by the nominal wage variable HINCP

PUMS23_NYCITY <-  PUMS23_NYCITY %>% mutate(ADJ_HINCP = HINCP*(ADJINC/1000000))  

### now i can calculate the descriptive statistics for this variable after having created the survey design object with weight WGTP
##WGPT is the weight to be used at the household level. 

svy_design <- PUMS23_NYCITY %>%  as_survey_design(weights = WGTP)


#to caluclate the mean

mean_result <-svymean(~ADJ_HINCP, design = svy_design, na.rm = TRUE, ci = TRUE)

##MEAN WITHOUT SURVEY WEIGHT TO CHECK THE DIFFERENCE
mean(PUMS23_NYCITY$ADJ_HINCP, na.rm = TRUE)


#TO CALCULATE THE MEDIAN
median_result <- svyquantile(~ADJ_HINCP, design = svy_design, quantiles = 0.5, na.rm = TRUE, ci = TRUE)


#I store the results 

mean_median_df <- data.frame(
  Statistic = c("Mean", "Median"),
  Value = c(coef(mean_result), coef(median_result))
)


### to calculate percentiles 

percentiles <- seq(0.01, 0.99 by = 0.01)

quantile_values <- svyquantile(~ADJ_HINCP, design = svy_design, quantiles = percentiles, na.rm = TRUE)
quantile_values <- coef(quantile_values)

NYC_wage_quintiles <- tibble (percentile = percentiles, qADJ_HINCP = quantile_values)


# Calculate income brackets and store only the TRUE percentages
income_brackets <- data.frame(
  Income_Bracket = c(
    "Less than $10,000",
    "$10,000 to $14,999",
    "$15,000 to $24,999",
    "$25,000 to $34,999",
    "$35,000 to $49,999",
    "$50,000 to $74,999",
    "$75,000 to $99,999",
    "$100,000 to $149,999",
    "$150,000 to $199,999",
    "$200,000 or more"
  ),
  Percentage = c(
    coef(svymean(~I(ADJ_HINCP < 10000), svy_design, na.rm = TRUE))[2] * 100,  # Extract TRUE value
    coef(svymean(~I(ADJ_HINCP >= 10000 & ADJ_HINCP < 15000), svy_design, na.rm = TRUE))[2] * 100,
    coef(svymean(~I(ADJ_HINCP >= 15000 & ADJ_HINCP < 25000), svy_design, na.rm = TRUE))[2] * 100,
    coef(svymean(~I(ADJ_HINCP >= 25000 & ADJ_HINCP < 35000), svy_design, na.rm = TRUE))[2] * 100,
    coef(svymean(~I(ADJ_HINCP >= 35000 & ADJ_HINCP < 50000), svy_design, na.rm = TRUE))[2] * 100,
    coef(svymean(~I(ADJ_HINCP >= 50000 & ADJ_HINCP < 75000), svy_design, na.rm = TRUE))[2] * 100,
    coef(svymean(~I(ADJ_HINCP >= 75000 & ADJ_HINCP < 100000), svy_design, na.rm = TRUE))[2] * 100,
    coef(svymean(~I(ADJ_HINCP >= 100000 & ADJ_HINCP < 150000), svy_design, na.rm = TRUE))[2] * 100,
    coef(svymean(~I(ADJ_HINCP >= 150000 & ADJ_HINCP < 200000), svy_design, na.rm = TRUE))[2] * 100,
    coef(svymean(~I(ADJ_HINCP >= 200000), svy_design, na.rm = TRUE))[2] * 100
  )
)

# Round the percentages to 2 decimal places for better readability
income_brackets$Percentage <- round(income_brackets$Percentage, 2)




## Calculating quantiles, deciles and Palma index.


income_quintiles <- svy_design %>%
  mutate(quintile = ntile(ADJ_HINCP, 5)) %>%  # Divide into 5 quintiles
  group_by(quintile) %>%
  summarize(total_income = survey_total(ADJ_HINCP, na.rm = TRUE)) %>%
  mutate(share = total_income / sum(total_income))



income_deciles <- svy_design %>%
  mutate(decile = ntile(ADJ_HINCP, 10)) %>%  # Create deciles
  group_by(decile) %>%
  summarize(total_income = survey_total(ADJ_HINCP, na.rm = TRUE))

top_10_income <- income_deciles %>%
  filter(decile == 10) %>%
  pull(total_income)

bottom_40_income <- income_deciles %>%
  filter(decile %in% c(1, 2, 3, 4)) %>%
  summarise(total_income = sum(total_income)) %>%
  pull(total_income)

palma_index <- top_10_income / bottom_40_income
print(palma_index)



#Store the results in Excel


write_xlsx(list(
  "Mean & Median" = mean_median_df,
  "Percentiles" = NYC_wage_quintiles,
  "Income Distribution" = income_brackets,
  "Income Quintiles" = income_quintiles,  
  "Income Deciles" = income_deciles,
  "Palma Index" = data.frame(Palma_Index = palma_index)  # Store Palma Index in a dataframe
), path = "~/Desktop/CNYCA/ACS2023_5years_pums/Data/NYC_2019_2023_household_income_analysis.xlsx")
