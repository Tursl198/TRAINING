setwd("~/Desktop/CNYCA/ACS2023_5years_pums/Data/2019_5_years_PUMS/csv_hny_directory")

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

### Dataset donwloaded deom: https://www2.census.gov/programs-surveys/acs/data/pums/2019/5-Year/ on March 21st, 2025. Household data for NY state, ACS 2019 5 years.


PUMS19_5years_NYS <- read.csv("psam_h36.csv")

"ADJINC" %in% names(PUMS19_5years_NYS)
"WGTP" %in% names(PUMS19_5years_NYS)
"PUMA" %in% names(PUMS19_5years_NYS)

PUMS19_5years_NYC <- PUMS19_5years_NYS %>% filter(PUMA >= 3701 & PUMA <= 4114)

summary(PUMS19_5years_NYC$HINCP, na.rm = TRUE)
table(PUMS19_5years_NYC$ADJINC)

PUMS19_5years_NYC <- PUMS19_5years_NYC %>% select(1:9, HINCP)

PUMS19_5years_NYC <-  PUMS19_5years_NYC %>% mutate(ADJ_HINCP = HINCP*(ADJINC/1000000)) 

summary(PUMS19_5years_NYC$ADJ_HINCP, na.rm = TRUE)

#I adjust for the sample weight 
svy_design <- PUMS19_5years_NYC %>%  as_survey_design(weights = WGTP)

#Calculate the weighted adjusted household income 
mean_result <- svymean(~ADJ_HINCP, design = svy_design, na.rm = TRUE, ci = TRUE)


#TO CALCULATE THE weighted adjusted household MEDIAN income
median_result <- svyquantile(~ADJ_HINCP, design = svy_design, quantiles = 0.5, na.rm = TRUE, ci = TRUE)


#I store the results 

mean_median_df <- data.frame(
  Statistic = c("Mean", "Median"),
  Value = c(coef(mean_result), coef(median_result))
)

### to calculate percentiles 

percentiles <- seq(0.01, 1.00, by = 0.01)

quantile_values <- svyquantile(~ADJ_HINCP, design = svy_design, quantiles = percentiles, na.rm = TRUE)
quantile_values <- coef(quantile_values)

NYC_wage_quintiles2019 <- tibble (percentile = percentiles, qADJ_HINCP = quantile_values)


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


#Store the results in Excel

write_xlsx(list(
  "Mean & Median" = mean_median_df,
  "Percentiles" = NYC_wage_quintiles2019,
  "Income Distribution" = income_brackets
), path = "~/Desktop/CNYCA/ACS2023_5years_pums/Data/NYC_Household_Income_2019_5yearsACS.xlsx")


