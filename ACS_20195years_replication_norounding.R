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
##NYC_PUMA_VALUES <- c(04103, 04104, 04107, 04108, 04109, 04110, 04111, 04112, 04121, 04165, 04204, 04205, 04207, 04208, 04209, 04210, 04211, 04212, 04221, 04263, 04301, 04302, 04303, 04304, 04305, 04306, 04307, 04308, 04309, 04310, 04311, 04312, 04313, 04314, 04315, 04316, 04317, 04318, 04401, 04402, 04403, 04404, 04405, 04406, 04407, 04408, 04409, 04410, 04411, 04412, 04413, 04414, 04501, 04502, 04503)


###PUMS19_5years_NYC <- PUMS19_5years_NYS %>% filter(PUMA %in% NYC_PUMA_VALUES)


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

percentiles <- seq(0.01, 0.99, by = 0.01)

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
  "Percentiles" = NYC_wage_quintiles2019,
  "Income Distribution" = income_brackets,
  "Income Quintiles" = income_quintiles,  
  "Income Deciles" = income_deciles,
  "Palma Index" = data.frame(Palma_Index = palma_index)  # Store Palma Index in a dataframe
), path = "~/Desktop/CNYCA/ACS2023_5years_pums/Data/NYC_2015_2019_household_income_analysis.xlsx")


