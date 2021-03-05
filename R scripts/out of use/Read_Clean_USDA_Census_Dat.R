## Read_Clean_USDA_Census_Dat.R ##
## Programmer: Kees Schipper ##
## Class: Advanced Data Analysis ##
## Created: 9/17/2020 ##
## Last updated: 9/17/2020 ##

if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if (!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if (!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(wbstats)) install.packages("wbstats", repos = "http://cran.us.r-project.org")

## Read in xlsx data for datasets 'Poverty-Rates-by-County-1960-2010.xlsm' and
## 'Food access research data atlas USDA ERS 2015.xlsx'
## 'FoodEnvironmentAtlas/StateAndCountyData.csv'

FoodAccess <- read.csv('FoodEnvironmentAtlas/StateAndCountyData.csv') %>%
  rename(FIPS = ï..FIPS) %>%
  pivot_wider(id_cols = c('FIPS', 'State', 'County'),
              names_from = 'Variable_Code',
              values_from = 'Value')
  

Poverty <- read_excel('Raw Data/Poverty-Rates-by-County-1960-2010.xlsm', sheet = 2)
colnames(Poverty) <- Poverty[1,]
Poverty <- Poverty[2:nrow(Poverty),] %>%
  mutate(FIPS = as.numeric(FIPS)) %>%
  select(FIPS, State, County, `Poverty Rate 2010`, `Population 2010`, `Persons in Poverty 2010`) %>%
  rename(povrate2010 = `Poverty Rate 2010`,
         pop2010 = `Population 2010`,
         persinpov2010 = `Persons in Poverty 2010`) %>%
  mutate(povrate2010 = as.numeric(povrate2010),
         pop2010 = as.numeric(pop2010),
         persinpov2010 = as.numeric(persinpov2010))

# Crete dataset with only state-level poverty data
Pov_State <- Poverty %>%
  filter(FIPS <= 56)

# Create dataset with only county-level poverty data
Pov_County <- Poverty %>%
  filter(FIPS > 1000)


# Examine data in poverty dataset for errors and missing data -------------
# Check for complete observations
length(complete.cases(Pov_County))
length(complete.cases(Pov_State))
# all counties and states have complete data


# Examine variables in FoodAccess dataset for variables to keep -----------






