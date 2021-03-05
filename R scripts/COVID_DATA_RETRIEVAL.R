## MDSX_COVID_ANALYSIS_CLEAN.R ##
## Author: Kees Schipper ##
## Created: 2021-02-18 ##
## Last Updated: 2021-02-18 ##
## Notes: This code is a combination of COVID_JHU_DATA and MDSX_COVID_ANALYSIS
## in an attempt to try to reduce the size of the bloated codes that I've made
## which contain a lot of unneccessary processes and waste a lot of time. This code
## (1) read and clean data from the MA github repository
## (2) read and clean data from the google mobility reports repository

rm(list = ls())
dev.off()

library(tidyverse)

source('R scripts/COVID_Analysis_Utilities.R')


# read data from github repositories --------------------------------------
# case and death data
US_cases_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
Sys.sleep(2)
US_deaths_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
Sys.sleep(2)

US_cases <- read_csv(US_cases_url)
US_deaths <- read_csv(US_deaths_url)

# mobility data
google.mobil.zip <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"

# COVID_mobil_test <- read_csv(url(url.google.mobil))
temp <- tempfile()
try(download.file(url = google.mobil.zip, 
                  temp,
                  mode = "wb"))
US_mobility <- read_csv(unz(temp, "2020_US_Region_Mobility_Report.csv"))
file.remove(temp)


# cleaning US cases, deaths, and mobility for merge -----------------------
# keep variables for county, state, date, cases, deaths, mobility metrics, fips,
# all in lower-case for consistency

# functions can be found in Utilities file
US_cases_long <- CleanCasesDeaths(US_cases, 'cases')
US_deaths_long <- CleanCasesDeaths(US_deaths, 'deaths')

# cleaning mobility data
mobility_c <- US_mobility %>%
  select(sub_region_1, sub_region_2, census_fips_code, date:last_col()) %>%
  rename(retail_rec = `retail_and_recreation_percent_change_from_baseline`,
         groc_pharm = `grocery_and_pharmacy_percent_change_from_baseline`,
         parks = `parks_percent_change_from_baseline`,
         transit = `transit_stations_percent_change_from_baseline`,
         work = `workplaces_percent_change_from_baseline`,
         residential = `residential_percent_change_from_baseline`,
         state = sub_region_1,
         county = sub_region_2,
         fips = census_fips_code) %>%
  filter(!is.na(state) & !is.na(county) & !is.na(fips)) %>%
  mutate(county = trimws(str_remove_all(county, " County")),
         fips = as.numeric(fips))
  
# merge the three datasets together: cases, then deaths, then mobility

COVID <- US_cases_long %>%
  left_join(US_deaths_long, by = c('fips', 'date', 'county', 'state')) %>%
  left_join(mobility_c, by = c('fips', 'date', 'county', 'state'))

# Make population-standardized case and death rates -----------------------

# start by getting the daily rate of COVID-19 cases and deaths
# then standardize daily rate of COVID-19 cases and deaths per 100,000 people 
# in each county
# create a variable for the day of the week based on `date`
COVID_master <- COVID %>%
  mutate(daily_cases = ave(cases, fips, FUN = function(x) c(0, diff(x))),
         daily_deaths = ave(deaths, fips, FUN = function(x) c(0, diff(x))),
         daily_cases_100k = (daily_cases/Population)*100000,
         daily_deaths_100k = (daily_deaths/Population)*100000,
         weekday = weekdays(date),
         weekday = factor(weekday))

# write Rdata and csv file to save COVID dataset up to 2021-02-18 -------------------

write_csv(COVID_master, 'cleaned data/COVID_master_20200218.csv')
save(COVID_master, file = paste0('cleaned data/COVID_master_', Sys.Date(), '.RData'))



