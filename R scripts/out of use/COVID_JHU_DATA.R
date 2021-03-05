## COVID_JHU_DATA.R ##
# Author: Kees Schipper
# Date Created: 8/28/2020
# Last Update : 9/17/2020
# Notes: The purpose of this script is solely to retrieve and clean the JHU COVID data so that all
#        Location names, Lat, Long, etc are standardized. THIS SCRIPT IS ONLY FOR CLEANING
# This code will be made with the guidance of code from the following github repository:
# https://github.com/eparker12/nCoV_tracker, pulled from the R shiny gallery

if (!require(devtools)) install.packages("devtools"); library(devtools)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(stringr)) install.packages("stringr"); library(stringr)
if (!require(readxl)) install.packages("readxl"); library(readxl)
if (!require(data.table)) install.packages("data.table"); library(data.table)
if (!require(kza)) install.packages("kza"); library(kza)
if (!require(lubridate)) install.packages("lubridate"); library(lubridate)
if (!require(MASS)) install.packages("MASS"); library(MASS)
if (!require(broom)) install.packages("broom"); library(broom)
if (!require(forecast)) install.packages("forecast"); library(forecast)
if (!require(conflicted)) install.packages("conflicted"); library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("week", "lubridate")

# Get URLs and read data from github for updated JHU data on global and US cases and deaths
url.confirmed.global = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
Sys.sleep(2)
url.confirmed.US = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
Sys.sleep(2)
url.death.global = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
Sys.sleep(2)
url.death.US = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
Sys.sleep(2)
# US Deaths
COV_D_US <- read_csv(url(url.death.US))
Sys.sleep(2)
# Global Deaths
COV_D_G <- read_csv(url(url.death.global))
Sys.sleep(2)
# US Cases
COV_C_US <- read_csv(url(url.confirmed.US))
Sys.sleep(2)
# Global Cases
COV_C_G <- read_csv(url(url.confirmed.global))


# Save Raw Covid death and case data --------------------------------------
wd <- "C:/Users/keess/Box/Tufts Friedman School/Semester 3/Fa20-NUTR-0394-01-Advanced-Data-Analysis-2020-Sep-08_15-31-57/JHU Raw Data"
COVID_deaths_path_w <- paste(wd, "/JHU_COVID_Deaths_US_wide_", Sys.Date(), ".RData", sep = "")
COVID_cases_path_w <- paste(wd, "/JHU_COVID_Cases_US_wide_", Sys.Date(), ".RData", sep = "")

save(COV_D_US, file = COVID_deaths_path_w)
save(COV_C_US, file = COVID_cases_path_w)



# Create function for cleaning global data --------------------------------


# Clean COV_D_G names, change Hong Kong, Taiwan, and other "Provinces" into countries

clean_jhu_global <- function(input_df, tag){
names(input_df)[1:2] <- c("Province", "Country")
input_df$Country[input_df$Province == "Hong Kong"] <- "Hong Kong"
input_df$Country[input_df$Province == "Macau"] <- "Macau"
input_df$Country[input_df$Country == "Taiwan*"] <- "Taiwan"
input_df$Country[input_df$Country=="Congo (Brazzaville)" | input_df$Country=="Republic of the Congo"] <- "Congo"
input_df$Country[input_df$Country=="Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
input_df$Country[input_df$Country == "Cote d'Ivoire"] <- "Ivory Coast"
input_df$Country[input_df$Country == "Guinea-Bissau"] <- "GuineaBissau"
input_df$Country[input_df$Country == "Korea, South"] <- "South Korea"
ifelse(input_df[[tag]] < 0, 0, input_df[[tag]])
# 
# # Transition data to long format
input_df <- input_df %>%
  pivot_longer(cols = c(`1/22/20`:ncol(input_df)),
                        values_to = tag,
                        names_to = "Date") %>%
  mutate(Date = as.Date(Date, "%m/%d/%y")) %>%
  select(-c(Lat, Long, Province))
}

# test function
test_df <- clean_jhu_global(COV_C_G, "cases")

# Going to consider Western Sahara a part of Morocco (Western Sahara only has around 10 
# reported cases so it doesn't make much of a difference anyway)
# Will help because WB combines Morocco and Western Sahara population data
Cases_Global <- clean_jhu_global(COV_C_G, "cases") %>%
  mutate(Country = ifelse(Country == "Western Sahara", "Morocco", Country))
Deaths_Global <- clean_jhu_global(COV_D_G, "deaths") %>%
  mutate(Country = ifelse(Country == "Western Sahara", "Morocco", Country))


# Create function to calculate total cases/deaths per country and county -------------

get_area_totals <- function(.input_df, .tag, ...){
  .tag <- enquo(.tag)

  .input_df <- .input_df %>%
    group_by(...) %>%
    summarize(!!.tag := sum(!!.tag))
}

# test <- get_area_totals(test_df, cases, Country, Date)

# Create Function for cleaning state data ---------------------------------
# Examine if there are any errors or simplifications you can make in the county names
clean_jhu_state <- function(input_df, tag){
  input_df <- input_df %>%
    select(-c(iso2, iso3, code3, Lat, Long_, Combined_Key, Country_Region, UID)) %>%
    rename(State = Province_State,
           County = Admin2)
  input_df <- input_df %>%
    pivot_longer(cols = c(`1/22/20`:last_col()),
                 names_to = "Date",
                 values_to = tag) %>%
    mutate(Date = as.Date(Date, "%m/%d/%y"))
}

state_cases <- clean_jhu_state(COV_C_US, "cases") %>%
  mutate(FIPS = as.numeric(FIPS))
state_deaths <- clean_jhu_state(COV_D_US, "deaths")


# Merge State Cases and deaths into one dataset ---------------------------

COVID_states <- full_join(state_cases, state_deaths, by = c("FIPS", "Date", "County", "State")) %>%
  select(FIPS, County, State, Population, Date, cases, deaths) %>%
  arrange(State, County)

# calculate the change in cases and deaths by day
# standardize cases, deaths, change in cases, and change in deaths by 100,000 population
# remove county areas that contain "Out of," NA, Dukes and Nantucket, Federal Correctional,
# Michigan Department of Corrections, Central Utah, Southeast Utah, TriCounty, Southwest Utah, 
# Weber-Morgan

COVID <- COVID_states %>%
  filter(Population != 0) %>%
  mutate(dcases = ave(cases, FIPS, FUN = function(x) c(0, diff(x))),
         ddeaths = ave(deaths, FIPS, FUN = function(x) c(0, diff(x))))

COVID_states <- COVID %>%
  mutate(dcases = ifelse(dcases < 0, NA, dcases),
         ddeaths = ifelse(ddeaths < 0, NA, ddeaths)) %>%
  group_by(FIPS) %>%
  mutate(dcases = na.interp(dcases),
         ddeaths = na.interp(ddeaths)) %>%
  ungroup() %>%
  mutate(cper100k = (cases/Population)*100000,
         cper100k = ifelse(is.nan(cper100k), NA, cper100k),
         dper100k = (deaths/Population)*100000,
         dper100k = ifelse(is.nan(dper100k), NA, dper100k),
         dcper100k = (dcases/Population)*100000,
         dcper100k = ifelse(is.nan(dcper100k), NA, dcper100k),
         ddper100k = (ddeaths/Population)*100000,
         ddper100k = ifelse(is.nan(ddper100k), NA, ddper100k),
         weekday = weekdays(Date),
         Monday = ifelse(weekday == "Monday", 1, 0),
         Tuesday = ifelse(weekday == "Tuesday", 1, 0),
         Wednesday = ifelse(weekday == "Wednesday", 1, 0),
         Thursday = ifelse(weekday == "Thursday", 1, 0),
         Friday = ifelse(weekday == "Friday", 1, 0),
         Saturday = ifelse(weekday == "Saturday", 1, 0),
         Sunday = ifelse(weekday == "Sunday", 1, 0),
         weekfac = factor(weekday))

County_population <- data.frame(
  County = COVID_states$County,
  State = COVID_states$State,
  FIPS = COVID_states$FIPS,
  population = COVID_states$Population
)

save(County_population, file = 'cleaned data/county_population.RData')

COVID_deaths_path_l <- paste(wd, "/JHU_COVID_US_states_", Sys.Date(), ".RData", sep = "")

save(COVID_states, file = COVID_deaths_path_l)

# Merge Global cases and deaths into one dataset --------------------------

# use previously made functions to conglomerate cases and deaths by country
CasesbyCountry <- get_area_totals(Cases_Global, .tag = cases, Country, Date)
DeathsbyCountry <- get_area_totals(Deaths_Global, .tag = deaths, Country, Date)

# join global datasets, calculate change in deaths and cases by day
COVID_global <- full_join(CasesbyCountry, DeathsbyCountry, by = c("Country", "Date")) %>%
  mutate(dcases = ave(cases, Country, FUN = function(x) c(0, diff(x))),
         ddeaths = ave(deaths, Country, FUN = function(x) c(0, diff(x))))

save(COVID_global, file = 'cleaned data/COVID_countries_data_jhu.RData')

# Get data with population information for every country ------------------

# Using the world bank's population data at the country level
# popsearch <- wb_search("population, total")[27, ]
# population_data <- wb_data(indicator = popsearch$indicator_id, country = "countries_only")
# # # TODO: come back to look at different regional populations, as there are 
# # # a lot of interesting WB options.
# # 
# pop_2019 <- population_data %>%
#   select(-c(iso2c, iso3c, unit, obs_status, footnote, last_updated)) %>%
#   filter(date == 2019) %>%
#   rename(Country = country,
#          Population = SP.POP.TOTL) %>%
#   select(-date)
# 
# #find country values in population and covid data that aren't matched
# outersect <- function(x, y) {
#   sort(c(setdiff(x, y),
#          setdiff(y, x)))
# }
# 
# nomatch <- outersect(pop_2019$Country, unique(COVID_global$Country))
# 
# 
# oldnames <- c("Bahamas, The", "Brunei Darussalam", "Cote d'Ivoire", "Congo, Dem. Rep.",
#               "Congo, Rep.", "Egypt, Arab Rep.", "Gambia, The", "Guinea-Bissau",
#               "Hong Kong SAR, China", "Iran, Islamic Rep.", "Korea, Dem. People's Rep.",
#               "Korea, Rep.", "Kyrgyz Republic", "Lao PDR", "Macao SAR, China",
#               "Russian Federation", "Slovak Republic", "St. Vincent and the Grenadines",
#               "St. Lucia", "St. Kitts and Nevis", "Syrian Arab Republic", "United States",
#               "Venezuela, RB", "Yemen, Rep.", "Western Sahara", "Macao", "Myanmar", "Czech Republic")
# newnames <- c("Bahamas", "Brunei", "Ivory Coast", "Democratic Republic of the Congo",
#               "Congo", "Egypt", "Gambia", "GuineaBissau", "Hong Kong", "Iran",
#               "Korea, Rep.", "South Korea", "Kyrgyzstan", "Laos", "Macao", "Russia",
#               "Slovakia", "Saint Vincent and the Grenadines", "Saint Lucia",
#               "Saint Kitts and Nevis", "Syria", "US", "Venezuela", "Yemen", "Morocco",
#               "Macau", "Burma", "Czechia")
# 
# for (i in seq_along(oldnames)){
#   cat("iteration ", i, " of ", length(oldnames), "\n")
#   pop_2019$Country[pop_2019$Country == oldnames[i]] <- newnames[i]
# }
# # now there are only 45 names in total that do not match.
# nomatch2 <- outersect(pop_2019$Country, unique(COVID_global$Country))
# 
# 
# # Combine population data with COVID data ---------------------------------
# # Calculate standardized death and case rates
# COVID_global <- left_join(COVID_global, pop_2019, by = "Country")
# 
# 
# # WB doesn't have population data on Taiwan or Eritrea. POpulation data is from the following website:
# # https://www.worldometers.info/world-population/taiwan-population/
# # data was taken from the website on 8/29/2020, and may update frequently
# # https://en.wikipedia.org/wiki/Diamond_Princess_(ship)
# # https://en.wikipedia.org/wiki/MS_Zaandam
# COVID_global <- COVID_global %>%
#   filter(Country != "Diamond Princess" | Country != "Holy See" | Country != "MS Zaandam") %>%
#   mutate(cper100k = (cases/Population)*100000,
#          dper100k = (deaths/Population)*100000,
#          dcper100k = (dcases/Population)*100000,
#          ddper100k = (ddeaths/Population)*100000) %>%
#   mutate(Population = ifelse(Country == "Taiwan", 23816775,
#                              ifelse(Country == "Eritrea", 3554644,
#                                     ifelse(Country == "Diamond Princess", 3711,
#                                            ifelse(Country == "MS Zaandam", 1829,
#                                                   ifelse(Country == "Holy See", 801, Population))))),
#          weekday = weekdays(Date),
#          Monday = ifelse(weekday == "Monday", 1, 0),
#          Tuesday = ifelse(weekday == "Tuesday", 1, 0),
#          Wednesday = ifelse(weekday == "Wednesday", 1, 0),
#          Thursday = ifelse(weekday == "Thursday", 1, 0),
#          Friday = ifelse(weekday == "Friday", 1, 0),
#          Saturday = ifelse(weekday == "Saturday", 1, 0),
#          Sunday = ifelse(weekday == "Sunday", 1, 0))
# 
# # which countries have missing population data?
# # miss_pop <- COVID_global[which(is.na(COVID_global$Population)), ]
# 
# # miss_pop_countries <- unique(miss_pop$Country)
#   
# # Keep population data as a separate save file
# # Note: Do not have population for cruise ships, Eritrea, or Holy See
# save(pop_2019, file = "cleaned data/country_population.RData")
# 
# 
rm(list= ls()[! (ls() %in% c('COVID_states', 'COVID_global'))])

# Incorporate google satellite mobility -----------------------------------
# Maybe some potential for continuously updated mobility reports, but this source seems sketchy
# url.google.mobil <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_US.csv"
google.mobil.zip <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"

# COVID_mobil_test <- read_csv(url(url.google.mobil))
temp <- tempfile()
try(download.file(url = google.mobil.zip, 
                  temp,
                  mode = "wb"))
US_mobility <- read_csv(unz(temp, "2020_US_Region_Mobility_Report.csv"))
file.remove(temp)

# renaming in case we want to retain these variables in the future
COVID_mobility <- US_mobility %>%
  rename(country_abb = country_region_code,
         country = country_region,
         State = sub_region_1,
         County =  sub_region_2,
         FIPS = census_fips_code,
         Date = date) %>%
  select(FIPS, Date, c(9:14)) %>%
  mutate(FIPS = as.numeric(FIPS),
         Date = as.Date(Date))
  
  
  
names(COVID_mobility)[3:8] <- c("retail_rec", "groc_pharm", "parks", "transit", "workplace", "residential")

COVID_stmobil <- left_join(COVID_states, COVID_mobility, by = c("FIPS", "Date")) %>%
  filter(!is.na(FIPS)) %>%
  group_by(FIPS) %>%
  mutate(time = 1:length(unique(Date)),
         time = as.numeric(time),
         weekend = ifelse(weekday == "Saturday" | weekday == "Sunday", 1, 0)) %>%
  ungroup() %>%
  mutate(FIPS = as.character(FIPS),
         weekfac = ordered(weekday, levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday",
                                               "Thursday", "Friday"))) %>%
  filter(Population != 0)



save(COVID_stmobil, file = paste0("cleaned data/COVID_state_mobility_", Sys.Date(), ".RData"))


COVID_stmobil <- COVID_stmobil %>%
  mutate(logcases = log(((dcases + 1)/Population)*100000),
         logdeaths = log(((ddeaths + 1)/Population)*100000),
         weekfac = factor(weekfac, ordered = F),
         weekfac = relevel(weekfac, ref = "Sunday"),
         weeknd = ifelse(weekfac == "Saturday" | weekfac == "Sunday", "weekend", "weekday"),
         weeknd = factor(weeknd, levels = c("weekend", "weekday"))) %>%
  select(-dcases, -ddeaths, -cper100k, -dper100k)



# look at missing data in MDSX --------------------------------------------

Mddsx <- COVID_states %>%
  filter(State == "Massachusetts" & County == "Middlesex") %>%
  filter(Date > "2020-03-01") %>%
  mutate(
    dcases = ave(cases, FIPS, FUN = function(x) c(0, diff(x))),
    ddeaths = ave(deaths, FIPS, FUN = function(x) c(0, diff(x)))
  )

plot(Mddsx$Date, Mddsx$dcases, type = "l", col = "blue")


plot(Mddsx$Date, Mddsx$ddeaths, type = "l", col = "red")

case_correction <- Mddsx[which(Mddsx$dcases < 0),]
death_correction <- Mddsx[which(Mddsx$ddeaths < 0),]
