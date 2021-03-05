## covid_JHU_data_scrape.R ##
# Author: Kees Schipper
# Date Created: 9/29/2020
# Last Update : 9/29/2020
# Notes: The purpose of this script is solely to retrieve and clean the JHU COVID data so that all
#        Location names, Lat, Long, etc are standardized. THIS SCRIPT IS ONLY FOR CLEANING
#        This code will be made with the guidance of code from the following github repository:
#        https://github.com/eparker12/nCoV_tracker, pulled from the R shiny gallery
# IMPORTANT: In COVID_global and COVID_states output, there are certain dates where case and death counts
#            have been corrected with negative values. These case and death counts are represented in 
#            the dcases and ddeaths variables as NA values. If the user would like to analyze the size of
#            These corrections, comment out the last line of this code, and use the COV_C_US, COV_D_US,
#            COV_C_G, and COV_D_G data sets, which are the raw, wide-format dataframes for cases and deaths
#            in the US and globally.
# Note on Data sources: COVID US cases and deaths are from JHU github repository linked in the code below. 
#                       COVID Global cases and deaths are from JHU github repository linked below as well
#                       Global population data was derived from world bank data.

if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(stringr)) install.packages("stringr"); library(stringr)
if (!require(readxl)) install.packages("readxl"); library(readxl)
if (!require(data.table)) install.packages("data.table"); library(data.table)
if (!require(wbstats)) install.packages("wbstats"); library(wbstats)
if (!require(kza)) install.packages("kza"); library(kza)
if (!require(lubridate)) install.packages("lubridate"); library(lubridate)
if (!require(MASS)) install.packages("MASS"); library(MASS)
if (!require(broom)) install.packages("broom"); library(broom)
if (!require(boxr)) install.packages("boxr"); library(boxr)
if (!require(conflicted)) install.packages("conflicted"); library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# Scrape data from JHU github repository ----------------------------------
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
# wd <- "[insert save directory here]"
# 
# # create path for COVID cases and deaths filename
# COVID_deaths_path_w <- paste(wd, "/[insert deaths filename here]", Sys.Date(), ".RData", sep = "")
# COVID_cases_path_w <- paste(wd, "/[insert cases filename here]", Sys.Date(), ".RData", sep = "")
# 
# save(COV_D_US, file = COVID_deaths_path_w)
# save(COV_C_US, file = COVID_cases_path_w)



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


# Create Function for cleaning state data ---------------------------------
# Examine if there are any errors or simplifications you can make in the county names
clean_jhu_state <- function(input_df, tag){
  input_df <- input_df %>%
    select(-c(iso2, iso3, code3, Lat, Long_, Combined_Key, Country_Region, UID)) %>%
    rename(State = Province_State,
           County = Admin2)
  input_df <- input_df %>%
    pivot_longer(cols = c(`1/22/20`:ncol(input_df)),
                 names_to = "Date",
                 values_to = tag) %>%
    mutate(Date = as.Date(Date, "%m/%d/%y"))
}

state_cases <- clean_jhu_state(COV_C_US, "cases")
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

COVID_states <- COVID_states %>%
  mutate(dcases = ave(cases, FIPS, FUN = function(x) c(0, diff(x))),
         dcases = ifelse(dcases < 0, NA, dcases),
         ddeaths = ave(deaths, FIPS, FUN = function(x) c(0, diff(x))),
         ddeaths = ifelse(ddeaths < 0, NA, ddeaths),
         cper100k = (cases/Population)*100000,
         cper100k = ifelse(is.nan(cper100k), NA, cper100k),
         dper100k = (deaths/Population)*100000,
         dper100k = ifelse(is.nan(dper100k), NA, dper100k),
         dcper100k = (dcases/Population)*100000,
         dcper100k = ifelse(is.nan(dcper100k), NA, dper100k),
         ddper100k = (ddeaths/Population)*100000,
         ddper100k = ifelse(is.nan(ddper100k), NA, ddper100k),
         log_dcases = log(((dcases+1)/Population)*100000),
         log_ddeaths = log(((ddeaths)/Population)*100000),
         log_dcases = ifelse(is.infinite(log_dcases) | is.nan(log_dcases), NA, log_dcases),
         log_ddeaths = ifelse(is.infinite(log_ddeaths) | is.nan(log_ddeaths), NA, log_dcases),
         weekday = weekdays(Date),
         Monday = ifelse(weekday == "Monday", 1, 0),
         Tuesday = ifelse(weekday == "Tuesday", 1, 0),
         Wednesday = ifelse(weekday == "Wednesday", 1, 0),
         Thursday = ifelse(weekday == "Thursday", 1, 0),
         Friday = ifelse(weekday == "Friday", 1, 0),
         Saturday = ifelse(weekday == "Saturday", 1, 0),
         Sunday = ifelse(weekday == "Sunday", 1, 0))

County_population <- data.frame(
  County = COVID_states$County,
  State = COVID_states$State,
  FIPS = COVID_states$FIPS,
  population = COVID_states$Population
)








# Merge Global cases and deaths into one dataset --------------------------

# use previously made functions to conglomerate cases and deaths by country
CasesbyCountry <- get_area_totals(Cases_Global, .tag = cases, Country, Date)
DeathsbyCountry <- get_area_totals(Deaths_Global, .tag = deaths, Country, Date)

# join global datasets, calculate change in deaths and cases by day
COVID_global <- full_join(CasesbyCountry, DeathsbyCountry, by = c("Country", "Date")) %>%
  mutate(dcases = ave(cases, Country, FUN = function(x) c(0, diff(x))),
         ddeaths = ave(deaths, Country, FUN = function(x) c(0, diff(x))))



# Get data with population information for every country ------------------

# Using the world bank's population data at the country level
popsearch <- wb_search("population, total")[27, ]
population_data <- wb_data(indicator = popsearch$indicator_id, country = "countries_only")
# # TODO: come back to look at different regional populations, as there are 
# # a lot of interesting WB options.
# 
pop_2019 <- population_data %>%
  select(-c(iso2c, iso3c, unit, obs_status, footnote, last_updated)) %>%
  filter(date == 2019) %>%
  rename(Country = country,
         Population = SP.POP.TOTL) %>%
  select(-date)

#find country values in population and covid data that aren't matched
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

nomatch <- outersect(pop_2019$Country, unique(COVID_global$Country))


oldnames <- c("Bahamas, The", "Brunei Darussalam", "Cote d'Ivoire", "Congo, Dem. Rep.",
              "Congo, Rep.", "Egypt, Arab Rep.", "Gambia, The", "Guinea-Bissau",
              "Hong Kong SAR, China", "Iran, Islamic Rep.", "Korea, Dem. People's Rep.",
              "Korea, Rep.", "Kyrgyz Republic", "Lao PDR", "Macao SAR, China",
              "Russian Federation", "Slovak Republic", "St. Vincent and the Grenadines",
              "St. Lucia", "St. Kitts and Nevis", "Syrian Arab Republic", "United States",
              "Venezuela, RB", "Yemen, Rep.", "Western Sahara", "Macao", "Myanmar", "Czech Republic")
newnames <- c("Bahamas", "Brunei", "Ivory Coast", "Democratic Republic of the Congo",
              "Congo", "Egypt", "Gambia", "GuineaBissau", "Hong Kong", "Iran",
              "Korea, Rep.", "South Korea", "Kyrgyzstan", "Laos", "Macao", "Russia",
              "Slovakia", "Saint Vincent and the Grenadines", "Saint Lucia",
              "Saint Kitts and Nevis", "Syria", "US", "Venezuela", "Yemen", "Morocco",
              "Macau", "Burma", "Czechia")

for (i in seq_along(oldnames)){
  cat("iteration ", i, " of ", length(oldnames), "\n")
  pop_2019$Country[pop_2019$Country == oldnames[i]] <- newnames[i]
}
# now there are only 45 names in total that do not match.
nomatch2 <- outersect(pop_2019$Country, unique(COVID_global$Country))


# Combine population data with COVID data ---------------------------------
# Calculate standardized death and case rates
COVID_global <- left_join(COVID_global, pop_2019, by = "Country")


# WB doesn't have population data on Taiwan or Eritrea. POpulation data is from the following website:
# https://www.worldometers.info/world-population/taiwan-population/
# data was taken from the website on 8/29/2020, and may update frequently
# https://en.wikipedia.org/wiki/Diamond_Princess_(ship)
# https://en.wikipedia.org/wiki/MS_Zaandam
COVID_global <- COVID_global %>%
  filter(Country != "Diamond Princess" | Country != "Holy See" | Country != "MS Zaandam") %>%
  mutate(cper100k = (cases/Population)*100000,
         dper100k = (deaths/Population)*100000,
         dcper100k = (dcases/Population)*100000,
         ddper100k = (ddeaths/Population)*100000,
         log_dcases = ((dcases + 1)/Population)*100000,
         log_ddeaths = ((ddeaths + 1)/Population)*100000) %>%
  mutate(Population = ifelse(Country == "Taiwan", 23816775,
                             ifelse(Country == "Eritrea", 3554644,
                                    ifelse(Country == "Diamond Princess", 3711,
                                           ifelse(Country == "MS Zaandam", 1829,
                                                  ifelse(Country == "Holy See", 801, Population))))),
         weekday = weekdays(Date),
         Monday = ifelse(weekday == "Monday", 1, 0),
         Tuesday = ifelse(weekday == "Tuesday", 1, 0),
         Wednesday = ifelse(weekday == "Wednesday", 1, 0),
         Thursday = ifelse(weekday == "Thursday", 1, 0),
         Friday = ifelse(weekday == "Friday", 1, 0),
         Saturday = ifelse(weekday == "Saturday", 1, 0),
         Sunday = ifelse(weekday == "Sunday", 1, 0))



# Clear directory of everything except for state, global and population --------

rm(list= ls()[! (ls() %in% c('COVID_states', 'pop_2019', 'COVID_global'))])
