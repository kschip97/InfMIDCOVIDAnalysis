# Title: Fetch_Census_API_Data #
# Author: Kees Schipper
# Last Updated: 8/25/2020
# 
# Notes:
# - The variable DENSITY is expressed as persons per square mile, and varies widely by county
# - seems that the vintage 2019 populations were all updated in July

library(tidyverse)
library(censusapi)

# Set API environment
Sys.setenv(CENSUS_KEY="5dd5e5cf4f31db7df101cb80b4f9791c49a2b514")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
key <- Sys.getenv("CENSUS_KEY")


# View current Census APIs
apis <- listCensusApis()

# get census metadata
# CensusMeta <- listCensusMetadata(name = "pep/population", vintage = "2019", type = "variables")
CensusPov <- listCensusMetadata(name = "timeseries/poverty/saipe", type = "variables")

# pull census data on population density, date description, population, and county name
# most other variables are missing or nonsensical
censuspop <- getCensus(name = "pep/population", vintage = 2019, key = key,
          vars = c("DENSITY", "DATE_DESC",
                   "POP", "NAME"),
          region = "county")

poverty <- getCensus(name = "timeseries/poverty/saipe", vintage = NULL, key = key,
                       vars = CensusPov$name[1:10],
                     time = "from 2000 to 2019",
                     region = "county")

# change variable names to lowercase for ease of use
names(censuspop) <- tolower(names(censuspop))

# Change state code and population to numeric
censuspop_clean <- censuspop %>%
  dplyr::select(-state) %>%
  mutate(pop = as.numeric(pop)) %>%
  arrange(county, name) %>%
  group_by(county) %>%
  mutate(date_desc = gsub(" ", "", date_desc),
         last_update = gsub("[[:alpha:]]+", "", date_desc),
         last_update = as.Date(last_update, format = "%m/%d/%Y")) %>%
  select(-date_desc)
  
# separate name column into county and state column
censuspop_clean <- separate(censuspop_clean, name, into = c("county", "state"),
                            sep = ", ", remove = T)

#remove the string " County" in the county column
censuspop_clean <- censuspop_clean %>%
  mutate(county = gsub(" County", "", x = county),
         county = trimws(county)) %>% #Just in case there are remaining spaces
  arrange(state, county)



