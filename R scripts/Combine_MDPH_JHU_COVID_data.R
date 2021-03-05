## Combine_MDPH_JHU_COVID_data.R ##
## Created by: Kees Schipper ##
## Date Created: 2021-01-28 ##
## Date Updated: 2021-01-28 ##
rm(list = ls())

library(MASS)


# Read in previous scripts and load in data -------------------------------


source('R scripts/COVID_JHU_Data.R')

MDPH_data <- read_csv(paste0("cleaned data/from_MDPH_web_Middlesex_data_2021-01-28.csv"))

rm(list=ls()[! ls() %in% c("MDPH_data","COVID_stmobil")])


# rename columns in both datasets so that they match ----------------------

Middlesex_JHU <- COVID_stmobil %>%
  filter(State == "Massachusetts" & County == "Middlesex") %>%
  filter(Date > "2020-03-01") %>%
  mutate(dcases = ave(cases, FIPS, FUN = function(x) c(0, diff(x))),
         ddeaths = ave(deaths, FIPS, FUN = function(x) c(0, diff(x))))

MDPH_merge <- MDPH_data %>%
  rename(cases = Cases,
         deaths = Deaths,
         tests = Tests)

# Merge Datasets but keep data differentiated -----------------------------

Middlesex_compare <- Middlesex_JHU %>%
  left_join(MDPH_merge, by = c("County", "Date"), suffix = c(".JHU", ".MDPH")) %>%
  select(Date, cases.JHU:ddper100k, cases.MDPH:tests, retail_rec:residential, everything()) %>%
  select(-c(weekday:Sunday), -c(time:weeknd)) %>%
  filter(Date < "2021-01-20") %>%
  mutate(cases.MDPH = na.interp(cases.MDPH, linear = TRUE),
         deaths.MDPH = na.interp(deaths.MDPH, linear = TRUE),
         tests = na.interp(deaths.MDPH, linear = TRUE),
         dcases.MDPH = ave(cases.MDPH, County, FUN = function(x) c(0, diff(x))),
         ddeaths.MDPH = ave(deaths.MDPH, County, FUN = function(x) c(0, diff(x))),
         dtests = ave(tests, County, FUN = function(x) c(0, diff(x))))


# Missing data: public holidays? ------------------------------------------
# missingness on the following dates for MDPH data:
# 2020-05-31, 2020-08-23, 2020-11-26, 2020-12-25, 2020-01-01
# don't know about May 31 or August 23, but the rest are Thanksgiving, Christmas,
# and new years
# NOTE: I have used linear interpolation to fill missing values in the CUMULATIVE data,
# and then differentiated to get rate data
# for some reason, January 11, 2021 is remaining duplicated. Probably an artifact where
# there are two instances in the COVID data where January 11 has differing values.

miss_list <- lapply(Middlesex_compare, function(x) sum(is.na(x)))
which_miss <- lapply(Middlesex_compare, function(x) Middlesex_compare$Date[which(is.na(x))])


# Visualizing differences between JHU and MDPH data -----------------------

Middlesex_compare %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = cases.JHU), color = "blue", size = 2) +
  geom_line(aes(y = cases.MDPH), color = "coral3", size = 2) +
  labs(title = "Cumulative COVID cases by date \n (JHU in blue and MDPH in red)")
  

Middlesex_compare %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = deaths.JHU), color = "blue", size = 2) +
  geom_line(aes(y = deaths.MDPH), color = "coral3", size = 2) +
  labs(title = "COVID deaths by date \n (JHU in blue and MDPH in red)")

Middlesex_compare %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = ddeaths.JHU), color = "blue", size = 1) +
  geom_line(aes(y = ddeaths.MDPH), color = "coral3", size = 1) +
  labs(title = "COVID daily deaths by date \n (JHU in blue and MDPH in red)")

Middlesex_compare %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = dcases.MDPH), color = "coral3", size = 1) +
  geom_line(aes(y = dcases.JHU), color = "blue", size = 1) +
  labs(title = "COVID cases by date \n (JHU in blue and MDPH in red)")

# TODO: Need to account for correction in data from August 12-18. In JHU data, you set
# the values to zero.


# Create negative binomial models -----------------------------------------


  
