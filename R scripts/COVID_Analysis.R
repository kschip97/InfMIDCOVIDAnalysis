## COVID_Analysis.R ##
## Author: Kees Schipper ##
## Created: 2021-02-18 ##
## Last Updated: 2021-02-25 ##
## Notes: Continuing from COVID data retrieval, we will follow with these steps:
## (3) clean Middlesex data at dates where JHU and MDPH values differ
## (4) create negative binomial models of DoW fluctuations for cases, deaths, and tests
## (5) create time series visualizations for cases, deaths, and tests, in a stacked
##     ggarrange plot
## (6) Save a reusable dataset for further analysis so that results are consistent across
##     runs for middlesex county

rm(list = ls())

library(tidyverse)
library(data.table)
library(forecast)
library(kza)
library(lubridate)
library(broom)

source('R scripts/COVID_Analysis_Utilities.R')
load('cleaned data/COVID_master_2021-02-24.RData')


# Calculate total population for Massachusetts ----------------------------

MA_pop <- FilterCovid(COVID_master, State = "Massachusetts") %>%
    dplyr::select(county, Population) %>%
    count(county, Population, sort = T) %>%
    summarise(totpop = sum(Population)) %>% as.numeric()


# Create dataset for Middlesex, and add MA population ---------------------

MD <- FilterCovid(COVID_master, State = "Massachusetts", County = "Middlesex") %>%
  filter(date >= "2020-03-01" & date <= "2021-03-01") %>%
  mutate(MA_pop = MA_pop,
         daily_cases = ifelse(date > as.Date('2020-09-01') & daily_cases <= 0, NA, daily_cases),
         daily_cases = na.interp(daily_cases, linear = T),
         daily_cases_100k = ifelse(date > as.Date('2020-09-01') & daily_cases_100k <= 0, NA, daily_cases_100k),
         daily_cases_100k = na.interp(daily_cases_100k, linear = T),
         weekday = factor(weekday),
         weekday = fct_relevel(weekday, "Sunday", "Monday", "Tuesday", "Wednesday",
                               "Thursday", "Friday", "Saturday"))



# Correct Reporting anomalies in MD data ----------------------------------

ind <- c()
val <- c()
change_ind <- c()
change_date <- c()
# for loop interpolates reporting anomalies
for (i in 1:nrow(MD)){
  if (sum(MD$daily_deaths_100k[1:i]) == 0){
    
    ind <- c()
    val <- c()
    
  } else if (MD$daily_deaths_100k[i] < 0){
    
    MD$daily_deaths_100k[i] <- 0
    
  } else if (MD$daily_deaths_100k[i] == 0){
    
    ind <- c(ind, i)
    val <- c(val, MD$daily_deaths_100k[i])
    
  } else if (MD$daily_deaths_100k[i] > 0 & length(ind) >= 1){
    
    ind <- c(ind, i)
    val <- c(val, MD$daily_deaths_100k[i])
    
    i1 <- ind[1]
    i2 <- ind[length(ind)]
    
    
    if (sum(val) >= 1){
      
      MD$daily_deaths_100k[i1:i2] <- sum(val)/length(val)
      change_ind <- c(change_ind, ind)
      change_date <- MD$date[change_ind]
      
    }
    
    #reset indicies and values
    ind <- c()
    val <- c()
  }
}

# add testing data to Middlesex data --------------------------------------

MD_testing <- read_csv('cleaned data/from_MDPH_web_Middlesex_data_2021-01-28.csv') %>%
  dplyr::select(Date, Tests, dtests) %>%
  rename(date = Date,
         tests = Tests,
         daily_tests = dtests) %>%
  mutate(daily_tests = na.interp(daily_tests, linear = T))

MDWTests <- MD %>%
  left_join(MD_testing, by = 'date') %>%
  mutate(daily_tests_100k = (daily_tests/MA_pop)*100000,
         daily_tests_100k = na.interp(daily_tests_100k, linear = T),
         daily_tests = na.interp(daily_tests, linear = T),
         cases_smooth = kz(daily_cases_100k, m = 21, k = 3),
         deaths_smooth = kz(daily_deaths_100k, m = 21, k = 3),
         tests_smooth = kz(daily_tests_100k, m = 21, k = 3))

# TODO: Missing May 31st from testing data. Had to use linear interpolation.
# Probably one other day missing.

# calculate inflection points as 0-points on accelerations ----------------
# create a for loop seeing where accelerations change from positive to negative,
# and negative to positive
# these inflection points will be used to define critical periods

# find values of acceleration curves that change from pos to neg, and neg to pos
case_zeros <- .FindAccelZeros(df = MDWTests, var = 'daily_cases_100k')
death_zeros <- .FindAccelZeros(df = MDWTests, var = 'daily_deaths_100k')
test_zeros <- .FindAccelZeros(df = MDWTests, var = 'daily_tests_100k')

# If you want to test multiple windows and iterations with KZ filter
# par(mfrow = c(5, 5))
# for (j in 1:4){
#   for (i in 10:50){
#     test_zeros <- .FindAccelZeros(df = MDWTests, var = 'daily_tests_100k', smwindow = i, smiter = j)
#   }
# }


# defining critical periods -----------------------------------------------
# NOTE: These critical periods are not automatically generted from case_zeros, death_zeros
# etc... There was significant subjectivity in determining these critical periods, as some
# empirically derived changes in acceleration didn't translate to visual changes in 
# case velocity.

# case critical periods:
# T1: 2020-03-01 to 2020-04-18
# T2: 2020-04-19 to 2020-06-11
# T3: 2020-06-12 to 2020-10-01
# T4: 2020-10-02 to 2021-01-02
# T5: 2021-01-03 to 2021-03-01

# death critical periods:
# "2020-04-28" "2020-08-01" "2020-10-17"
# T1: 2020-03-01 to 2020-04-28
# T2: 2020-03-29 to 2020-08-01
# T3: 2020-08-02 to 2020-11-14
# T4: 2020-11-15 to 2021-01-11
# T5: 2021-01-11 to 2020-03-01

# create critical period variables
MD_CPs <- MDWTests %>%
  mutate(
    CP_cases = case_when(
      date <= "2020-04-18" ~ 'T1',
      date > "2020-04-18" & date <= "2020-06-11" ~ 'T2',
      date > "2020-06-11" & date <= "2020-10-01" ~ 'T3',
      date > "2020-10-01" & date <= "2021-01-02" ~ 'T4',
      date > "2021-01-02" & date <= "2021-03-01" ~ 'T5'
    ),
    CP_deaths = case_when(
      date <= "2020-04-18" ~ 'T1',
      date > "2020-04-18" & date <= "2020-06-11" ~ 'T2',
      date > "2020-06-11" & date <= "2020-10-01" ~ 'T3',
      date > "2020-10-01" & date <= "2021-01-02" ~ 'T4',
      date > "2021-01-02" & date <= "2021-03-01" ~ 'T5'
    )
  )

daysinCP <- function(var, CP){
  sum(MD_CPs[[var]] == CP, na.rm = T)
}

# calculate the number of days in each critical period
CP_days <- c("T1C_days", "T2C_days", "T3C_days", "T4C_days", "T5C_days",
             "T1D_days", "T2D_days", "T3D_days", "T4D_days", "T5D_days")
days <- vector(length = length(CP_days))

for (i in 1:length(days)){
  
  if (i <= 5){
    
    days[i] <- daysinCP('CP_cases', paste0('T', i))
  
  } else if (i > 5){
    
    days[i] <- daysinCP('CP_deaths', paste0('T', i-5))
    
  }
}
names(days) <- CP_days
days <- days + 1

MD_CPs <- MD_CPs %>%
  mutate(
    T1C_ts = row_number(),
    T2C_ts = T1C_ts - days[1],
    T2C_ts = ifelse(T2C_ts < 0, 0, T2C_ts),
    T3C_ts = T1C_ts - sum(days[1:2]),
    T3C_ts = ifelse(T3C_ts < 0, 0, T3C_ts),
    T4C_ts = T1C_ts - sum(days[1:3]),
    T4C_ts = ifelse(T4C_ts < 0, 0, T4C_ts),
    T5C_ts = T1C_ts - sum(days[1:4]),
    T5C_ts = ifelse(T5C_ts < 0, 0, T5C_ts)
  ) %>%
  mutate(
    T1D_ts = row_number(),
    T2D_ts = T1D_ts - days[6],
    T2D_ts = ifelse(T2D_ts < 0, 0, T2D_ts),
    T3D_ts = T1D_ts - sum(days[6:7]),
    T3D_ts = ifelse(T3D_ts < 0, 0, T3D_ts),
    T4D_ts = T1D_ts - sum(days[6:8]),
    T4D_ts = ifelse(T4D_ts < 0, 0, T4D_ts),
    T5D_ts = T1D_ts - sum(days[6:9]),
    T5D_ts = ifelse(T5D_ts < 0, 0, T5D_ts)
  )



# Create case, death, and testing models ----------------------------------

library(MASS)

# test model
testmod <- glm.nb(daily_tests_100k ~ tests_smooth + weekday + T1C_ts + I(T1C_ts**2) + T2C_ts + 
                   I(T2C_ts**2) + T3C_ts + I(T3C_ts**2) + T4C_ts + I(T4C_ts**2) + T5C_ts +
                   I(T5C_ts**2), data = MD_CPs)
tidy(testmod, conf.int = T, exponentiate = T)
plot(MD_CPs$date, MD_CPs$daily_tests_100k, type = "l", col = "black", main = "Testing Model Fit")
lines(MD_CPs$date, testmod$fitted.values, type = "l", col = "red")

# Case model without adjusting for tests
casemod <- glm.nb(daily_cases_100k ~ cases_smooth + weekday + T1C_ts + I(T1C_ts**2) + T2C_ts + 
                    I(T2C_ts**2) + T3C_ts + I(T3C_ts**2) + T4C_ts + I(T4C_ts**2) + T5C_ts +
                    I(T5C_ts**2), data = MD_CPs)
tidy(casemod, conf.int = T, exponentiate = T)
plot(MD_CPs$date, MD_CPs$daily_cases_100k, type = "l", col = "black", main = "Cases Model Fit")
lines(MD_CPs$date, casemod$fitted.values, type = "l", col = "red")

# death model without adjusting for tests
deathmod <- glm.nb(daily_deaths_100k ~ deaths_smooth + weekday + T1D_ts + I(T1D_ts**2) + T2D_ts +
                     I(T2D_ts**2) + T3D_ts + I(T3D_ts**2) + T4D_ts + I(T4D_ts**2) + T5D_ts +
                     I(T5D_ts**2), data = MD_CPs)
tidy(deathmod, conf.int = T, exponentiate = T)
plot(MD_CPs$date, MD_CPs$daily_deaths_100k, type = "l", col = "black", main = "Deaths Model Fit")
lines(MD_CPs$date, deathmod$fitted.values, type = "l", col = "red")



# Run case and death models while adjusting for tests ---------------------

# Case model without adjusting for tests
casemod <- glm.nb(daily_cases_100k ~ cases_smooth + weekday + T1C_ts + I(T1C_ts**2) + T2C_ts + 
                    I(T2C_ts**2) + T3C_ts + I(T3C_ts**2) + T4C_ts + I(T4C_ts**2) + T5C_ts +
                    I(T5C_ts**2) + daily_tests_100k, data = MD_CPs)
tidy(casemod, conf.int = T, exponentiate = T)
plot(MD_CPs$date, MD_CPs$daily_cases_100k, type = "l", col = "black", main = "Cases Model Fit")
lines(MD_CPs$date, casemod$fitted.values, type = "l", col = "red")

# death model without adjusting for tests
deathmod <- glm.nb(daily_deaths_100k ~ deaths_smooth + weekday + T1D_ts + I(T1D_ts**2) + T2D_ts +
                     I(T2D_ts**2) + T3D_ts + I(T3D_ts**2) + T4D_ts + I(T4D_ts**2) + T5D_ts +
                     I(T5D_ts**2) + daily_tests_100k, data = MD_CPs)
tidy(deathmod, conf.int = T, exponentiate = T)
plot(MD_CPs$date, MD_CPs$daily_deaths_100k, type = "l", col = "black", main = "Deaths Model Fit")
lines(MD_CPs$date, deathmod$fitted.values, type = "l", col = "red")



# List of potential holidays/Calendar effects -----------------------------
# Memorial Day:                                       May 25th, 2020
# Labor Day:                                          September 7th, 2020
# College spring break starts/classes go remote:     March 23-25, 2020
# College spring semesters end:                      May 7-14th, 2020
# Thanksgiving week:                                  November 23-27, 2020
# Fall Semesters End:                                 December 20, 2020
# Christmas week:                                     December 21-26
# Hannukah:                                           December 10-18
# New Years eve/day:                                  December 31-January 01, 2020/2021
# Spring Semesters Begin:                             January 25, 2021
# Super bowl:                                         February 7th, 2021
# Valentine's Day:                                    February 14th, 2021
# Bars, Restaurants, Businesses open to 40% cap       February 7th, 2021



# Plot dates with respect to cases, deaths and tests ----------------------

date_vec <- as.Date(c("2020-03-23", "2020-05-10", "2020-05-25", "2020-09-07", "2020-11-23",
                      "2020-12-10","2020-12-23", "2021-01-01", "2021-02-07", "2021-02-14"))
date_labels <- c("Spring Break/Remote Learning", "End Spring Sem", "Memorial Day",
                 "Labor day/school start", "Thanksgiving Week", "Hannukah", "Christmas",
                 "New Years", "Super Bowl", "Valentine's Day")

dates <- data.frame(
  date = date_vec,
  labels = date_labels,
  y = rep(75, 10)
)

summer_recess <- data.frame(
  xmin = as.Date("2020-05-10"),
  xmax = as.Date("2020-09-07"),
  ymin = -Inf,
  ymax = Inf
)
Fall_2020 <- data.frame(
  xmin = as.Date("2020-09-07"),
  xmax = as.Date("2020-12-20"),
  ymin = -Inf,
  ymax = Inf
)
winter_recess <- data.frame(
  xmin = as.Date("2020-12-20"),
  xmax = as.Date("2021-01-25"),
  ymin = -Inf,
  ymax = Inf
)
Spring_2021 <- data.frame(
  xmin = as.Date("2021-01-25"),
  xmax = as.Date(Sys.Date()),
  ymin = -Inf,
  ymax = Inf
)
pre_covid_school <- data.frame(
  xmin = as.Date("2020-03-01"),
  xmax = as.Date("2020-03-23"),
  ymin = -Inf,
  ymax = Inf
)
start_remote <- data.frame(
  xmin = as.Date("2020-03-23"),
  xmax = as.Date("2020-05-10"),
  ymin = -Inf,
  ymax = Inf
)
thnksgvg_brk <- data.frame(
  xmin = as.Date("2020-11-20"),
  xmax = as.Date("2020-11-30"),
  ymin = -Inf,
  ymax = Inf
)

sch_cal_data <- MD_CPs %>%
  pivot_longer(cols = c("daily_cases_100k", "daily_deaths_100k", "daily_tests_100k"),
               names_to = 'outcome',
               values_to = 'values')

ggplot() +
  rectobj(df = pre_covid_school, color = 'coral', alpha = 0.3) +
  rectobj(df = start_remote, color = 'green3', alpha = 0.3) +
  rectobj(df = summer_recess, color = 'green3', alpha = 0.3) +
  rectobj(df = Fall_2020, color = "coral", alpha = 0.3) +
  rectobj(df = winter_recess, color = 'green3', alpha = 0.3) +
  rectobj(df = Spring_2021, color = "coral", alpha = 0.3) +
  rectobj(df = thnksgvg_brk, color = "green3", alpha = 0.3) +
  geom_line(aes(x = date, y = values), data = sch_cal_data) +
  facet_wrap(~outcome, ncol = 1, scales = "free_y") +
  labs(title = "COVID-19 Dynamics with School Calendar Effects",
       x = 'date', y = 'measurements')

