## COVID_Analysis_Viz.R ##
## Author: Kees Schipper ##
## Created: 2021-02-25 ##
## Last Updated: 2021-02-25 ##

library(tidyverse)
library(data.table)
library(forecast)
library(kza)
library(lubridate)
library(broom)

source('R scripts/COVID_Analysis.R')



# Visualization of COVID dynamics with school calendar --------------------

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
  xmax = as.Date("2020-03-20"),
  ymin = -Inf,
  ymax = Inf
)
spring_break_2020 <- data.frame(
  xmin = as.Date("2020-03-20"),
  xmax = as.Date("2020-03-30"),
  ymin = -Inf,
  ymax = Inf
)
SB_to_summer <- data.frame(
  xmin = as.Date("2020-03-30"),
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
  rectobj(df = summer_recess, color = 'green3', alpha = 0.3) +
  rectobj(df = Fall_2020, color = "coral", alpha = 0.3) +
  rectobj(df = winter_recess, color = 'green3', alpha = 0.3) +
  rectobj(df = Spring_2021, color = "coral", alpha = 0.3) +
  rectobj(df = thnksgvg_brk, color = "green3", alpha = 0.3) +
  rectobj(df = spring_break_2020, color = "green3", alpha = 0.3) +
  rectobj(df = SB_to_summer, color = "coral", alpha = 0.3) +
  geom_line(aes(x = date, y = values), data = sch_cal_data) +
  facet_wrap(~outcome, ncol = 1, scales = "free_y") +
  labs(title = "COVID-19 Dynamics with School Calendar Effects",
       x = 'date', y = 'measurements')
