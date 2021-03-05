## COVID_TS_PANELPLOT.R ##
## Programmer: Kees Schipper ##
## Created: 2021-02-22 ##
## Last updated: 2021-02-22 ##

rm(list = ls())
dev.off()

library(tidyverse)
library(viridis)
library(ggridges)
library(kza)


# read in COVID data ------------------------------------------------------

load('cleaned data/COVID_master_20200218.RData')

COVID_county <- COVID_master %>%
  filter(Population > 50000) %>%
  group_by(fips) %>%
  mutate(daily_cases_smooth = kz(daily_cases_100k, m = 14, k = 3),
         daily_deaths_smooth = kz(daily_cases_100k, m = 14, k = 3),
         accel_cases = c(0, diff(daily_cases_smooth)),
         accel_deaths = c(0, diff(daily_cases_smooth)))


# Ridgeplot by for a state ------------------------------------------------

COVID_spec <- COVID_county %>%
  filter(state == "Pennsylvania")

COVID_spec %>%
  ggplot(aes(
    x = date, 
    y = reorder(county, Population), 
    height = daily_cases_smooth/max(daily_cases_smooth), 
    fill = county)) +
  geom_ridgeline() +
  theme(legend.position = 'none',
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8))
  

# Raster plot for all counties across the country -------------------------

COVID_county %>%
  filter(date > "2020-03-01") %>%
  ggplot(aes(x = date, y = reorder(fips, state), fill = daily_cases_smooth)) +
  geom_tile() + 
  scale_fill_viridis(limits = c(0, 175)) +
  theme(axis.text.y = element_blank())


# Examine relationship between case acceleration and holidays -------------

COVID_county %>%
  filter(state == "Massachusetts" & county == "Middlesex") %>%
  ggplot(aes(x = date, y = accel_cases)) +
  geom_line(size = 2) +
  geom_vline(xintercept = as.Date(c("2020-09-01","2020-11-24", "2020-12-23", "2021-01-01")),
             color = "red")
