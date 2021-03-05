## COVID_Mobility_20201124.R ##
## Programmer: Kees SChipper ##
## Date Created: 2020-11-24 ##
## Last Updated: ##

# Installs all the packages that you could ever dream of
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
if (!require(segmented)) install.packages("segmented"); library(segmented)
if (!require(sjPlot)) install.packages("sjPlot"); library(sjPlot)
if (!require(gt)) install.packages("gt"); library(gt)
if (!require(mgcv)) install.packages("mgcv"); library(mgcv)
if (!require(emmeans)) install.packages("emmeans"); library(emmeans)
if (!require(moments)) install.packages("moments"); library(moments)
if (!require(arsenal)) install.packages("arsenal"); library(arsenal)
if (!require(FSA)) install.packages("FSA"); library(FSA)
if (!require(segmented)) install.packages("segmented"); library(segmented)
if (!require(sjPlot)) install.packages("sjPlot"); library(sjPlot)
if (!require(gt)) install.packages("gt"); library(gt)
if (!require(forestplot)) install.packages("forestplot"); library(forestplot)
if (!require(covid19mobility)) install.packages("covid19mobility"); library(covid19mobility)
if (!require(sf)) install.packages("sf"); library(sf)
if (!require(raster)) install.packages("raster"); library(raster)
if (!require(spData)) install.packages("spData"); library(spData)
if (!require(spDataLarge)) install.packages("spDataLarge"); library(spDataLarge)
if (!require(tmap)) install.packages("tmap"); library(tmap)
if (!require(leaflet)) install.packages("leaflet"); library(leaflet)
if (!require(gifski)) install.packages("gifski"); library(gifski)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

set_theme(theme_bw())

# make sure that you have both of these R scripts, as this script depends on them
source('R scripts/COVID_JHU_DATA.R')
source('R scripts/COVID_Daysumm_and_weights_function.R')

USmoburl <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_US.csv"
US_mobility <- read_csv(url(USmoburl))

NAoceaniaurl <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_america_oceania.csv"
NAoceania_mobility <- read_csv(url(NAoceaniaurl))

ASAFmoburl <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_asia_africa.csv"
AsiaAfr_mobility <- read_csv(url(ASAFmoburl))

Brazilmoburl <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_brazil.csv"
Brazil_mobility <- read_csv(url(Brazilmoburl))

Countriesmoburl <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_countries.csv"
Country_mobility <- read_csv(url(Countriesmoburl))

Europemoburl <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_europe.csv"
Europe_mobility <- read_csv(url(Europemoburl))     


# Africa mobility data ----------------------------------------------------

Africa_mobility <- AsiaAfr_mobility %>%
  filter(world_region == "Africa" & `sub region 1` == "Total") %>%
  rename(retail_rec = `retail and recreation`,
         groc_pharm = `grocery and pharmacy`,
         transit = `transit stations`,
         subreg1 = `sub region 1`,
         subreg2 = `sub region 2`) %>%
  mutate(unique_id = paste(country, subreg1, subreg2, sep = "_")) %>%
  group_by(unique_id) %>%
  mutate(day = 1:length(unique(date)))

ggplot(data = Africa_mobility, aes(x = day)) +
  geom_smooth(aes(y = groc_pharm, group = country, color = country), se = F, span = 0.5) +
  theme(legend.position = "none") +
  labs(title = "Africa Mobility Data")

State_mobility <- US_mobility %>%
  mutate(county = str_remove(county, " County")) %>%
  rename(retail_rec = `retail and recreation`,
         groc_pharm = `grocery and pharmacy`,
         transit = `transit stations`)

US_shape <- map_data("state") %>%
  st_as_sf()

US_geo_data <- State_tot_mobility %>%
  left_join(US_shape, by = c("state" = "NAME")) %>%
  mutate(transit = ifelse(is.na(transit), 0, transit)) %>%
  ungroup() %>%
  rename(geom = geometry) %>%
  st_as_sf()

ggplot(data = US_geo_data, aes(x = day)) +
  geom_smooth(aes(y = transit, group = REGION, color = REGION), method = "loess", se = F, span = 0.20) +
  labs(title = "Overall State Transportation Mobility Data") 
  # theme(legend.position = "none")




# Mobility Data for Middlesex, MA -----------------------------------------

Middlesex_mobility <- State_mobility %>%
  filter(state == "Massachusetts" & county == "Middlesex") %>%
  mutate(time = row_number())

for_plot <- Middlesex_mobility %>%
  pivot_longer(cols = c(retail_rec:residential),
               names_to = "mobility_type",
               values_to = "change")

ggplot(data = for_plot, aes(x = time)) +
  geom_vline(xintercept = c(42, 85, 101, 125, 141, 190, 206, 286), color = "red") +
  annotate(geom = "rect", xmin = 6, xmax = 15, ymin = -Inf, ymax = Inf, fill = "green2", alpha = 0.3) +
  annotate(geom = "rect", xmin = 80, xmax = 185, ymin = -Inf, ymax = Inf, fill = "firebrick3", alpha = 0.3) +
  geom_smooth(aes(y = change, color = mobility_type), method = "loess", span = 7/nrow(Middlesex_mobility), se = F) +
  scale_color_manual(values=c("black", "purple", "blue", "deeppink", "greenyellow", "cyan1")) +
  ylab("Mobility change from baseline") +
  xlab("Days since February 15th, 2020") +
  labs(title = "Mobility trends from Middlesex County, MA")

# # fuck this fucking map damnit --------------------------------------------
# 
# 
# animated_map <- ggplot() +
#   geom_sf(data = US_geo_data, aes(geometry = geom, fill = transit)) +
#   coord_sf(datum = st_crs(4269)) +
#   scale_fill_viridis_c(option = "plasma") 
#   transition_states(states = date, transition_length = 1, state_length = 1)
# 
# animate(animated_map)
