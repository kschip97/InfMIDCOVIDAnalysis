## COVID_timeseries_analysis.R ##
# Author: Kees Schipper
# Date Created: 10/1/2020
# Last Updated: 10/20/2020
# Notes: 
# This Script is for analysis of data created through COVID_JHU_DATA.R. This involves analysis of 
# Day-of-the-week effects through negative binomial regression, interrupted time series analysis for public
# holidays and other calendar events, and the effectiveness of public health interventions on COVID prevention.
# We will also look to smooth the COVID data with the use of KZ filters
# TODO: Test different model fits of death and case data. Fit cubic splines, negative binomial (with sunday
# as the reference), multiple iterations of multi-day filters, etc...

source('R scripts/COVID_JHU_DATA.R')

if (!require(kza)) install.packages("kza"); library(kza)
if (!require(mgcv)) install.packages("mgcv"); library(mgcv)
if (!require(emmeans)) install.packages("emmeans"); library(emmeans)
if (!require(moments)) install.packages("moments"); library(moments)
if (!require(arsenal)) install.packages("arsenal"); library(arsenal)
if (!require(FSA)) install.packages("FSA"); library(FSA)
conflict_prefer("lag", "dplyr")



# Model day of the week effects in a specific county ----------------------------

COVID_county <- COVID_states %>%
  filter(State == "Massachusetts" & County == "Middlesex")

COVID_county <- COVID_county %>%
  mutate(time = 1:nrow(COVID_county),
         time = as.numeric(time),
         weekfac = factor(weekday),
         weekfac = ordered(weekday, levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday",
                                               "Thursday", "Friday")))



# Incorporate google satellite mobility -----------------------------------
# Maybe some potential for continuously updated mobility reports, but this source seems sketchy
# url.google.mobil <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_US.csv"
# 
# COVID_mobil_test <- read_csv(url(url.google.mobil))

COVID_mobility <- read_csv("Raw Data/2020_US_Region_Mobility_Report.csv") %>%
  rename(FIPS = census_fips_code,
         County = sub_region_2,
         State = sub_region_1,
         Date = date) %>%
  mutate(FIPS = as.numeric(FIPS),
         Date = as.Date(Date)) %>%
  select(-County, -State, -country_region_code, -country_region, -iso_3166_2_code, -metro_area) %>%
  filter(!is.na(FIPS))

names(COVID_mobility)[3:8] <- c("retail_rec", "groc_pharm", "parks", "transit", "workplace", "residential")

COVID_stmobil <- left_join(COVID_states, COVID_mobility, by = c("FIPS", "Date")) %>%
  filter(!is.na(FIPS)) %>%
  group_by(FIPS) %>%
  mutate(time = 1:nrow(COVID_county),
         time = as.numeric(time),
         weekend = ifelse(weekday == "Saturday" | weekday == "Sunday", 1, 0)) %>%
  ungroup() %>%
  mutate(FIPS = as.character(FIPS),
         weekfac = ordered(weekday, levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday",
                                              "Thursday", "Friday"))) %>%
  filter(Population != 0)


save(COVID_stmobil, file = "cleaned data/COVID_state_mobility.RData")

Mobility_county <- COVID_stmobil %>%
  filter(State == "Massachusetts" & County == "Middlesex")


# Plot Mobility county data -----------------------------------------------

Mobility_plot <- Mobility_county %>%
  pivot_longer(cols = c("retail_rec", "groc_pharm", "parks", "transit", "workplace", "residential", "dcper100k",
                        "ddper100k"),
               names_to = "variables",
               values_to = "values")

ggplot(Mobility_plot, aes(x = time, y = values)) + geom_line(aes(color = variables), size = 1) +
  geom_smooth(method = "loess", color = "black", se = T, span = .2) +
  facet_grid(variables ~ ., scales = "free_y") + 
  theme(legend.position = "none") +
  ggtitle(label = "COVID Case, Death, and Mobility plot with 0.2 span loess")

ggplot(Mobility_plot, aes(x = weekfac, y = values)) + geom_boxplot(aes(x = weekfac, group = weekfac)) +
  facet_wrap(variables ~ ., scales = "free_y", nrow = 2, ncol = 4) +
  stat_summary(aes(x = weekfac, y = values), fun = "mean", color= "red", size = 0.25) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(label = "Case, Death, and Mobility boxplots")

ggplot(Mobility_plot, aes(y = values)) + geom_histogram() +
  facet_wrap(variables ~ ., scales = "free_y", nrow = 2, ncol = 4) +
  ggtitle(label = "Histograms of key variables")

## Dates of interest:
# 2020-03-15 Public/Private schools closed, restaurant and dine-in services banned as well -> i = 54
# 2020-03-17 MBTA running subways on weekend service schedule during the week -> i = 56
# 2020-03-22 Shelter-in-place order -> i = 61
# 2020-06-01 MA starts reporting probable COVID cases and deaths in their data -> i = 132
# Should be a large spike in cases due to changes
# 2020-06-08 phase II of reopening plan -> i = 139
# 2020-07-06 phase III of reopening plan begins -> 167


# Workflow ----------------------------------------------------------------

## From recording with Ryan
# 1. Transmission is highly reflective of mobility in which people have close proximity
# 2. How do relationships of COVID relate to COVID mobility data
# 3. Aim 1: describe case and death fluctuations, as well as mobility data
#    Control day of the week effect and smooth data
#    Statistical Hypothesis: What is the mean/median of each day of the weekdays
#    Calculate means, medians, sd, sample size, skewness, kurtosis for cases and deaths by day of week
#    Conduct Kruskal wallis test for mean ranks-->equivalent to nonparametric ANOVA
#    Dunn's test for medians-->conducted for multiple comparisons of individual days
# 4. Aim 2: Define if there is correlation between mobility and disease outcomes
#    Subaim 1: If mobility increases, does disease increase
#              Are these things related with a lag of 0?
#              H0 = There is no statistically significant serial correlation of lag 0
#    Subaim 2: Is there a correlation between two things with a lag of x?
#              Correlation but not causation
#              Statistical hypothesis = there is no lagged correlation
#    Subaim 3: Compare time series curves across counties
#              Is there significant serial correlation across Massachusetts counties at lag 0?
#              H0 = there is no significant serial correlation across MA counties at lag 0
#              Is there significant serial correlation across MA counties at lag x?
#              H0 = there is no significant serial correlation across MA counties at lag x
#       - Conduct serial correlation tests for both (1) unsmoothed and (2) smoothed case and death
#         curves
   
  

# Univariate statistics for cases and deaths by day of the week -----------

Mob_sum_stats <- Mobility_county %>%
  select(12:13, 23:28, weekfac) %>%
  pivot_longer(cols = c("dcper100k", "ddper100k", "groc_pharm", "parks", "residential",
                        "retail_rec", "transit", "workplace"),
               names_to = "variables",
               values_to = "values") %>%
    group_by(variables, weekfac) %>%
  summarise(Mean = mean(values, na.rm = T),
            Median = median(values, na.rm = T),
            StdDev = sd(values, na.rm = T),
            Skew = skewness(values, na.rm = T),
            Kurtos = kurtosis(values, na.rm = T),
            nobs = n())

mycontrols  <- tableby.control(test=T, total=T,
                               numeric.test="kwt", cat.test="chisq",
                               numeric.stats=c("N", "meansd", "median"),
                               cat.stats=c("countpct"),
                               stats.labels=list(N='Count', median='Median', meansd = "Mean (SD)"))

tab_county <- tableby(
  weekfac ~ dcper100k + ddper100k + groc_pharm + parks + residential + 
  retail_rec + transit + workplace , control = mycontrols,
  data = Mobility_county
  )

summary(tab_county, text = T)


# Univariate stats for cases and deaths for all of MA ---------------------

Mob_sum_day <- Mobility_county %>%
  select(12:13, 23:28, weekfac) %>%
  pivot_longer(cols = c("dcper100k", "ddper100k", "groc_pharm", "parks", "residential",
                        "retail_rec", "transit", "workplace"),
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables, weekfac) %>%
  summarise(Mean = mean(values, na.rm = T),
            StdDev = sd(values, na.rm = T),
            Min = min(values, na.rm = T),
            Q1 = quantile(values, probs = 0.25, na.rm = T),
            Median = median(values, na.rm = T),
            Q3 = quantile(values, probs = 0.75, na.rm = T),
            Max = max(values, na.rm = T),
            Skew = skewness(values, na.rm = T),
            Kurtos = kurtosis(values, na.rm = T),
            nobs = sum(!is.na(values)))
write_csv(Mob_sum_day, path = "summary tables/WeekdaySumm.csv")

Mob_sum_tot <- Mobility_county %>%
  select(12:13, 23:28, weekfac) %>%
  pivot_longer(cols = c("dcper100k", "ddper100k", "groc_pharm", "parks", "residential", 
                        "retail_rec", "transit", "workplace"),
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables) %>%
  summarise(Mean = mean(values, na.rm = T),
            StdDev = sd(values, na.rm = T),
            Min = min(values, na.rm = T),
            Q1 = quantile(values, probs = 0.25, na.rm = T),
            Median = median(values, na.rm = T),
            Q3 = quantile(values, probs = 0.75, na.rm = T),
            Max = max(values, na.rm = T),
            Skew = skewness(values, na.rm = T),
            Kurtos = kurtosis(values, na.rm = T),
            nobs = sum(!is.na(values)))
write_csv(Mob_sum_tot, path = "summary tables/AggDaySumm.csv")

tab_county <- tableby(
  weekfac ~ dcper100k + ddper100k + groc_pharm + parks + residential + 
  retail_rec + transit + workplace , control = mycontrols,
  data = Mobility_county
  )

summary(tab_county, text = T)


# Deal with outliers in death and case data -------------------------------
## There are strings of 0 deaths interspersed with unusually high death counts in the data.
## I plan to distribute these deaths to the previous string of zeros, using median ddper100k
## as a weight to distribute the total on the day that deaths are reported

interp_weights <- tibble(
  variable = Mob_sum_day$variables,
  weekfac  = Mob_sum_day$weekfac,
  medians = Mob_sum_day$Median
) %>%
  mutate(
    variable = paste0(variable, "_wt")
  ) %>%
  group_by(variable) %>%
  mutate(
    totals = sum(medians),
    weights = medians/totals
  ) %>%
  ungroup() %>%
  pivot_wider(id_cols = c("weekfac"), names_from = "variable", values_from = "weights")

Mobility_county <- left_join(Mobility_county, interp_weights, by = c("weekfac"))



# Graph smoothers for outlier-corrected data ------------------------------


SmoothOption <- function(days, color, size = 1){
  geom_smooth(span = days/nrow(Mobility_county), se = F, color = color, size = size)
}


ggplot(Mobility_county, aes(x = time, y = dcper100k)) +
  geom_line(col = "black", size = .75) + 
  ylab("Cases per 100k Population") +
  ggtitle(label = "COVID Cases per 100k Population in Middlesex County",
                                              subtitle = "From January 22 to October 22, 2020") +
  
  SmoothOption(3, "purple", size = 2) +
  SmoothOption(7, "orange", size = 2) +
  SmoothOption(14, "red", size = 2) +
  SmoothOption(21, "black", size = 2)

ggplot(Mobility_county, aes(x = time, y = ddper100k)) +
  geom_line() +
  SmoothOption(3, "purple", size = 2) +
  SmoothOption(7, "orange", size = 2) +
  SmoothOption(14, "red", size = 2) +
  SmoothOption(21, "black", size = 2)



# Apply KZA filters to COVID case and death data --------------------------


# Plot KZA filter with 
plot(kza(Mobility_county$ddper100k, m = 7, k = 2))
plot(kza(Mobility_county$dcper100k, m = 7, k = 2))

deathkza <- kza(Mobility_county$ddper100k, m = 7, k = 2, impute_tails = TRUE)
casekza <- kza(Mobility_county$dcper100k, m = 7, k = 2, impute_tail = TRUE)

KZmetrics <- tibble(
  time = 1:length(deathkza$kz),
  ddper100k = Mobility_county$ddper100k,
  dcper100k = Mobility_county$dcper100k,
  d_kz = deathkza$kz,
  d_kz_der = ave(d_kz, FUN = function(x) c(0, diff(x))),
  c_kz = casekza$kz,
  c_kz_der = ave(c_kz, FUN = function(x) c(0, diff(x))),
)


# plot KZ smoother with second derivative ---------------------------------

# case plot
ggplot(KZmetrics, aes(x = time)) + 
  geom_line(aes(y = c_kz/max(abs(c_kz))), color = "deepskyblue", size = 1.5) +
  geom_line(aes(y = c_kz_der/max(abs(c_kz_der))), color = "red", size = 1.5) +
  ggtitle("Standardized KZ Smoothed Case Rate and Acceleration")

# death plot
ggplot(KZmetrics, aes(x = time)) + 
  geom_line(aes(y = d_kz/max(abs(d_kz))), color = "deepskyblue", size = 1.5) +
  geom_line(aes(y = d_kz_der/max(abs(d_kz_der))), color = "red", size = 1.5) +
  ggtitle("Standardized KZ Smoothed Death Rate and Acceleration")



COVID_stmobil <- COVID_stmobil %>%
  mutate(logcases = log(((dcases + 1)/Population)*100000),
         logdeaths = log(((ddeaths + 1)/Population)*100000),
         weekfac = factor(weekfac, ordered = F),
         weekfac = relevel(weekfac, ref = "Sunday"),
         weeknd = ifelse(weekfac == "Saturday" | weekfac == "Sunday", "weekend", "weekday"),
         weeknd = factor(weeknd, levels = c("weekend", "weekday"))) %>%
  select(-dcases, -ddeaths, -cper100k, -dper100k)


