## COVID moving averages experiment script ##
## Kees Schipper ##
## Advanced data analysis ##
## 9/12/2020 ##

source('COVID_JHU_DATA.R')

## To make the filter that Ryan is talking about into a function, we need the following:
### specify a value 'n' that is to be the width of the moving average
#### need to specify a vector of n values, and have a mutate function that clearly shows the avg width
### have a date input for when you want your moving averages to start-->easy use of dplyr::filter()
### need to propagate filter values at the edge of your filter over the NA values generated
#### for example, when we have a width of 50, our first 24 values will be NA. Use the 25th value as
#### your values from 1-24

COVID_Mass <- COVID_states %>%
  filter(State == 'Massachusetts') %>%
  group_by(County) %>%
  mutate(filter_3 = stats::filter(dcases, rep(1/3, 3), sides = 2),
         filter_5 = stats::filter(dcases, rep(1/5, 5), sides = 2),
         filter_7 = stats::filter(dcases, rep(1/7, 7), sides = 2),
         filter_9 = stats::filter(dcases, rep(1/9, 9), sides = 2),
         mean_filter = (filter_3 + filter_5 + filter_7 + filter_9)/4)

plot(length(COVID_Mass), COVID_Mass$filter_9)
