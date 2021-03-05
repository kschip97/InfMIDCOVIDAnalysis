## GET_MDPH_data.R ##
## Created by: Kees Schipper ##
## Date Created: 2020-01-19 ##
## Date last updated: 2020-01-10 ##

rm(list = ls())
dev.off()

# install libraries -------------------------------------------------------

library(httr)
library(tidyverse)
library(readxl)
library(data.table)

# set url base path to get data from MDPH website -------------------------

## example URL:
## https://www.mass.gov/doc/covid-19-raw-data-january-19-2021/download

url.root <- "https://www.mass.gov/doc/covid-19-raw-data-"

## dates are structured as 
## mmmm-dd-yyyy/download ##

months <- tolower(month.name)
month_data <- data.frame(name = months,
                         numdays = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
                         monthnum = c("01", "02", "03", "04", "05", "06",
                                      "07", "08", "09", "10", "11", "12"))
year <- c(2020, 2021)
p <- NULL
i <- NULL
j <- NULL

url.list <- vector(mode = "character", length = as.numeric(Sys.Date() - as.Date("2020-01-01")))
date.list <- vector(mode = "character", length = as.numeric(Sys.Date() - as.Date("2020-01-01")))

for (p in seq_along(year)){
  for (i in 1:nrow(month_data)){
    for (j in 1:month_data$numdays[i]){
      
       if (as.Date(paste0(year[p], "-", month_data$monthnum[i], "-", j)) > Sys.Date()){
         break
       }
      download_url <- paste0(url.root, month_data$name[i], "-", j, "-", year[p], "/download")
      url.list[[(p-1)*365 + (sum(month_data$numdays[1:i])-31) + j]] <- download_url
      
      {date.list[[(p-1)*365 + (sum(month_data$numdays[1:i])-31) + j]] <- 
          paste0(year[p], "-", month_data$name[i], "-", j)}
      
      print((p-1)*365 + (sum(month_data$numdays[1:i])-31) + j)
      
    }
  }
}

# install.packages("stringi")
library(stringi)

url.list <- url.list[125:length(url.list)]
url.list <- stri_remove_empty(url.list)

date.list <- date.list[125:length(date.list)]
date.list <- stri_remove_empty(date.list)

## get csv files
# file.list <- vector(mode = "list", length = length(url.list))

data.list <- vector(mode = "list", length = length(url.list))




timer <- proc.time()
for (i in 1:length(url.list)){
  cat("donwloading file ", i, " of ", length(url.list), "\n")
  temp <- tempfile()
  
  
  try(download.file(url = url.list[[i]], 
                temp,
                mode = "wb"))
  
  
  if (i < 239){
  # destfile = paste0("Raw Data/MADPH COVID Downloads/testfile-", i, ".zip")
  
  # deaths <- read_csv(unz(temp, "DeathsReported.csv"))
  # cases <- read_csv(unz(temp, "Cases.csv"))
  County <- try(read_csv(unz(temp, "County.csv")) %>%
    arrange(County, Date))
  
  Testing <- try(read_csv(unz(temp, "Testing2.csv")))
  
  if (is.tibble(County) & is.tibble(Testing)){
  
  County <- County %>%
    left_join(Testing, by = "Date")
  
  }
  
  if (!is_tibble(County)){
    County <- NULL
  }
  # tempdata <- left_join(cases, deaths, by = "Date")
  
  data.list[[i]] <- County
  
  unlink(temp)
  Sys.sleep(4)
  } else if (i >= 239){
    County <- try(read_xlsx(temp, sheet = "County_Daily") %>%
      arrange(County, Date))
    
    Testing <- try(read_xlsx(temp, sheet = "Testing2 (Report Date)"))
    
    if (is.tibble(County) & is.tibble(Testing)){
      
      County <- County %>%
        left_join(Testing, by = "Date")
      
    }
    
    if (!is_tibble(County)){
      County <- NULL
    }
    
    data.list[[i]] <- County
    unlink(temp)
    Sys.sleep(3)
  }


}
file.remove(temp)
proc.time()-timer

save(data.list, file = "Raw Data/MA DPH COVID Dashboard Raw/MDPH_xlsx_data_list.RData")

if (exists("data.list")){
  
  data.list.2clean <- data.list
  
} else {
  load("Raw Data/MA DPH COVID Dashboard Raw/MDPH_xlsx_data_list.RData")
  data.list.2clean <- data.list
}


# remove null values of list
data.list.2clean[sapply(data.list.2clean, is.null)] <- NULL

# create list of column names for each data frame
col_names <- sapply(data.list.2clean, names)

# testing how all works
# all(col_names[[1]] %in% colnames(data.list.2clean[[1]]))

# examining all unique variable names in the colnames list
all_colnames <- col_names %>%
  reduce(union)

# colnames to keep in early vs. later data
early_keep <- c("Date", "County", "Count", "Deaths", "Total")
late_keep <- c("Date", "County", "Total Confirmed Cases", "Total Probable and Confirmed Deaths",
               "Molecular All Tests Total", "Molecular Total")

all_keep <- union(early_keep, late_keep)

# select relevant columns from each data frame in list --------------------

## inputs: data set, whether to remove or add the columns, and what columns to remove (rep as ...)
sel_cols <- function(.data, rm, county, ...){
  
  if (rm == TRUE){
  
  output <- .data %>%
    select(-any_of(...)) %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    arrange(Date, County)
  
} 

  if (rm == FALSE){
  
  output <- .data %>%
    select(any_of(...)) %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    arrange(Date, County)
  
} else {
  
  output <- .data
  
}
  return(output)
  
}

# test case
# d_245_rm <- sel_cols(d_245, rm = TRUE, "14-day average turnaround time (only updated on Wednesdays)")

data.list.2clean <- map(data.list.2clean, sel_cols, rm = FALSE, county, all_keep)

# check that names have been reduced
sapply(data.list.2clean, names) %>%
  reduce(union)

# Select county before combining the data in MA --------------------------



filter_county <- function(.data, county){
  
  output <- .data %>%
    filter(County %in% county)
  
  return(output)
  
}

county <- "Middlesex"

data.list.2clean <- lapply(data.list.2clean, filter_county, county)


# combine county-level data -----------------------------------------------


combined_data <- rbindlist(data.list.2clean, use.names = TRUE, fill = TRUE)

# keep only distinct observations
final_data <- distinct(combined_data) %>%
  arrange(Date) %>%
  rename(Cases = Count,
         Tests = Total)

# find which dates are duplicated
dupped_dates <- which(duplicated(final_data$Date))
# conf_data <- final_data[c(49:135, 311, 352),]

# multiple steps: 
final_data_cln <- final_data %>%
  mutate(Tests = ifelse(is.na(Tests),
                       `Molecular Total`, Tests),
         Cases = ifelse(is.na(Cases),
                        `Total Confirmed Cases`, Cases),
         Deaths = ifelse(is.na(Deaths),
                         `Total Probable and Confirmed Deaths`, Deaths)) %>%
  filter((!is.na(Deaths) | !is.na(Tests)) | Date < "2020-04-01") %>%
  mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%
  select(Date:Tests) %>%
  mutate(Tests = ifelse(is.na(Tests), 0, Tests))
  

# need to make some data for August 12-18th to insert in final_data_cln
miss_dat <- data.frame(Date = seq(as.Date("2020-08-12"), by = "day", length.out = 7),
                       County = rep(final_data_cln$County[1], 7),
                       Cases = floor(seq(final_data_cln$Cases[final_data_cln$Date == "2020-08-11"], 
                                         final_data_cln$Cases[final_data_cln$Date == "2020-08-19"], 
                                         length.out = 9)[2:8]),
                       Deaths = floor(seq(final_data_cln$Deaths[final_data_cln$Date == "2020-08-11"], 
                                          final_data_cln$Deaths[final_data_cln$Date == "2020-08-19"], 
                                          length.out = 9)[2:8]),
                       Tests = floor(seq(final_data_cln$Tests[final_data_cln$Date == "2020-08-11"], 
                                         final_data_cln$Tests[final_data_cln$Date == "2020-08-19"], 
                                         length.out = 9)[2:8]))

comp_data <- rbind(final_data_cln, miss_dat, use.names = TRUE, fill = TRUE) %>%
  arrange(Date)

comp_data <- comp_data[!duplicated(comp_data),]

miss_dates <- data.frame(Date = as.Date(c("2021-01-01", "2020-12-25", "2020-11-26")),
                         County = rep("Middlesex", 3),
                         Cases = rep(NA, 3),
                         Deaths = rep(NA, 3),
                         Tests = rep(NA, 3))

fill_data <- rbind(comp_data, miss_dates, use.names = TRUE, fill = TRUE) %>%
  arrange(Date) %>%
  mutate(Cases = na.interp(Cases, linear = TRUE),
         Deaths = na.interp(Deaths, linear = TRUE),
         Tests = na.interp(Tests, linear = TRUE))

which(duplicated(fill_data$Date))
fill_data_nodups <- fill_data[-315,] # not robust...

# Create data for changing rates of cases and deaths
rates <- fill_data_nodups %>%
  mutate(dtests = ave(Tests, County, FUN = function(x) c(0, diff(x))),
         dcases = ave(Cases, County, FUN = function(x) c(0, diff(x))),
         ddeaths = ave(Deaths, County, FUN = function(x) c(0, diff(x))))


plot(rates$Date, rates$dtests, type = "l", col = "green4", main = "Change in Tests per Day")
# text(x = 30, y = 30000, paste0("Indicies where dtests < 0: \n", list(which(rates$dtests < 0))))

plot(rates$Date, rates$dcases, type = "l", col = "blue4", main = "Change in Cases per Day")
# text(x = 150, y = 1500, paste0("Indicies where dcases < 0: \n", list(which(rates$dcases < 0))),
     # col = "black")

plot(rates$Date, rates$ddeaths, type = "l", col = "firebrick3", main = "Change in Deaths")
# text(x = 200, y = 60, paste0("Indicies where dcases < 0: \n", list(which(rates$ddeaths < 0))),
     # col = "black")


write_csv(rates, paste0("cleaned data/from_MDPH_web_Middlesex_data_2021-01-28.csv"))

## Notes: Probable case definition changed on September 9th, 2020. May affect data integrity
## messed up links:
# https://www.mass.gov/doc/covid-19-raw-data-january-3-202/download

#TODO: need to adjust for negative values in the COVID change in cases, deaths, and testing data
# need to take into account the changing nature of the data over time.
