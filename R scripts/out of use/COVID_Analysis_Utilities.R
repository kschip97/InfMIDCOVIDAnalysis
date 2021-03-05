## Daysumm_and_weights_function.R ##
## Author: Kees Schipper ##
## Created on: 10/20/2020 ##
## Last Edited: 10/20/2020 ##

## Notes: This function will calculate summary statistics given a state, county, and 
## output type of "summary." If you want to add median-based weights to your dataset,
## keep the county and state argument the same, but use the "weights" argument

daysumm_county <- function(state, county, output = c("summary", "weights")){
  if (!is.character(state) | !is.character(county)){
    stop("state and county args must be character")
  }
  Mobility_county <- COVID_stmobil %>%
    filter(State == state & County == county)
  
  Mob_sum_day <- Mobility_county %>%
    select(dcper100k:ddper100k, retail_rec:residential, logcases:logdeaths, weekfac) %>%
    pivot_longer(cols = c("dcper100k", "ddper100k", "logcases", "logdeaths", "groc_pharm", "parks", "residential",
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
              nobs = sum(!is.na(values)),
              County = county,
              State = state)
  
  if (output == "summary"){
    return(Mob_sum_day)
  } else if (output == "weights") {
    
    interp_weights <- tibble(
      variable = Mob_sum_day[["variables"]],
      weekfac  = Mob_sum_day[["weekfac"]],
      medians = Mob_sum_day[["Median"]]
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
    
    ########## Changed COVID_states to COVID_stmobil-->Seems to work but keep an eye out
    COVID_speccounty <- COVID_stmobil %>%
      filter(State == state & County == county) %>%
      mutate(
        time = 1:nrow(COVID_county),
        time = as.numeric(time)
      )
    
    COVID_speccounty <- left_join(COVID_speccounty, interp_weights, by = c("weekfac")) %>%
      mutate(
        time = 1:nrow(COVID_speccounty),
        time = as.numeric(time)
      )
    return(COVID_speccounty)
    
  }
}


# Function for cross-correlating time series curves with COVID data. has the option to 
# make a cross-correlation with a kolomogrov-zurbenko filter with window m and number of
# passes k
compare_ccf <- function(state, county1, intercounty, county2, var, compvar = NULL, m = NULL, k = NULL, 
                        lag = 30, plot = T, lagunits = NULL){
  
  refcounty <- daysumm_county(state = state, county = county1, output = "weights") # x[t]
  if (sum(refcounty[[var]]) > 0){
    refcounty[[var]][is.na(refcounty[[var]])] <- 0
  }
  
  if (intercounty == T) {
    corrcounty <- daysumm_county(state = state, county = county2, output = "weights") # y[t+k]
    if(sum(corrcounty[[var]]) > 0){
      corrcounty[[var]][is.na(corrcounty[[var]])] <- 0
    }
  

    corr <- ccf(corrcounty[[var]], refcounty[[var]], lag.max = lag, plot = F, na.action = na.pass)
    
    reference <- list(State = state, County = county1, values = refcounty[[var]])
    correlated <- list(State = state, County = county2, values = corrcounty[[var]])
    
    MaxRawCorr <- corr$acf[which.max(abs(corr$acf))]
    RawCorrLag <- corr$lag[which.max(abs(corr$acf))]
    
    # Filter data with kz function for further comparison
    kz1 <- kz(refcounty[[var]], m, k)
    kz2 <- kz(corrcounty[[var]], m, k)
    kzcorr <- ccf(kz2, kz1, lag.max = lag, plot = F)
    
    kzrefcounty <- list(State = state, County = county1, kz_filter = kz1) # y[t]
    kzcorrcounty <- list(State = state, County = county2, kz_filter = kz2) # x[t+k]
    
    MaxKzCorr <- kzcorr$acf[which.max(abs(kzcorr$acf))]
    KzCorrLag <- kzcorr$lag[which.max(abs(kzcorr$acf))]
    
    print(paste0("With Raw data, max correlation of ", county2, " with ", county1, " is ", MaxRawCorr))
    print(paste0("With Raw Data ", county2, " lags ", county1, " by ", RawCorrLag, " ", lagunits))
    print(paste0("With KZ filter, max correlation of ", county2, " with ", county1, " is ", MaxKzCorr))
    print(paste0("With KZ filter ", county2, " lags ", county1, " by ", KzCorrLag, " ", lagunits))
    
    if (plot == T) {
      
      par(mfrow = c(2, 3))
      plot(refcounty[[var]], main = paste0(county1, " ", var, " timeseries", xlab = "time"),
                                           ylab = paste0(var), type = "l")
      plot(corrcounty[[var]], main = paste0(county2, " ", var, " timeseries", xlab = "time"),
                                            ylab = paste0(var), type = "l")
      plot(corr, ci = 0.99, type = "l", main = paste0("Raw Data ", county1, " y[t] corr. ", county2, " x[t+k]"),
           xlab = "lag (k)", ylab = "Cross Correlation", ci.col = "blue")
      abline(v = corr$lag[which.max(abs(corr$acf))], col = "red")
      
      plot(kz1, main = paste0("Filtered ", county1, " ", var, " timeseries"), xlab = "time",
           ylab = paste0("Filtered ", var), type = "l")
      plot(kz2, main = paste0("Filtered ", county2, " ", var, " timeseries"), xlab = "time",
           ylab = paste0("Filtered ", var), type = "l")
      plot(kzcorr, ci = 0.99, type = "l", main = paste0("Raw Data: ", county1, " y[t] corr. ", county2, " x[t+k]"),
           xlab = "lag(k)", ylab = "Cross Correlation", ci.col = "blue")
      abline(v = kzcorr$lag[which.max(abs(kzcorr$acf))], col = "red")
    }
    
    return(list(refdata = refcounty, cordata = corrcounty, ref_county = kzrefcounty,
                RefSmoothTS = ts(kz1), CorrSmoothTS = ts(kz2),
                corr_county = kzcorrcounty, raw_corr = corr,
                MaxRawCorr = MaxRawCorr, RawCorrLag = RawCorrLag, kz_corr = kzcorr,
                MaxKzCorr = MaxKzCorr, KzCorrLag = KzCorrLag))
    
    } else if (intercounty == F) {
      
      refcounty <- daysumm_county(state = state, county = county1, output = "weights")
      
      if (sum(is.na(refcounty[[compvar]])) > 0){
        refcounty[[compvar]][is.na(refcounty[[compvar]])] <- 0
      }
      
      corr <- ccf(refcounty[[compvar]], refcounty[[var]], lag.max = lag, plot = F, na.action = na.pass)
      
      reference <- list(State = state, County = county1, values = refcounty[[var]])
      correlated <- list(State = state, County = county1, values = refcounty[[compvar]])
      
      MaxRawCorr <- corr$acf[which.max(abs(corr$acf))]
      RawCorrLag <- corr$lag[which.max(abs(corr$acf))]
      
      # run same correlations with KZ filters
      kz1 <- kz(refcounty[[var]], m, k)
      kz2 <- kz(refcounty[[compvar]], m, k)
      kzcorr <- ccf(kz2, kz1, lag.max = lag, plot = F)
      
      kzrefvar <- list(State = state, County = county1, kz_filter = kz1) # y[t]
      kzcorrvar <- list(State = state, County = county1, kz_filter = kz2) # x[t+k]
      
      MaxKzCorr <- kzcorr$acf[which.max(abs(kzcorr$acf))]
      KzCorrLag <- kzcorr$lag[which.max(abs(kzcorr$acf))]
      
      print(paste0("With Raw data, max correlation of ", compvar, " with ", var, " is ", MaxRawCorr))
      print(paste0("With Raw Data ", compvar, " lags ", var, " by ", RawCorrLag, " ", lagunits))
      print(paste0("With KZ filter, max correlation of ", compvar, " with ", var, " is ", MaxKzCorr))
      print(paste0("With KZ filter ", compvar, " lags ", var, " by ", KzCorrLag, " ", lagunits))
    
    if (plot == T){
      
      par(mfrow = c(2, 3))
      plot(refcounty[[var]], type = "l", xlab = "time", ylab = paste0(var), main = paste0("Raw ", var, " timeseries"))
      plot(refcounty[[compvar]], type = "l", xlab = "time", ylab = paste0(compvar), main = paste0("Raw ", var, " timeseries"))
      plot(corr, ci = 0.99, type = "l", main = paste0("Raw Data ", var, " y[t] corr. ", compvar, " x[t+k]"),
           xlab = "lag (k)", ylab = "Cross Correlation", ci.col = "blue")
      abline(v = corr$lag[which.max(abs(corr$acf))], col = "red")
      
      plot(kz1, type = "l", xlab = "time", ylab = paste0("filtered ", var), main = paste0("filtered ", var, " timeseries"))
      plot(kz2, type = "l", xlab = "time", ylab = paste0("filtered ", compvar), main = paste0("filtered ", compvar, " timeseries"))
      plot(kzcorr, ci = 0.99, type = "l", main = paste0("Raw Data: ", var, " y[t] corr. ", compvar, " x[t+k]"),
           xlab = "lag(k)", ylab = "Cross Correlation", ci.col = "blue")
      abline(v = kzcorr$lag[which.max(abs(kzcorr$acf))], col = "red")
    }
      return(list(data = refcounty, ref_var = kzrefvar,
                  RefSmoothTS = kz1, CorrSmoothTS = kz2,
                  corr_var = compvar, raw_corr = corr,
                  MaxRawCorr = MaxRawCorr, RawCorrLag = RawCorrLag, kz_corr = kzcorr,
                  MaxKzCorr = MaxKzCorr, KzCorrLag = KzCorrLag))
    }
  }


rectobj <- function(df, color, alpha){
  geom_rect(data = df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = color, alpha = alpha)
}

Make_summary_table <- function(input_df, vars, spanlab, title, subtitle, footnotes){
  
  if (footnotes == "cases"){
    footnotes = c("March 5 - April 18", "April 19 -June 9",
                  "June 10 - August 8", "August 9 - November 18")
  } else if (footnotes == "deaths"){
    footnotes = c("March 27 - April 28", "April 29 -June 9",
                  "June 10 - August 13", "August 14 - November 18")
  }
  
  output <- input_df %>%
    gt() %>%
    cols_label(
      variables = "",
      T2 = "T1",
      T3 = "T2", 
      T4 = "T3", 
      T5 = "T4"
    ) %>%
    row_group_order(
      groups = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything())
      )
    ) %>%
    opt_table_lines(extent = "default") %>%
    tab_options(
      row_group.padding = 1,
      data_row.padding = 1,
      footnotes.padding = 2,
      table.font.size = 10,
      column_labels.border.top.color = "white",
      column_labels.border.top.width = px(3),
      column_labels.border.bottom.color = "black",
      table_body.hlines.color = "white",
      table.border.bottom.color = "black",
      table.border.bottom.width = px(3),
      row_group.background.color = "#CCCCCC"
    ) %>%
    tab_spanner(
      label = spanlab,
      columns = 3:6
    ) %>%
    tab_header(
      title = md(paste0("**",title,"**")),
      subtitle = html(paste0("<em>",subtitle,"</em>"))
    ) %>%
    tab_footnote(
      footnote = footnotes[1],
      locations = cells_column_labels(2)
    ) %>%
    tab_footnote(
      footnote = footnotes[2],
      locations = cells_column_labels(3)
    ) %>%
    tab_footnote(
      footnote = footnotes[3],
      locations = cells_column_labels(4)
    ) %>%
    tab_footnote(
      footnote = footnotes[4],
      locations = cells_column_labels(5)
    ) #%>%
  # tab_footnote(
  #   footnote = footnotes[5],
  #   locations = cells_column_labels(6)
  # )
  
  return(output)
}



