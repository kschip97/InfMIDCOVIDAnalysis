## COVID_maps.R ##
# Author: Kees Schipper
# Date Created: 8/29/2020
# Last Update : 8/29/2020
# Notes: This program is to be used to create a standardized map template, and 
#        functions which can be used to plot states, countries, and counties by date
#        I'm attempting to make plots in plotly

#source data from data cleaning program
source(file = 'COVID_JHU_DATA.R')

{if(!require(maps)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  require(maps)}
{if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
  require(RColorBrewer)}
{if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
  require(leaflet)}
{if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
  require(plotly)}
{if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
  require(geojsonio)}
{if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
  require(shiny)}
{if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
  require(shinyWidgets)}
{if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
  require(shinydashboard)}
{if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
  require(shinythemes)}
{if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
  require(sf)}
{if(!require(htmltools)) install.packages("htmltools", repos = "http://cran.us.r-project.org")
  require(htmltools)}
{if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
  require(viridis)}
# {if(!require(usmap)) install.packages("usmap", repos = "http://cran.us.r-project.org")
#   require(usmap)}


# Create objects for county, state, and world maps for future use

countymap <- maps::map(database = 'county', projection = "albers", parameters = c(45, 60), plot = F, fill = TRUE) %>%
  st_as_sf() %>%
  separate(ID, into = c("State", "County"), sep = ',') %>%
  mutate(State = str_to_title(State),
         County = str_to_title(County),
         County = str_replace_all(County, " ", ""))

# add FIPS data to countymap
data(county.fips)
FIPS <- county.fips
FIPS.cleaned <- FIPS %>%
  separate(polyname, into = c("State", "County"), sep = ",") %>%
  mutate(State = str_to_title(State),
         County = str_to_title(County),
         County = str_replace_all(County, " ", ""),
         County = trimws(County),
         State = trimws(State))

new_countymap <- merge(countymap, FIPS.cleaned, by = c("State", "County"))

statemap <- maps::map(database = 'state', plot = F, fill = TRUE)
worldmap <- maps::map(database = "world", plot = F, fill = TRUE)

# countymap[["names"]] = str_to_title(map(str_split(countymap[["names"]], ',', simplify = F), 2))




# create base leaflet maps and store as an object for standardization across maps

single_date <- function(input_df, date, dateform = "%Y-%m-%d", tag,
                        leaflet = F, ggplot = T){
  date <- as.Date(date, format = dateform)

  temp_df <- input_df %>%
    filter(Date == date) %>%
    mutate(County = str_replace_all(County, " ", ""))

  plot_df <- merge(temp_df, countymap, by = c("State", "County")) %>%
    filter(!st_is_empty(geom)) %>%
    st_as_sf()

}

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

mar25 <- single_date("2020-03-25", input_df = COVID_states, tag = "cases", ggplot = T) %>%
  mutate(dcper100k = ifelse(dcper100k < 0, 0, dcper100k))

# examine quantiles of certain outbreak statistics
(quant <- quantile(mar25$dcper100k, probs = seq(0, 1, 0.05), na.rm = TRUE))
quant <- quant[quant>0]
brks <- c()
brks <- c(0, round(quant, digits = 2))

labels <- c()
for (i in 1:length(brks)){
  labels <- c(labels, round(brks[i + 1], 1))
}
labels <- labels[1:length(labels)-1]

mar25$brks <- cut(mar25$dcper100k, breaks = brks, labels = labels, include.lowest = TRUE)
(brks_scale <- levels(mar25$brks))
(labels_scale <- rev(brks_scale))

ggplot(data = mar25, aes(fill = brks)) + geom_sf(aes(fill = brks)) +
  labs(x = NULL,
       y = NULL,
       title = "COVID-19 in the U.S.",
       subtitle = "Mapping Change in Cases Data",
       caption = mar25$Date[1]) +
  theme_map() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = rev(magma(8)[2:7]),
                     breaks = rev(brks_scale),
                     name = "Change in Cases per 100k population",
                     drop = FALSE,
                     labels = labels_scale,
                     guide = guide_legend(
                       direction = "horizontal",
                       keyheight = unit(2, units = "mm"),
                       keywidth = unit(70 / length(labels), units = "mm"),
                       title.position = 'top',
                       # I shift the labels around, the should be placed 
                       # exactly at the right end of each legend key
                       title.hjust = 0.5,
                       label.hjust = 1,
                       nrow = 1,
                       byrow = T,
                       # also the guide needs to be reversed
                       reverse = T,
                       label.position = "bottom"))

missingmp <- mar25 %>%
  filter(is.na(brks))

# TODO: figure out why there are missing counties on the map. It probably has to do with the 
# fact that there are some negative 
  

# leaflet(data = countymap) %>%
#   addTiles() %>%
#   addPolygons(color = "black", fillColor = viridis(n = 8, begin = 0, end = 1),
#               fillOpacity = .7, stroke = FALSE,
#               popup = paste0(countymap$county),
#               popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE))
