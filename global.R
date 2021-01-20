# TO DO - link csv direct from web and filter  most recent date
# why crude rates for region from IZ and CA dataset different... Check!
# detail where population taken from
# Make region choice>?  default East Lothian


library(tidyverse)
library(plotly)
library(lubridate)
library(sf)
library(ggiraph)
library(readxl)

#Pull in covid data
locality_data <- read_csv("data/trend_iz.csv", 
                          col_types = cols(CrudeRate7DayPositive = col_character(), Positive7Day = col_integer())) %>% 
  mutate(Date = ymd(as.character(Date))) 

el_data <- locality_data %>% 
  select(-c(Positive7DayQF, CrudeRate7DayPositiveQF)) %>% 
  filter(CAName == "East Lothian") %>% 
  rename(InterZone = IntZone) %>% 
  mutate(CrudeRate7DayPositive = ifelse(is.na(CrudeRate7DayPositive), 0, CrudeRate7DayPositive)) %>% 
  mutate(multiplier = 100000/Population) %>% 
  mutate(Positive7Day = ifelse(is.na(Positive7Day), 0, Positive7Day)) %>% 
  mutate(real_rate_per_OT = round(multiplier*Positive7Day))

#Convert crude rate to ordered factor
el_data$CrudeRate7DayPositive<- factor(el_data$CrudeRate7DayPositive, levels = c(0, "1 to 49", "50 to 99", "100 to 199", "200 to 399", "400+"))
unique(el_data$CrudeRate7DayPositive)
class(el_data$CrudeRate7DayPositive)

el_data %>% write_csv("data_check.csv")
# Bring in Spatial Data and join Covid data
el_map <- st_read("data/shape_files/SG_IntermediateZoneBdry_2011/") 
el_map <- left_join(el_map, el_data) %>% 
  select(-c(Name, TotPop2011, ResPop2011, HHCnt2011, StdAreaHa, StdAreaKm2)) %>% 
  filter(CAName == "East Lothian") %>% 
  arrange(IntZoneName, Date) %>% 
  #dplyr lag function to confirm data from 7 days previous - needed to arrange data so picking up correct area
  mutate(wow = Positive7Day - lag(Positive7Day,7)) %>% 
  mutate(change = case_when(
    wow < 0 ~ "less than 7 days previous",
    wow > 0 ~ "more than 7 days previous",
    TRUE ~ "No change on 7 days previous")
  )   %>% 
  filter(Date == "2021-01-01") 


tooltip_css <- "background-color:#9c9a98;"

#---------------PLOT 2 ---------------------

national_data  <- read_csv("data/trend_ca.csv") %>% 
  mutate(Date = ymd(as.character(Date)))



county_cumulative <- national_data %>%
  filter(CAName == "East Lothian") %>% 
  select(-c(DailyPositive,DailyDeaths, CA, CAName, TotalTests)) %>% 
  filter(Date == "2021-01-01") %>% 
  select(-Date) %>% 
  pivot_longer(cols =c(CumulativePositive, CumulativeDeaths),
               names_to = "Stats",
               values_to = "CumulativeTotals") %>% 
  mutate(Stats =factor(Stats, levels = c("CumulativePositive", "CumulativeDeaths"))) 





