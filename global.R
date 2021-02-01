
# why crude rates for region from IZ and CA dataset different... eg 21/01 cases. Neighbourhood 100, local authority: 111

library(tidyverse)
library(plotly)
library(lubridate)
library(sf)
library(ggiraph)
library(readxl)
library(shiny)

# 1 Pull in locality data data


locality_data <- read_csv("data/trend_iz.csv", 
                          col_types = cols(CrudeRate7DayPositive = col_character(), Positive7Day = col_integer())) %>% 
  mutate(Date = ymd(as.character(Date))) 

# 2 Pull in local authority data data
la_data <- read_csv("data/trend_ca.csv") %>% 
  select(-c(DailyNegative, CrudeRate7DayPositive)) %>% 
  mutate(Date = ymd(as.character(Date)))

#3 Scottish Cumulative Dates - not using
national_total_data <- read_csv("data/daily_cuml_scot.csv") %>%
  mutate(Date = ymd(as.character(Date)))

#4 Hospital Data
hospitalisation_data <- read_excel("data/trends.xlsx", sheet = "Table 2 - Hospital Care", skip = 2) 
#5 Vax Data
vax_data <- read_excel("data/trends.xlsx", sheet = "Table 10a - Vaccinations", skip = 2) %>% 
  rename("FirstDose" = "Number of people who have received the first dose of the Covid vaccination", "SecondDose" = "Number of people who have received the second dose of the Covid vaccination" )
#locality data is 3 days behind - so to make sure - using same date

app_date <- la_data %>% 
  select(Date) %>% 
  arrange(desc(Date)) %>% 
  head(1) 
#Make sure locality data to app date
# la_data <- la_data %>% 
#   filter(Date <= app_date)


head_date <- app_date$Date %>% 
  format('%d/%m/%y')

Head_title_date <- today() %>% 
  format('%d %B %Y')

local_date <- locality_data %>% 
  select(Date) %>% 
  arrange(desc(Date))  %>% 
  head(1)

local_box_date <- local_date$Date  %>% 
  format('%d/%m/%y')

local_date_title <- local_date$Date %>% 
  format('%d %B %Y')

head_date_tile <- local_date_title
la_regions <- unique(locality_data$CAName)


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
  )  %>% 
  filter(Date == local_date)


tooltip_css <- "background-color:#d9d9d9; 
                color: #000000;
                font-weight: 500;
                padding-left: 5px;"

#---------------PLOT 2 ---------------------




county_cumulative <- la_data %>%
  filter(CAName == "East Lothian") %>% 
  select(-c(DailyPositive,DailyDeaths, CA, CAName, TotalTests)) %>% 
  filter(Date == local_date) %>% 
  select(-Date) %>% 
  pivot_longer(cols =c(CumulativePositive, CumulativeDeaths),
               names_to = "Stats",
               values_to = "CumulativeTotals") %>% 
  mutate(Stats =factor(Stats, levels = c("CumulativePositive", "CumulativeDeaths"))) 


#------------------PLOT 3

totals_data <- la_data  %>% 
  select(-c(CumulativeNegative , CrudeRateNegative, PositiveTests, PositivePercentage7Day, PositivePercentage, TotalPillar1, TotalPillar2, CrudeRateDeaths, CrudeRatePositive))

colnames(locality_data)
population <- locality_data %>% 
  filter(Date == local_date) %>% 
  group_by(CAName) %>% 
  summarise(population = sum(Population))

scot_population <- sum(population$population)

total_tests <- sum(totals_data$TotalTests)
scot_pos <- sum(totals_data$DailyPositive)
scot_deaths <- sum(totals_data$DailyDeaths)

region_total_crude <- totals_data %>%
  filter(CAName == "East Lothian") %>% 
  filter(Date <= local_date) %>% 
  arrange(desc(Date)) %>% 
  slice_max(Date, n = 7) %>% 
  mutate(CAName = as.character(CAName)) 

region_total_crude$CAName = as.character(region_total_crude$CAName)
region_total_crude <- left_join(region_total_crude, population, by ="CAName")
region_multiplier <- 100000/head(as.numeric(region_total_crude$population),1)

colnames(totals_data)
scot_total_crude <- totals_data %>%
  group_by(Date) %>% 
  filter(Date <= local_date)  %>% 
  summarise(DailyPositive = sum(DailyPositive), DailyDeaths = sum(DailyDeaths), TotalTests = sum(TotalTests)) %>% 
  arrange(desc(Date)) %>% 
  slice_max(Date, n = 7) 

scot_multiplier <- 100000/head(as.numeric(scot_population),1)

scot_total <- scot_total_crude %>% 
  slice_max(Date, n = 1) 

el_crude_today <- sum(region_total_crude$DailyPositive) *region_multiplier
scot_crude_today <- sum(scot_total_crude$DailyPositive) *scot_multiplier
total_tests_today = sum(la_data$TotalTests)
#compare with below
scot_pos_today = sum(la_data$DailyPositive)
# scot_deaths_today = sum(la_data$DailyDeaths)

crude_check <- locality_data %>% 
  select(-c(Positive7DayQF, CrudeRate7DayPositiveQF)) %>% 
  filter(CAName == "East Lothian") %>%
  filter(Date ==app_date) %>% 
  rename(InterZone = IntZone) %>% 
  mutate(CrudeRate7DayPositive = ifelse(is.na(CrudeRate7DayPositive), 0, CrudeRate7DayPositive)) %>% 
  mutate(multiplier = 100000/Population) %>% 
  mutate(Positive7Day = ifelse(is.na(Positive7Day), 0, Positive7Day)) %>% 
  mutate(real_rate_per_OT = round(multiplier*Positive7Day))
sum(crude_check$Positive7Day)*region_multiplier

#East Lothian Crude Rate 98 as opposed to 
#---- TEST FINISH!!!! ____

scot_test_data <- la_data %>% 
  select(Date, TotalTests)  %>% 
  group_by(Date) %>% 
  summarize (TotalTests = sum(TotalTests))



# national_cuml_data <- left_join(national_cuml_data, scot_test_data, by ="Date")


CovidTime <- totals_data %>% 
  select(-c(CA, CumulativeDeaths)) %>% 
  rename(DailyCases = DailyPositive, CumulativeCases = CumulativePositive, Region = CAName, Deaths = DailyDeaths)

colnames(CovidTime)
scot_cuml_time <- CovidTime %>% 
  group_by(Date) %>% 
  summarise(DailyCases = sum(DailyCases), Deaths = sum(Deaths), TotalTests = sum(TotalTests), CumulativeCases = sum(CumulativeCases)) %>% 
  mutate(Region = "Scotland")

# Add Data daily and cumulative cases, deaths - Scotland and Local Authority Regions
CovidTime <- rbind(CovidTime, scot_cuml_time) %>%
  filter(Region == "East Lothian" | Region == "Scotland")

  
  
CovidTimeLine <-  CovidTime %>% 
  select(-CumulativeCases) %>% 
  mutate(DailyCases7 = round(zoo::rollmean(DailyCases, k = 7, fill = NA),2),
         DailyDeaths7 = round(zoo::rollmean(Deaths, k = 7, fill = NA),2),
         DailyTests7 = round(zoo::rollmean(TotalTests, k = 7, fill = NA),2)) %>% 
  rename(Cases = DailyCases, Tests = TotalTests) %>% 
  pivot_longer(cols =c(Cases, Deaths, Tests),
               names_to = "Stats",
               values_to = "Numbers") %>% 
  rename(RollingCases = DailyCases7, RollingTests = DailyTests7, RollingDeaths = DailyDeaths7) %>% 
  pivot_longer(cols =c(RollingCases, RollingDeaths, RollingTests),
               names_to = "RollingStats",
               values_to = "RollingNumbers") 

y_la <- list(
  title = "Daily Cases")

el_daily <- totals_data %>% 
  filter(Date == local_date) %>%
  filter(CAName == "East Lothian")

# scot_total <- CovidTime %>% 
#   mutate(DailyDeaths = Deaths - lag(Deaths,1)) %>% 
#   filter(Date == app_date) %>%
#   filter(Region == "Scotland") 

#-------------SCOT STATS _______________
totals_data <- la_data  %>% 
  select(-c(CumulativeNegative , CrudeRateNegative, PositiveTests, PositivePercentage7Day, TotalPillar1, TotalPillar2, CrudeRateDeaths, CrudeRatePositive))

colnames(locality_data)
population <- locality_data %>% 
  group_by(CAName) %>% 
  summarise(population = sum(Population))

# scot_population <- sum(population$population)

total_tests <- sum(totals_data$TotalTests)
scot_pos <- sum(totals_data$DailyPositive)
scot_deaths <- sum(totals_data$DailyDeaths)

# totals_data %>% 
#   group_by(Date) %>%
#   summarise(D = sum(CumulativeDeaths), C = sum(CumulativePositive)) %>% 
#   slice_max(Date, n = 1) 

scot_cumulative <-  totals_data %>% 
  summarise(scot_pos = sum(DailyPositive), scot_deaths = sum(DailyDeaths)) %>% 
  pivot_longer(cols =c(scot_pos, scot_deaths),
               names_to = "Stats",
               values_to = "CumulativeTotals") %>% 
  mutate(Stats =factor(Stats, levels = c("scot_pos", "scot_deaths"))) 
  
region_total_crude <- totals_data %>%
  filter(CAName == "East Lothian") %>% 
  filter(Date <= local_date) %>% 
  arrange(desc(Date)) %>% 
  slice_max(Date, n = 7) %>% 
  mutate(CAName = as.character(CAName)) 


region_total_crude$CAName = as.character(region_total_crude$CAName)
region_total_crude <- left_join(region_total_crude, population, by ="CAName")

# region_multiplier <- 100000/head(as.numeric(region_total_crude$population),1)
scot_total_crude <- totals_data %>%
  group_by(Date) %>% 
  summarise(DailyPositive = sum(DailyPositive)) %>% 
  arrange(desc(Date)) %>% 
  slice_max(Date, n = 7) 

scot_multiplier <- 100000/head(as.numeric(scot_population),1)

el_7day <- sum(region_total_crude$DailyPositive)
# el_crude_today <- el_7day *region_multiplier
# scot_crude_today <- sum(scot_total_crude$DailyPositive) *scot_multiplier
total_tests_today = sum(la_data$TotalTests)

national_total_data <- national_total_data %>% 
  filter(Date == app_date) %>% 
  pivot_longer(cols =c(CumulativeCases, Deaths),
               names_to = "Stats",
               values_to = "CumulativeTotals")


#---------hospitalisation rates ---------------------------------------



hospitalisation_data <- hospitalisation_data %>% 
  janitor::clean_names() %>% 
  rename("icu_covid" = "x_i_covid_19_patients_in_icu_or_combined_icu_hdu" , "all_hospital" = "ii_covid_19_patients_in_hospital_including_those_in_icu" ) %>% 
  mutate(date = ymd(as.character(str_sub(reporting_date, 1, 10))))  %>% 
  filter(date <= app_date) %>% 
  arrange(desc(date)) %>% 
  select(-reporting_date) %>% 
  pivot_longer(cols =c(all_hospital, icu_covid),
               names_to = "hospitalisation",
               values_to = "numbers") %>% 
  mutate(hospitalisation = ifelse( hospitalisation == "icu_covid", "ICU", "All Hospital"))

#--------- Vax Data

vax_data <- vax_data %>%
  mutate(Date = as.character(vax_data$Date)) %>%
  arrange(desc(Date)) %>%
  top_n(2) %>%
  arrange(Date) %>%
  head(1) %>% 
  mutate(propotion_first = round((FirstDose/scot_population)*100, 1),
         propotion_not_first = 100 - propotion_first) %>% 
  mutate(propotion_second = round((SecondDose/scot_population)*100, 1),
         propotion_not_second = 100 - propotion_second)

vax_data$FirstDose
first_vax_pie <- vax_data %>% 
  select(propotion_first, propotion_not_first) %>% 
  pivot_longer(cols =c(propotion_first, propotion_not_first),
               names_to = "split",
               values_to = "split_numbers") %>% 
  mutate(split =factor(split, levels = c("propotion_first", "propotion_not_first"))) 

had_vax_one <- first_vax_pie
first_vax_pie<- ggplot(first_vax_pie, aes(x="", y=split_numbers, fill=split)) +
  geom_bar(width = 1, stat = "identity")


second_vax_pie <- vax_data %>% 
  select(propotion_second, propotion_not_second) %>% 
  pivot_longer(cols =c(propotion_second, propotion_not_second),
               names_to = "split",
               values_to = "split_numbers") %>% 
  mutate(split =factor(split, levels = c("propotion_second", "propotion_not_second"))) 


second_vax_pie<- ggplot(second_vax_pie, aes(x="", y=split_numbers, fill=split)) +
  geom_bar(width = 1, stat = "identity")

#  Initially do this stat as a text KPI!!!!!!!!!!!!!
second_vax_pie <- second_vax_pie + 
  coord_polar("y", start=0)+ 
  scale_fill_brewer(palette = "Purples", labels = c("Had Second Dose", "Not Had Second Dose")) + 
  theme_void() +
  theme(axis.text.x=element_blank(),
        legend.title = element_blank(),
        legend.position = c(1.1,.8)) +
  geom_text(aes(y = split_numbers/2, label = paste0(split_numbers, " %"))) +
  labs(title = "Proportion of Scottish Population who have\n received Second Dose of Vaccination")

second_vax_pie