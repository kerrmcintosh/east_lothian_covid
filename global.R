# TO DO - link csv direct from web and filter  most recent date
# why crude rates for region from IZ and CA dataset different... Check!
# detail where population taken from



library(tidyverse)
library(plotly)
library(lubridate)
library(sf)
library(ggiraph)
library(readxl)
library(shiny)

#Pull in covid data
locality_data <- read_csv("data/trend_iz.csv", 
                          col_types = cols(CrudeRate7DayPositive = col_character(), Positive7Day = col_integer())) %>% 
  mutate(Date = ymd(as.character(Date))) 

date_working <- locality_data %>% 
  select(Date) %>% 
  arrange(desc(Date)) %>% 
  head(1) 
app_date <- locality_data %>% 
  select(Date) %>% 
  arrange(desc(Date)) %>% 
  head(1) 

head_date <- date_working$Date %>% 
  format('%d/%m/%y')

head_date_title <- date_working$Date %>% 
  format('%d %B %Y')
la_regions <- unique(locality_data$CAName)


el_data <- locality_data %>% 
  select(-c(Positive7DayQF, CrudeRate7DayPositiveQF)) %>% 
  filter(CAName == "East Lothian") %>%
  rename(InterZone = IntZone) %>% 
  mutate(CrudeRate7DayPositive = ifelse(is.na(CrudeRate7DayPositive), 0, CrudeRate7DayPositive)) %>% 
  mutate(multiplier = 100000/Population) %>% 
  mutate(Positive7Day = ifelse(is.na(Positive7Day), 0, Positive7Day)) %>% 
  mutate(real_rate_per_OT = round(multiplier*Positive7Day))
# el_data %>% 
#   filter(CAName == "East Lothian") %>% 
#   filter(Date > (app_date-7)) 
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

el_7day <- sum(el_map$Positive7Day)

tooltip_css <- "background-color:#9c9a98; 
                color: #000000;"

#---------------PLOT 2 ---------------------

la_data <- read_csv("data/trend_ca.csv") %>% 
  mutate(Date = ymd(as.character(Date)))



county_cumulative <- la_data %>%
  filter(CAName == "East Lothian") %>% 
  select(-c(DailyPositive,DailyDeaths, CA, CAName, TotalTests)) %>% 
  filter(Date == app_date) %>% 
  select(-Date) %>% 
  pivot_longer(cols =c(CumulativePositive, CumulativeDeaths),
               names_to = "Stats",
               values_to = "CumulativeTotals") %>% 
  mutate(Stats =factor(Stats, levels = c("CumulativePositive", "CumulativeDeaths"))) 


#------------------PLOT 3

totals_data <- la_data  %>% 
  select(-c(CumulativeNegative , CrudeRateNegative, PositiveTests, PositivePercentage, TotalPillar1, TotalPillar2, CrudeRateDeaths, CrudeRatePositive))

colnames(locality_data)
population <- locality_data %>% 
  filter(Date == app_date) %>% 
  group_by(CAName) %>% 
  summarise(population = sum(Population))

scot_population <- sum(population$population)

total_tests <- sum(totals_data$TotalTests)
scot_pos <- sum(totals_data$DailyPositive)
scot_deaths <- sum(totals_data$DailyDeaths)

region_total_crude <- totals_data %>%
  filter(CAName == "East Lothian") %>% 
  arrange(desc(Date)) %>% 
  slice_max(Date, n = 7) %>% 
  mutate(CAName = as.character(CAName)) 


region_total_crude$CAName = as.character(region_total_crude$CAName)
region_total_crude <- left_join(region_total_crude, population, by ="CAName")

region_multiplier <- 100000/head(as.numeric(region_total_crude$population),1)


scot_total_crude <- totals_data %>%
  group_by(Date) %>% 
  summarise(DailyPositive = sum(DailyPositive)) %>% 
  arrange(desc(Date)) %>% 
  slice_max(Date, n = 7) 

scot_multiplier <- 100000/head(as.numeric(scot_population),1)


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

national_cuml_data <- read_csv("data/daily_cuml_scot_20210116.csv") %>% 
  mutate(Date = ymd(as.character(Date))) %>% 
  mutate(Region = "Scotland") %>% 
  select(Date, Region, DailyCases, CumulativeCases, Deaths)

national_cuml_data <- left_join(national_cuml_data, scot_test_data, by ="Date")


CovidTime <- totals_data %>% 
  select(-c(CA, CumulativeDeaths)) %>% 
  rename(DailyCases = DailyPositive, CumulativeCases = CumulativePositive, Region = CAName, Deaths = DailyDeaths)


# Add Data daily and cumulative cases, deaths - Scotland and Local Authority Regions
CovidTime <- rbind(CovidTime, national_cuml_data) %>% 
  filter(Region == "East Lothian" | Region == "Scotland") 

CovidTimeLine <-  CovidTime %>% 
  select(-CumulativeCases) %>% 
  rename(Cases = DailyCases, Tests = TotalTests) %>% 
  pivot_longer(cols =c(Cases, Deaths, Tests),
               names_to = "Stats",
               values_to = "Numbers") 

y_la <- list(
  title = "Daily Cases")

el_daily <- totals_data %>% 
  filter(Date == app_date) %>%
  filter(CAName == "East Lothian")

scot_total <- CovidTime %>% 
  mutate(DailyDeaths = Deaths - lag(Deaths,1)) %>% 
  filter(Date == app_date) %>%
  filter(Region == "Scotland") 

#-------------SCOT STATS _______________
totals_data <- la_data  %>% 
  select(-c(CumulativeNegative , CrudeRateNegative, PositiveTests, PositivePercentage, TotalPillar1, TotalPillar2, CrudeRateDeaths, CrudeRatePositive))

colnames(locality_data)
population <- locality_data %>% 
  filter(Date == app_date) %>% 
  group_by(CAName) %>% 
  summarise(population = sum(Population))

scot_population <- sum(population$population)

total_tests <- sum(totals_data$TotalTests)
scot_pos <- sum(totals_data$DailyPositive)
scot_deaths <- sum(totals_data$DailyDeaths)


scot_cumulative <-  totals_data %>% 
  summarise(scot_pos = sum(DailyPositive), scot_deaths = sum(DailyDeaths)) %>% 
  pivot_longer(cols =c(scot_pos, scot_deaths),
               names_to = "Stats",
               values_to = "CumulativeTotals") %>% 
  mutate(Stats =factor(Stats, levels = c("scot_pos", "scot_deaths"))) 
  
region_total_crude <- totals_data %>%
  filter(CAName == "East Lothian") %>% 
  arrange(desc(Date)) %>% 
  slice_max(Date, n = 7) %>% 
  mutate(CAName = as.character(CAName)) 


region_total_crude$CAName = as.character(region_total_crude$CAName)
region_total_crude <- left_join(region_total_crude, population, by ="CAName")

region_multiplier <- 100000/head(as.numeric(region_total_crude$population),1)
scot_total_crude <- totals_data %>%
  group_by(Date) %>% 
  summarise(DailyPositive = sum(DailyPositive)) %>% 
  arrange(desc(Date)) %>% 
  slice_max(Date, n = 7) 

scot_multiplier <- 100000/head(as.numeric(scot_population),1)


el_crude_today <- sum(region_total_crude$DailyPositive) *region_multiplier
scot_crude_today <- sum(scot_total_crude$DailyPositive) *scot_multiplier
total_tests_today = sum(la_data$TotalTests)

#---------hospitalisation rates ---------------------------------------

hospitalisation_data <- read_excel("data/COVID-19+daily_data_trends.xlsx", sheet = "Table 2 - Hospital Care", skip = 2) 
# rename("FirstDose" = "Number of people who have received the first dose of the Covid vaccination", "SecondDose" = "Number of people who have received the second dose of the Covid vaccination" )


hospitalisation_data <- hospitalisation_data %>% 
  janitor::clean_names() %>% 
  rename("icu_covid" = "x_i_covid_19_patients_in_icu_or_combined_icu_hdu" , "all_hospital" = "ii_covid_19_patients_in_hospital_including_those_in_icu" ) %>% 
  mutate(date = ymd(as.character(str_sub(reporting_date, 1, 10))))  %>% 
  arrange(desc(date)) %>% 
  select(-reporting_date) %>% 
  pivot_longer(cols =c(all_hospital, icu_covid),
               names_to = "hospitalisation",
               values_to = "numbers") %>% 
  mutate(hospitalisation = ifelse( hospitalisation == "icu_covid", "ICU", "All Hospital"))

#--------- Vax Data

vax_icu_data <- read_excel("data/COVID-19+daily_data_trends.xlsx", sheet = "Table 10 - Vaccinations", skip = 2) %>% 
  rename("FirstDose" = "Number of people who have received the first dose of the Covid vaccination", "SecondDose" = "Number of people who have received the second dose of the Covid vaccination" )
head(vax_icu_data)
class(vax_icu_data$Date)
vax_icu_data <- vax_icu_data %>% 
  mutate(Date = as.character(vax_icu_data$Date)) %>% 
  arrange(desc(Date)) %>% 
  top_n(1) %>% 
  mutate(propotion_first = round((FirstDose/scot_population)*100, 1),
         propotion_not_first = 100 - propotion_first) %>% 
  mutate(propotion_second = round((SecondDose/scot_population)*100, 1),
         propotion_not_second = 100 - propotion_second)



vax_icu_data$FirstDose
first_vax_pie <- vax_icu_data %>% 
  select(propotion_first, propotion_not_first) %>% 
  pivot_longer(cols =c(propotion_first, propotion_not_first),
               names_to = "split",
               values_to = "split_numbers") %>% 
  mutate(split =factor(split, levels = c("propotion_first", "propotion_not_first"))) 

had_vax_one <- first_vax_pie
first_vax_pie<- ggplot(first_vax_pie, aes(x="", y=split_numbers, fill=split)) +
  geom_bar(width = 1, stat = "identity")


second_vax_pie <- vax_icu_data %>% 
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