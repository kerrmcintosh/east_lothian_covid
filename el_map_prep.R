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



class(el_data$Positive7Day)

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
  ) %>% 
  filter(Date == "2021-01-12") 



colnames(el_map)
tooltip_css <- "background-color:#9c9a98;"

gg <- ggplot(el_map) +
  geom_sf_interactive(aes(fill = CrudeRate7DayPositive, 
                          tooltip = c(paste0(IntZoneName, "\n",real_rate_per_OT,  " infections per 100,000 \n (Actual rate over previous 7 days) \n", Positive7Day, " infections in last 7 days \n(", abs(wow), " ",change,")")),  
                          data_id = IntZoneName)) +
  scale_fill_brewer(palette = "Purples") +
  theme_void() +
  labs(title = "East Lothian Locality Infection Rate ", subtitle = "Click map for locality info" ,fill = "Infections per 100,000 \n(Crude Rate)") +
  guides(shape = guide_legend(override.aes = list(size = 1)),
         color = guide_legend(override.aes = list(size = 1))) +
  theme(legend.title = element_text(size = 7), 
        legend.text = element_text(size = 5),
        legend.position = c(.9,.85))
map <- girafe(ggobj = gg) 
# x <- ggiraphOutput(height = .5, width = 1)
map <- girafe_options(map,
                      opts_zoom(min = 0.5, max = 2),
                      opts_tooltip(css = tooltip_css),
                      opts_sizing(rescale = TRUE, width = .7))
if( interactive() ) print(map)

#---------------------------------------------------------------------------
###### PLOT 2
#---------------------------------------------------------------------------

national_data  <- read_csv("data/trend_ca.csv") %>% 
  mutate(Date = ymd(as.character(Date)))

colnames(national_data)

county_cumulative <- national_data %>%
  filter(CAName == "East Lothian") %>% 
  select(-c(DailyPositive,DailyDeaths, CA, CAName, TotalTests)) %>% 
  filter(Date == "2021-01-01") %>% 
  select(-Date) %>% 
  pivot_longer(cols =c(CumulativePositive, CumulativeDeaths),
               names_to = "Stats",
               values_to = "CumulativeTotals") %>% 
  mutate(Stats =factor(Stats, levels = c("CumulativePositive", "CumulativeDeaths"))) 



ggplot(county_cumulative) +
  aes(y=CumulativeTotals, x = Stats, fill = Stats) +
  geom_col() +
  scale_fill_manual(values = c("#bfd3e6", "#88419d")) +
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, vjust=4),
        axis.text.y = element_blank(),
        legend.position = "none",
        line = element_blank(),
        panel.border = element_blank(),
        aspect.ratio = 4/5,
        plot.margin = unit(x = c(0.5, 0, 0, 0), units = "cm"),
        plot.title = element_text(margin=margin(0,0,20,0))) + 
  geom_text(aes(label = CumulativeTotals), vjust = -0.5, size = 6) +
  scale_x_discrete(expand = c(0.1, 0), labels = c("Total Positive Cases", "Total Deaths")) +
  labs(title = "East Lothian Total Cases and Deaths") +
  coord_cartesian(clip = "off") 

#---------------------------------------------------------------------------
###### KPI 3
#---------------------------------------------------------------------------
#National KPIs

totals_data <- national_data  %>% 
  select(-c(CumulativeNegative , CrudeRateNegative, PositiveTests, PositivePercentage, TotalPillar1, TotalPillar2, CrudeRateDeaths, CrudeRatePositive))

colnames(locality_data)
population <- locality_data %>% 
  filter(Date == "2021-01-01") %>% 
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
  View(total_crude)

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
total_tests_today = sum(national_data$TotalTests)
#compare with below
scot_pos_today = sum(national_data$DailyPositive)
scot_deaths_today = sum(national_data$DailyDeaths)



#---- TEST ____

#Pull in covid data

crude_check <- locality_data %>% 
  select(-c(Positive7DayQF, CrudeRate7DayPositiveQF)) %>% 
  filter(CAName == "East Lothian") %>%
  filter(Date =="2021-01-12") %>% 
  rename(InterZone = IntZone) %>% 
  mutate(CrudeRate7DayPositive = ifelse(is.na(CrudeRate7DayPositive), 0, CrudeRate7DayPositive)) %>% 
  mutate(multiplier = 100000/Population) %>% 
  mutate(Positive7Day = ifelse(is.na(Positive7Day), 0, Positive7Day)) %>% 
  mutate(real_rate_per_OT = round(multiplier*Positive7Day))
sum(crude_check$Positive7Day)*region_multiplier

#East Lothian Crude Rate 98 as opposed to 
#---- TEST FINISH!!!! ____

national_data <- read_csv("data/daily_cuml_scot_20210116.csv") %>% 
  mutate(Date = ymd(as.character(Date))) %>% 
  mutate(Region = "Scotland") %>% 
  select(Date, Region, DailyCases, CumulativeCases, Deaths) 

CovidTime <- totals_data %>% 
  select(-c(CA, DailyDeaths)) %>% 
  rename(DailyCases = DailyPositive, CumulativeCases = CumulativePositive, Region = CAName, Deaths = CumulativeDeaths)

# Add Data daily and cumulative cases, deaths - Scotland and Local Authority Regions
CovidTime <- rbind(CovidTime, national_data) %>% 
  filter(Region == "East Lothian" | Region == "Scotland") 

CovidTime <- ggplotly(
  ggplot(CovidTime) +
  geom_line(aes(x= Date, y = DailyCases, colour = Region)) + 
  theme_classic() +
    scale_colour_manual(values = c("#e08214", "#998ec3")) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_line( size=.1, color="#bfd3e6" )) +
    facet_grid(rows = vars(Region), scales = "free") +
    labs(title = "Cases over Time")) 
CovidTime %>%  layout(legend = list(orientation = 'v', x = 0.35, y = 1.15), yaxis = y) 

y <- list(
  title = "Daily Cases")

#-----------------------------------------------------------
#PIE CHART VAX

# SCOT STATS 
vaccintion rates  --  7 day cases
#-----------------------------------------------------------
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

first_vax_pie <- vax_icu_data %>% 
  select(propotion_first, propotion_not_first) %>% 
  pivot_longer(cols =c(propotion_first, propotion_not_first),
               names_to = "split",
               values_to = "split_numbers") %>% 
  mutate(split =factor(split, levels = c("propotion_first", "propotion_not_first"))) 


first_vax_pie<- ggplot(first_vax_pie, aes(x="", y=split_numbers, fill=split)) +
  geom_bar(width = 1, stat = "identity")

first_vax_pie <- first_vax_pie + 
  coord_polar("y", start=0)+ 
  scale_fill_brewer(palette = "Purples", labels = c("Had First Dose", "Not Had First Dose")) + 
  theme_void() +
  theme(axis.text.x=element_blank(),
        legend.title = element_blank(),
        legend.position = c(1.1,.8)) +
  geom_text(aes(y = split_numbers/2, label = paste0(split_numbers, " %"))) +
  labs(title = "Proportion of Scottish Population who have\n received First Dose of Vaccination")

first_vax_pie

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

view(hospitalisation_data)

hospitalisation_data <- ggplotly(
  ggplot(hospitalisation_data) +
    geom_line(aes(x= date, y = numbers, colour = hospitalisation )) + 
    theme_classic() +
    scale_colour_manual(values = c("#e08214", "#998ec3"), labels=c("Covid All Hospital Patients", "Covid ICU Patients")) +
    scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +    
    theme(legend.title = element_blank(),
          panel.grid.major.y = element_line( size=.1, color="#bfd3e6" ),
          axis.title.x = element_blank()) +
  labs(title = "Covid Hospitalisation and ICU numbers in Scotland", y = "Covid Hospitalisation Numbers") )

# y <- list(
#   title = "Covid Hospitalisation Numbers")
# 
hospitalisation_data %>%  layout(legend = list(orientation = 'v', x = 0.65, y = 0.99))


hospitalisation_data