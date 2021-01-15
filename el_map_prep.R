library(tidyverse)
library(plotly)
library(lubridate)
library(sf)
library(ggiraph)

#Pull in covid data
el_data <- read_csv("data/trend_iz_20210112.csv", 
                    col_types = cols(CrudeRate7DayPositive = col_character(), Positive7Day = col_integer()))%>% 
  select(-c(Positive7DayQF, CrudeRate7DayPositiveQF)) %>% 
  filter(CAName == "East Lothian") %>% 
  mutate(Date = ymd(as.character(Date))) %>% 
# rename column for simpler joining
  rename(InterZone = IntZone) %>% 
  mutate(CrudeRate7DayPositive = ifelse(is.na(CrudeRate7DayPositive), 0, CrudeRate7DayPositive)) %>% 
  mutate(multiplier = 100000/Population) %>% 
  mutate(Positive7Day = ifelse(is.na(Positive7Day), 0, Positive7Day)) %>% 
  mutate(real_rate_per_OT = round(multiplier*Positive7Day))

# el_data %>% write_csv("data_check.csv")

class(el_data$Positive7Day)

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
  ) %>% 
  filter(Date == "2021-01-09")

colnames(el_map)
tooltip_css <- "background-color:#9c9a98;"

gg <- ggplot(el_map) +
  geom_sf_interactive(aes(fill = CrudeRate7DayPositive, 
                          tooltip = c(paste0(IntZoneName, "\n",real_rate_per_OT,  " infections per 100,000 \n (Actual rate over previous 7 days) \n", Positive7Day, " infections in last 7 days \n", abs(wow), " ",change)),  
                          data_id = IntZoneName)) +
  scale_fill_brewer(palette = "Purples") +
  theme_void() +
  labs(title = "East Lothian Locality's Infection Rate ", subtitle = "Click map for locality info" ,fill = "Infections per 100,000 \n(Crude Rate)") +
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
