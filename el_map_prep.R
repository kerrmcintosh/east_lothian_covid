library(tidyverse)
library(plotly)
library(lubridate)
library(sf)
library(ggiraph)

#Pull in covid data
el_data <- read_csv("data/trend_iz_20210112.csv", 
                    col_types = cols(CrudeRate7DayPositive = col_character(), Positive7Day = col_integer()))%>% 
  filter(CAName == "East Lothian") %>% 
  mutate(Date = ymd(as.character(Date))) %>% 
# rename column for simpler joining
  rename(InterZone = IntZone) %>% 
  mutate(CrudeRate7DayPositive = ifelse(is.na(CrudeRate7DayPositive), 0, CrudeRate7DayPositive))

#Convert crude rate to ordered factor
el_data$CrudeRate7DayPositive<- factor(el_data$CrudeRate7DayPositive, levels = c(0, "1 to 49", "50 to 99", "100 to 199", "200 to 399", "400+"))
unique(el_data$CrudeRate7DayPositive)
class(el_data$CrudeRate7DayPositive)

# Bring in Spatial Data and join Covid data
el_map <- st_read("data/shape_files/SG_IntermediateZoneBdry_2011/") 
el_map <- left_join(el_map, el_data) %>% 
  filter(CAName == "East Lothian") %>% 
  filter(Date == "2021-01-07")

tooltip_css <- "background-color:#9c9a98;"

gg <- ggplot(el_map) +
  geom_sf_interactive(aes(fill = CrudeRate7DayPositive, 
                          tooltip = c(paste0(IntZoneName, "\n",CrudeRate7DayPositive,  " infections per 100,000 \n (Crude rate over previous 7 days) \n", Positive7Day, " cases in last 7 days")),  
                          data_id = IntZoneName)) +
  scale_fill_brewer(palette = "Purples") +
  theme_void() +
  labs(title = "East Lothian Locality's Infection Rate ", subtitle = "Crude rate over previous 7 days" ,fill = "Infections per 100,000") +
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
