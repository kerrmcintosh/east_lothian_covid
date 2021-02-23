library(readxl)
library(httr)
packageVersion("readxl")


url1<-"https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/coronavirus-covid-19-trends-in-daily-data/documents/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/govscot%3Adocument/COVID-19%2BDaily%2Bdata%2B-%2BTrends%2Bin%2Bdaily%2BCOVID-19%2Bdata%2B-%2B23%2BFebruary%2B2021.xlsx"
library(readxl)
library(httr)
packageVersion("readxl")
# [1] ‘0.1.1’

GET(url1, write_disk(tf <- tempfile()))
df <- read_excel(tf, sheet = "Table 2 - Hospital Care", skip = 2)
View(df)

read_csv(url("https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/8906de12-f413-4b3f-95a0-11ed15e61773/download/trend_iz_20210223.csv"), 
         col_types = cols(CrudeRate7DayPositive = col_character(), Positive7Day = col_integer())) %>% 
  mutate(Date = ymd(as.character(Date))) 
# locality_data <- locality_data[ -1 ]

# 2 Pull in local authority data data
read_csv(url("https://www.opendata.nhs.scot/dataset/6dbdd466-45e3-4348-9ee3-1eac72b5a592/resource/4ec38438-c8b3-4946-9283-ee99f7a86a3b/download/vaccination_cuml_ca_20210217.csv"))