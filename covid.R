library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(stringr)

testing <- read_csv("https://raw.githubusercontent.com/katiesegreti/health/master/New_York_State_Statewide_COVID-19_Testing.csv")
nyc_covid <- read_csv("https://raw.githubusercontent.com/katiesegreti/health/master/COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")


str(testing)
testing$County <- as.factor(testing$County)  

str(nyc_covid)


nyc_covid <- nyc_covid %>%
  mutate(date_interest = str_split(DATE_OF_INTEREST, pattern = " ", simplify = TRUE)[,1])

