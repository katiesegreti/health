library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(stringr)
library(janitor)

testing <- read_csv("https://raw.githubusercontent.com/katiesegreti/health/master/New_York_State_Statewide_COVID-19_Testing.csv")
nyc_covid <- read_csv("https://raw.githubusercontent.com/katiesegreti/health/master/COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")

nyc_covid <- nyc_covid %>%
  mutate(date_interest = mdy(str_split(DATE_OF_INTEREST, pattern = " ", simplify = TRUE)[,1]))



testing$County <- as.factor(testing$County)  
names(testing) <- make_clean_names(names(testing))

testing <- testing %>%
  mutate(test_date = as_date(test_date),
         pct_pos_today = new_positives / total_number_of_tests_performed)


str(nyc_covid)
str(testing)


nassau <- testing %>%
  filter(county == "Nassau")

nassau %>% 
ggplot(aes(x = test_date, y = new_positives)) +
  geom_col()

nassau %>% 
  ggplot(aes(x = test_date, y = total_number_of_tests_performed)) +
  geom_col()



