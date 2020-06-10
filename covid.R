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
  mutate(test_date = mdy(test_date),
         pct_pos_today = if_else((new_positives > 0 & total_number_of_tests_performed > 0),
                                 new_positives / total_number_of_tests_performed, 0)
         )


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

nassau %>% 
  ggplot(aes(x = test_date, y = pct_pos_today)) +
  geom_col()



testing %>% filter(county == "Queens") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col()

testing %>% filter(county == "Suffolk") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col()

testing %>% filter(county == "Kings") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col()

testing %>% filter(county == "New York") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col()

testing %>% filter(county == "Bronx") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col()

testing %>% filter(county == "Richmond") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col()

testing %>% filter(county == "Westchester") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col()
