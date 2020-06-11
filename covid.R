library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(stringr)
library(janitor)
library(ggthemes)
library(zoo)

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


#add rolling average column
#No! I need to do the rolling average just for each county, so after it's filtered
#testing$roll7 <- rollmean(testing$new_positives, 7, fill = NA)

str(nyc_covid)
str(testing)


bg_color = "gray95"
counties_theme <- theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    strip.background = element_rect(fill = bg_color),
    #axis.text.x = element_blank(),
    legend.position = "none",
    axis.line.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    panel.grid.major.y = element_line( colour = "darkgray"),
    plot.subtitle = element_text(size = 18),
    plot.title = element_text(size = 22)
  )

nassau <- testing %>%
  filter(county == "Nassau")

nassau %>% 
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "dodgerblue") +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests",
    subtitle = "Nassau County, NY"
  )

nassau %>% 
  ggplot(aes(x = test_date, y = total_number_of_tests_performed)) +
  geom_col(fill = "magenta") +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Nassau County, NY"
  )

nassau %>% 
  ggplot(aes(x = test_date, y = pct_pos_today)) +
  geom_col()

testing %>% filter(county == "Nassau") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Nassau County, NY"
  )



testing %>% filter(county == "Queens") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Queens, NY"
  )

testing %>% filter(county == "Suffolk") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Suffolk County, NY"
  )

testing %>% filter(county == "Kings") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Brooklyn, NY"
  )

testing %>% filter(county == "New York") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Manhattan, NY"
  )

testing %>% filter(county == "Bronx") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Bronx, NY"
  )

testing %>% filter(county == "Richmond") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Staten Island, NY"
  )

testing %>% filter(county == "Westchester") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Westchester County, NY"
  )

testing %>% filter(county == "Rockland") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Rockland County, NY"
  )
