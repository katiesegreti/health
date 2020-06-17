library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(stringr)
library(janitor)
library(ggthemes)
library(zoo)
library(tidyr)
library(plotly)

testing <- read_csv("https://raw.githubusercontent.com/katiesegreti/health/master/New_York_State_Statewide_COVID-19_Testing.csv")
testing1 <- read_csv("https://health.data.ny.gov/resource/xdss-u53e.csv")

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
    plot.title = element_text(size = 22),
    plot.caption = element_text(size = 12)
  )

counties_theme_stacked <- theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    strip.background = element_rect(fill = bg_color),
    #axis.text.x = element_blank(),
    legend.position = "top",
    #legend.key.size = unit(0.2, "npc"),
    legend.title = element_blank(),
    #legend.direction = "vertical",
    legend.background = element_rect(fill = bg_color),
    axis.line.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    panel.grid.major.y = element_line( colour = "darkgray"),
    plot.subtitle = element_text(size = 18),
    plot.title = element_text(size = 22),
    plot.caption = element_text(size = 12)
  )


testing %>% filter(county == "Nassau") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Nassau County, NY",
    caption = "data-chips.com"
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

bk <- testing %>% filter(county == "Kings") %>%
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


#filter testing data by county function
county_data <- function(county_selected) {
  filter(testing, county == county_selected)
}

county_data("Queens")


testing %>% filter(county == "Nassau") %>%
  ggplot(aes(x = test_date, y = total_number_of_tests_performed)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(total_number_of_tests_performed, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Number of Tests Performed By Day",
    subtitle = "Nassau County, NY"
  )


#stacked bar charts


testing_wide <- testing %>%
  select(test_date, county, new_positives, total_number_of_tests_performed) %>%
  mutate(new_negatives = total_number_of_tests_performed - new_positives) %>%
  select(test_date, county, positive = new_positives, negative = new_negatives)

testing_long <- testing_wide %>%
  pivot_longer(
    cols = c(positive, negative),
    names_to = "result"
  )
testing_long %>%
  ggplot(aes(x = test_date, y = value, fill = result)) +
  geom_col() 

t <- testing_long %>%
  filter(county == "Nassau") %>%
  ggplot(aes(x = test_date, y = value, fill = result)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("darkgrey", "magenta"))+
  counties_theme_stacked +
  labs(
    x = "",
    y = "",
    title = "Tests Performed By Day",
    subtitle = "Nassau County, NY"
  )

max(testing$test_date)


library(plotly)

fig <- plot_ly(
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(20, 14, 23),
  name = "SF Zoo",
  type = "bar"
)

fig


ggplotly(t)
ggplotly(bk)


county_data <- function(county_selected) {
  filter(testing, county == county_selected) %>%
    mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
}



county_data("Queens")



connecticut <- read_csv("https://data.ct.gov/resource/bfnu-rgqt.csv")
str(connecticut)





library(rjson)
ny_json <- do.call(rbind.data.frame, fromJSON(file = "https://health.data.ny.gov/resource/xdss-u53e.json") )

str(ny_json)
mdy(as_date(ny_json$test_date))

ny_json1 <- ny_json %>%
  mutate(test_date = mdy(test_date),
         pct_pos_today = if_else((new_positives > 0 & total_number_of_tests_performed > 0),
                                 new_positives / total_number_of_tests_performed, 0)
  )

ny_json1 <- ny_json %>%
  mutate(test_date = ymd(str_split(test_date, pattern = "T", simplify = TRUE)[,1]),
         county = as.factor(county),
         new_positives = as.numeric(new_positives),
         cumulative_number_of_positives = as.numeric(cumulative_number_of_positives),
         total_number_of_tests = as.numeric(total_number_of_tests),
         cumulative_number_of_tests = as.numeric(cumulative_number_of_tests),
         pct_pos_today = if_else((new_positives > 0 & total_number_of_tests > 0),
                                 new_positives / total_number_of_tests, 0))


str(ny_json1)
