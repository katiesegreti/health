mi1 <- read_csv("https://raw.githubusercontent.com/katiesegreti/health/master/michigan_covid.csv")

str(mi1)

michigan1 <- mi1 %>%
  mutate(test_date = mdy(Date),
         county = as.factor(COUNTY),
         new_positives = Cases) %>%
  select(test_date, county, new_positives)


alger <- michigan %>% filter(county == "Alger")

mi2 <- mi1 %>%
  group_by(COUNTY, Date) %>%
  mutate(new_cases = sum(Cases)) %>%
  unique()
  


michigan %>%
  #filter(test_date > "2020-04-17") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "New COVID-19 Cases By Day",
    subtitle = "Michigan",
    caption = "data-chips.com"
  )

unique(michigan$county)

michigan %>%
  filter(county == "Alcona") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "New COVID-19 Cases By Day",
    subtitle = "Michigan",
    caption = "data-chips.com"
  )



county_data <- function(state, county_selected) {
  if(state == "NY") {
    filter(newyork, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  } else if(state == "TX") {
    filter(texas, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  } else if(state == "CA") {
    filter(california, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  } else if(state == "CT") {
    filter(connecticut, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  } else if(state == "MI") {
    filter(michigan, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  }
}


alcona <- county_data("MI", "Alcona")
clinton <- county_data("NY", "Clinton")


mi1 %>%
  filter(COUNTY == "Alger" & Date == "4/11/2020")


mi1 %>%
  filter(COUNTY == "Alger" & Date == "4/11/2020") %>%
  group_by(COUNTY) %>%
  mutate(total_cases = sum(Cases)) %>%
  select(COUNTY, Date, total_cases) %>%
  unique()
