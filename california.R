ca <- read_csv("https://raw.githubusercontent.com/katiesegreti/health/master/california_covid.csv")

str(ca)

california <- ca %>%
  mutate(county = as.factor(COUNTY),
         new_positives = NEWCOUNTCONFIRMED,
         test_date = mdy(DATE)) %>%
  select(test_date, new_positives, county)

str(california)

sort(unique(california$county))

california %>% filter(county == "Los Angeles") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Los Angeles County, CA"
  )

california %>% filter(county == "Orange") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Orange County, CA"
  )

california %>% filter(county == "San Diego") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "San Diego County, CA"
  )

california %>% filter(county == "San Francisco") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "San Francisco County, CA"
  )

california %>% filter(county == "Sacramento") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Sacramento County, CA"
  )

california %>% filter(county == "Napa") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Napa County, CA"
  )

california %>% filter(county == "Alameda") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Alameda County, CA"
  )
