ohio <- read_csv("https://raw.githubusercontent.com/katiesegreti/health/master/ohio_covid.csv")

str(ohio)

names(ohio) <- make_clean_names(names(ohio))

oh0 <- ohio %>%
  mutate(test_date = mdy(onset_date),
         county = as.factor(county)) %>%
  filter(county != "Grand Total" & test_date > "2020-3-9") %>%
  group_by(test_date, county) %>%
  mutate(new_positives = sum(case_count)) %>%
  select(test_date, new_positives, county) %>%
  unique()

str(oh2)


ts <- seq.Date(as.Date("2020-03-10"), (today() - 1), by = 1)
oh2 <- data.frame(test_date = ts)

oh1 <- full_join(oh2, oh0)

oh1 %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Ohio",
    caption = "data-chips.com"
  )

unique(oh1$county)


oh1 %>%
  #filter(county == "Cuyahoga") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Ohio",
    caption = "data-chips.com"
  )

oh1 %>%
  filter(county == "Cuyahoga") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Cuyahoga County, OH",
    caption = "data-chips.com"
  )

oh1 %>%
  filter(county == "Franklin") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "turquoise", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Franklin County, OH",
    caption = "data-chips.com"
  )

oh1 %>%
  filter(test_date > "2020-3-1" & county == "Cuyahoga") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "turquoise", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Cuyahoga County, OH",
    caption = "data-chips.com"
  )

oh1 %>%
  filter(test_date > "2020-3-1" & county == "Hamilton") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "turquoise", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Hamilton County, OH",
    caption = "data-chips.com"
  )

oh1 %>%
  filter(test_date > "2020-3-1" & county == "Summit") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "turquoise", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Summit County, OH",
    caption = "data-chips.com"
  )
