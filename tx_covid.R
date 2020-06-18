connecticut <- read_csv("https://data.ct.gov/resource/bfnu-rgqt.csv")
str(connecticut)

fairfield <- connecticut %>% filter(county == "Fairfield")

as.Date(connecticut$dateupdated)

ct1 <- connecticut %>%
  mutate(test_date = as.Date(dateupdated),
         county = as.factor(county)) %>%
  group_by(county) %>%
  arrange(test_date, .by_group = TRUE) %>%
  mutate(new_positives = totalcases - lag(totalcases, default = 0))

str(ct1)

ct1 %>% filter(county == "Fairfield") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Fairfield County, CT",
    caption = "data-chips.com"
  )

ct1 %>% filter(county == "Hartford") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "Hartford County, CT",
    caption = "data-chips.com"
  )
ct1$county

ct1 %>% #filter(county == "Hartford") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 0.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "Positive COVID-19 Tests By Day",
    subtitle = "CT",
    caption = "data-chips.com"
  )


texas <- read_csv("https://raw.githubusercontent.com/katiesegreti/health/master/texas_covid.csv")
str(texas)


tx_names <- names(texas)

tx_names_clean <- tx_names %>% str_remove("Cases") %>%
  str_remove_all("\r") %>%
  str_remove_all("\n") %>%
  trimws() %>% make_clean_names()

names(texas) <- tx_names_clean

tx_long <- texas %>%
  pivot_longer(cols = starts_with("x"),
               names_to = "test_date") %>%
  mutate(county_name = as.factor(county_name),
         test_date = str_remove(test_date, "x")) %>%
  mutate(test_date = str_replace(test_date, "_", "/")) %>%
  mutate(test_date = paste0(test_date, "/2020")) %>%
  mutate(test_date = mdy(test_date))

str(tx_long)

tx1 <- tx_long %>%
  group_by(county_name) %>%
  arrange(test_date, .by_group = TRUE) %>%
  mutate(new_positives = value - lag(value, default = 0))

tx1 %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  #geom_line(aes(y = rollmean(new_positives, 7, fill = NA))) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "New COVID-19 Cases By Day",
    subtitle = "Texas",
    caption = "data-chips.com"
  )

tx1 %>%
  filter(county_name == "Harris") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "New COVID-19 Cases By Day",
    subtitle = "Harris County, Texas",
    caption = "data-chips.com"
  )

tx1 %>%
  filter(county_name == "Dallas") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  geom_line(aes(y = rollmean(new_positives, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "New COVID-19 Cases By Day",
    subtitle = "Dallas County, Texas",
    caption = "data-chips.com"
  )
