#unformatted maryland data
md1 <- read_csv("maryland_covid.csv")


str(as.Date(md_long$DATE[1]))
#make long (pivot_longer)
md_long <- md1 %>%
  pivot_longer(
    cols = Allegany:Unknown,
    names_to = "county",
    values_to = "totalcases"
  ) %>%
  mutate(test_date = as.Date(DATE),
         county = as.factor(county),
         ) %>%
  group_by(county) %>%
  arrange(test_date, .by_group = TRUE) %>%
  mutate(new_positives = totalcases - lag(totalcases, default = 0)) %>%
  select(test_date, county, new_positives)

str(md_long)





md_long %>%
  #filter(test_date > "2020-04-17") %>%
  ggplot(aes(x = test_date, y = new_positives)) +
  geom_col(fill = "magenta", width = 0.8) +
  #geom_line(aes(y = rollmean(total, 7, fill = NA)), color = "midnightblue", size = 1.3) +
  counties_theme +
  labs(
    x = "",
    y = "",
    title = "New COVID-19 Cases By Day",
    subtitle = "Maryland",
    caption = "data-chips.com"
  )

md_long[is.na(md_long$new_positives)]

#this converts all NAs to 0
md_long[is.na(md_long)] <- 0
