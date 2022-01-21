library(gapminder)
library(tidyverse)
library(gganimate)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')


mrc <- read_csv("https://opportunityinsights.org/wp-content/uploads/2018/04/mrc_table3.csv")

mrc_subset <- mrc |> 
  select(name, cohort, tier_name, type, iclevel, par_q1)

mrc_subset |> 
  group_by(cohort, iclevel) |> 
  summarise(parq1_mean = mean(par_q1, na.rm = TRUE)) |> 
  ggplot(aes(x = iclevel, y = parq1_mean, fill = iclevel)) +
  geom_col() +
  labs(title = 'Year: {frame_time}') +
  transition_time(as.integer(cohort))

mrc_subset |> 
  filter(iclevel == 1) |> 
  group_by(cohort, tier_name) |> 
  summarise(parq1_mean = mean(par_q1, na.rm = TRUE)) |> 
  ggplot(aes(x = reorder(tier_name, parq1_mean), 
             y = parq1_mean, fill = tier_name)) +
  geom_col() +
  labs(title = 'Year: {frame_time}') +
  transition_time(as.integer(cohort))

mrc_subset |> 
  filter(name == "Middlebury College") |> 
  ggplot(aes(x = cohort, y = par_q1,
             label = round(par_q1,2))) +
  geom_point() + geom_label() +  ylim(0,.1) +
  labs(title = 'Year: {frame_time}') +
  transition_time(as.integer(cohort))
