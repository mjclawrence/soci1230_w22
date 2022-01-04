library(tidyverse)
library(DT)

chetty <- read_csv("data/chetty_table2.csv")
chetty2 <- read.csv("data/chetty_table10.csv")

datatable(chetty |> 
            select(name, state),
          filter = "top")

datatable(chetty2 |> 
          select(name, state, flagship),
          filter = "top")

noflag <- chetty2 |> 
  group_by(state) |> 
  tally(flagship) |> 
  filter(n == 0)

states_without_flagship <- noflag$state

find_flag <- chetty2 |> 
  filter(state %in% states_without_flagship) |> 
  filter(type == 1 & tier != 9) |> 
  arrange(state)

chetty3 <- chetty2 |> 
  mutate(flagship = ifelse((str_detect(name, "System") & !str_detect(name, "Community") & flagship == 0) | flagship == 1, 1, 0),
         flagship = ifelse(state == "AZ" & str_detect(name, "University Of Arizona"), 1, flagship),
         flagship = ifelse(state == "DC" & str_detect(name, "University Of The District Of Columbia"), 1, flagship),
         flagship = ifelse(state == "HI" & str_detect(name, "University Of Hawaii"), 1, flagship),
         flagship = ifelse(state == "IL" & str_detect(name, "Computer"), 0, flagship),
         flagship = ifelse(state == "MD" & str_detect(name, "System"), 1, flagship),
         flagship = ifelse(state == "NH" & str_detect(name, "System"), 0, flagship),
         flagship = ifelse(state == "NY" & str_detect(name, "Long Island"), 0, flagship),
         flagship = ifelse(state == "PA" & str_detect(name, "Pittsburgh"), 0, flagship),
         flagship = ifelse(state == "SD" & str_detect(name, "University Of South Dakota"), 1, flagship),
         flagship = ifelse(state == "TX" & str_detect(name, "Houston"), 0, flagship),
         flagship = ifelse(state == "VT" & str_detect(name, "University Of Vermont"), 1, flagship)
         )

noflag2 <- chetty3 |> 
  group_by(state) |> 
  tally(flagship) |> 
  view()

flagships <- chetty3 |> 
  filter(flagship == 1) |> 
  select(super_opeid, name, state, fips)

mobility_rate <- chetty |> 
  select(super_opeid,  mr_kq5_pq1)

flagships <- left_join(flagships, mobility_rate)

write_csv(flagships, "data/flagships.csv")

library(tigris)
states <- states(year = 2020) |> 
  rename(fips = STATEFP)

flagships <- flagships |> 
  mutate(fips = as.character(fips))

flagship_map <- left_join(states, flagships)

plot(states)
ggplot(flagship_map) + 
  geom_sf() + 
  theme_void()
