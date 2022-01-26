# Week One

## Monday, January 10

# Use the hashtag to add comments.

# The first function is the "c" function. 

# Suppose I want the numbers 1, 4, and 8 in a list

c(1, 4, 8)

# Let's store our above vector for future use.

vector1 = c(1, 4, 8)
vector1

## Preferable to use the object assignment arrow 

vector1 <- c(1, 4, 8)

# Let's take the mean (average) of our vector

mean(vector1)

# We can store this mean in the same way as above

mean1 <- mean(vector1)
mean1

# Let's look at optional arguments to functions

student.sleep <- c(1, 4, 8, NA)
mean(student.sleep)
mean(student.sleep, na.rm = TRUE)

# Install a new package

## install.packages("ggplot2")

# Every time we open RStudio and want to use a package, 
# we need to load it's library

library("ggplot2")

# The ggplot2 package allows us to make "cool" and "pretty" graphs

# The package has built-in datasets, like `diamonds`

diamonds # Command-click over diamonds to open the spreadsheet
View(diamonds)

# Can we visualize the relationship between the weight (carat)
# of diamonds and their price?

ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point()

## Use the scales package to change axis label formats
library(scales)
ggplot(data = diamonds, aes(x = carat, y = price)) +
  scale_y_continuous(labels=scales::dollar_format())

ggplot(diamonds, aes(carat, price)) +
  scale_y_continuous(labels=dollar_format())

## Tuesday, January 11

# Let's make a graph visualizing the relationship between
# the color of a diamond and its price

ggplot(data = diamonds, aes(x = color, y = price)) + 
  geom_boxplot()

# Let's try faceting

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price, color = color)) +
  facet_grid(~color)

## Facet by two variables

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price, color = color)) +
  facet_grid(clarity~color)

## Let's focus on some of the cheaper diamonds

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price), bins = 100) +
  xlim(0, 5000)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price), bins = 100) +
  xlim(0, 2000)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price)) +
  xlim(0, 2000)

## Introducing Data Wrangling

### The filter function keeps rows

### Suppose I want to buy only diamonds that cost more than 10000

filter(diamonds, price > 10000)

### And only expensive diamonds of the best color (D)

filter(diamonds, price>10000, color == "D")

desirable.clarities <- c("I1", "SI1", "SI2")

### %in% 

filter(diamonds, carat>1.5 & clarity %in% desirable.clarities)

## Wednesday, January 12

### Introducing group_by()

library(tidyverse)

### plotly has its own group_by so uninstall it before using dplyr::group_by

grouped.diamonds <- dplyr::group_by(.data = diamonds, color)

### What is the average carat of expensive diamonds (>10000) 
### with colors E and F?

diamonds |> 
  filter(price > 10000 & color %in% c("E", "F")) |> 
  group_by(color) |> 
  summarise(mean_carat = mean(carat))

### Introducing the pipe operator %>%

vector1 <- c(1,2,3)
mean(vector1)

mean_vector1 <- c(1,2,3) |> 
  mean()
mean_vector1

diamonds %>% 
  filter(price > 10000 & 
           color %in% c("E", "F")) %>%
  group_by(color) %>%
  summarise(mean_carat = mean(carat))

### What is the average delay of flights into Burlington Airport 
### and the average delay of flights into Logan Airport (in Boston)?

library(nycflights13)

View(flights)

flights |> 
  filter(dest %in% c("BTV", "BOS")) |> 
  group_by(dest) |> 
  summarise(mean_delay = mean(arr_delay, na.rm = TRUE))

# This...
flights |> 
  filter(dest %in% c("BTV", "BOS")) |> 
  mutate(any_arr_delay = ifelse(arr_delay>0, 1, 0)) |> 
  group_by(dest) |> 
  summarise(mean_any_arr_delay = mean(any_arr_delay, na.rm = TRUE))

# Is the same as this...
flights |> 
  filter(dest %in% c("BTV", "BOS")) |> 
  group_by(dest) |> 
  summarise(mean_any_arr_delay = mean(arr_delay>0, na.rm = TRUE))


## Examples

flights |> 
  filter(origin %in% c("JFK", "LGA")) |> 
  ggplot(aes(x = origin, y = dep_time, fill = origin)) +
  geom_violin()

flights |> 
  group_by(carrier) |> 
  summarise(mean_distance = mean(distance, na.rm = TRUE)) |> 
  ggplot(aes(x = carrier, y = mean_distance, 
             fill = mean_distance)) + geom_col()


flights |> 
  group_by(carrier) |> 
  summarise(mean_distance = mean(distance, na.rm = TRUE)) |> 
  ggplot(aes(x = reorder(carrier, -mean_distance), y = mean_distance, 
             fill = mean_distance)) + geom_col()

flights |> 
  filter(carrier %in% c("DL", "HA", "OO")) |>
  group_by(carrier, month) |> 
  summarise(mean_dep_delay = mean(dep_delay, na.rm = TRUE)) |> 
  ggplot(aes(x = carrier, y = mean_dep_delay, fill = carrier)) +
  geom_col() + facet_wrap(~month)

# Week Two

## Tuesday, January 18

library(tidyverse)
profiles <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/13FuGzlObJQd3GyLHDpIC9n0IIVVGrPWk/SOCI 1230A: DataScience Across Disciplines - Winter 2022/Morning Activities/profiles.csv")

profiles |> 
  ggplot() +
  geom_boxplot(aes(x = height))

profiles |> 
  ggplot() +
  geom_boxplot(aes(x = height)) +
  xlim(48, 96)

profiles |> 
  ggplot() +
  geom_boxplot(aes(x = height, y = sex)) +
  xlim(48, 96)

profiles |> 
  ggplot() +
  geom_histogram(aes(x = height, fill = sex)) 

profiles |> 
  ggplot() +
  geom_bar(aes(x = height, fill = sex)) 

### The f heights are *stacked* on top when there are overlaps

profiles |> 
  ggplot() +
  geom_bar(aes(x = height, fill = sex)) 

profiles |> 
  count(body_type) |> 
  filter(!is.na(body_type)) |> 
  mutate(prop = n / sum(n)) |> 
  arrange(-prop)

profiles |> 
  count(body_type) |> 
  filter(!is.na(body_type)) |> 
  mutate(prop = n / sum(n)) |> 
  arrange(-prop) |> 
  ggplot(aes(x = reorder(body_type, n), y = n)) +
  geom_col()

## Do those who like soccer have different body types
## than those who don't mention soccer at all?

## Toy example

toy.vector <- c("Alex", "Becky", "Charlie")

### Introducing str_detect() - detects if a string is present

str_detect(toy.vector, "a")

#### How to include A and a:
str_detect(toy.vector, regex("a", ignore_case = TRUE))

### Can replace values 
str_replace(toy.vector, regex("a", ignore_case = TRUE), ":)")

## Let's make a new variable for soccer likers

soccer.data <- profiles |> 
  mutate(soccer = str_detect(essay0, "soccer"))

soccer.data |> 
  count(soccer, body_type) |> 
  na.omit() |> 
  ggplot(aes(x = body_type, y = n)) +
  geom_col()

### Let's look at this proportionally

soccer.data |> 
  count(soccer, body_type) |> 
  na.omit() |> 
  group_by(soccer) |> 
  mutate(prop = n/sum(n)) |> 
  ggplot(aes(x = body_type, y = prop)) +
  geom_col() + facet_wrap(~soccer)

library(scales)

soccer.data |> 
  count(soccer, body_type) %>% 
  na.omit() %>%
  group_by(soccer) %>% 
  mutate(prop = n/sum(n)) %>% 
  #filter(body_type == "athletic") %>%
  ggplot(aes(x = soccer, y = prop,
             fill = body_type)) +
  geom_col() +
 facet_wrap(~body_type) +
  theme(legend.position = "none")

soccer.data |> 
  group_by(soccer) |> 
  summarize(prop = mean(body_type=="athletic", na.rm = TRUE))


soccer.data |> 
  pivot_longer(names_to = "essay",
               values_to = "content",
               essay0:essay9) |> 
  mutate(any_soccer = str_detect(content, "soccer")) |> 
  group_by(essay) |> 
  count(any_soccer) |> 
  na.omit() |> 
  arrange(-n) |> 
  pivot_wider(names_from = "essay",
              values_from = "content") |> 
  count(any_soccer)


profiles |> 
  mutate(internet = str_detect(essay5, regex("internet", ignore_case = TRUE))) |> 
  filter(str_detect(job, regex("computer", ignore_case = TRUE))) |> 
  ggplot() +
  geom_bar(aes(x = internet))

profiles |> 
  mutate(teacher = str_detect(essay1, "teacher")) |> 
  ggplot()

## Week Two, Class Two

install.packages("tidytext")
library(tidytext)

### Introducing tokenizing

profiles <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/13FuGzlObJQd3GyLHDpIC9n0IIVVGrPWk/SOCI 1230A: DataScience Across Disciplines - Winter 2022/Morning Activities/profiles.csv")

essay2.tokens <- profiles |> 
  unnest_tokens(input = "essay2",
                output = "word") # needs to be `word` for anti_join to work

essay2.tokens |> 
  count(word) |> 
  arrange(-n) |>
  top_n(10)

# Let's filter out stop words

top.words <- essay2.tokens |> 
  count(word) |> 
  arrange(-n) |>
  filter(!(word %in% stop_words$word)) |> 
  #anti_join(stop_words) |> # more efficient way to filter out words in stop_words
  top_n(20)

# Intro to web scraping

library(rvest)

url <- "https://en.wikipedia.org/wiki/Joe_Biden"

biden.text <- url |> 
  read_html() |> 
  html_elements("p") |> 
  html_text()

biden.text[10]

biden.data <- tibble(text = biden.text)

biden.data |> 
  unnest_tokens(input = "text",
                output = "word") |> 
  count(word) |> 
  anti_join(stop_words) |> 
  arrange(-n)


url <- "https://en.wikipedia.org/wiki/Tom_Brady"

brady.table <- url |> 
  read_html() |> 
  html_element(xpath = '//*[@id="mw-content-text"]/div[1]/table[5]') |> 
  html_table()

### Replace column names with values in first row

colnames(brady.table) <- paste(colnames(brady.table),
                               brady.table[1, ])

brady.table2 <- brady.table[-c(1, 24),]

brady.table2 |> 
  mutate(yds_numeric = as.numeric(str_remove_all(`Passing Yds`, "[:punct:]"))) |> 
  ggplot() +
  geom_line(aes(x = as.numeric(`Year Year`),
             y = yds_numeric))


## Today

life <- read_csv("notes_slides/morning_sessions/data/life_expectancy_years.csv")

#Let's start by making a basic graph of life expectancy for two different countries
#China, Brazil

#First, let's reshape our data from wide format to long format
#using pivot_longer()
life.long <- life %>%
  pivot_longer(-country,
               names_to = "Year",
               values_to = "LifeExpectancy") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 2022)


life.long %>%
  filter(country %in% c("China", "Brazil")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = LifeExpectancy, color = country),
            size = 2)

#Let's repeat the above process with income
income <- read_csv("notes_slides/morning_sessions/data/income_per_person_gdppercapita_ppp_inflation_adjusted.csv")

#Repeat with income
income.long <- income %>%
  mutate(across(.cols = -country,
                .fns = ~as.numeric(str_replace(.x, "k", "e3")))) %>% #replaces k with e3
  pivot_longer(-country,
               names_to = "Year",
               values_to = "income") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 2022)


income.long %>%
  filter(country %in% c("China", "Brazil")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = income, color = country),
            size = 2)

#Let's join our income data and our life expectancy data
joined.data <- income.long %>%
  inner_join(life.long, by = c("country", "Year"))

#Let's make one frame of our animation
#The year 2000

#Only start using scientific notation above this number
options(scipen = 100000)

joined.data %>%
  filter(Year == 2000) %>%
  ggplot() +
  geom_point(aes(x = income, y = LifeExpectancy)) +
  scale_x_log10()

#Scrape in region data (for use in coloring)
library(rvest)

url <- "https://meta.wikimedia.org/wiki/List_of_countries_by_regional_classification"

region.data <- url %>%
  read_html() %>%
  html_element("table") %>%
  html_table()

#How do I join my "joined.data" and "region.data"?
joined.region.data <- joined.data %>%
  inner_join(region.data, by = c("country" = "Country"))

#Make our graph with region colors
joined.region.data %>%
  filter(Year == 2000) %>%
  ggplot() +
  geom_point(aes(x = income, y = LifeExpectancy, color = Region)) +
  scale_x_log10()

#How can I figure out which countries are being "deleted"
#with my inner_join? (Which countries don't have matching region data?)
joined.data %>%
  anti_join(region.data, by = c("country" = "Country")) %>%
  filter(Year == 2000)

population <- read_csv("notes_slides/morning_sessions/data/population_total.csv")

pop.long <- population %>%
  mutate(across(.cols = -country,
                .fns = ~str_replace(.x, "k", "e3"))) %>% #replaces k with e3
  mutate(across(.cols = -country,
                .fns = ~str_replace(.x, "M", "e6"))) %>% #replaces k with e3
  mutate(across(.cols = -country,
                .fns = ~as.numeric(str_replace(.x, "B", "e9")))) %>% #replaces k with e3
  pivot_longer(-country,
               names_to = "Year",
               values_to = "pop") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 2022)

final.joined.data <- joined.region.data |> 
  inner_join(pop.long)

## Ready to animate

### Step 1 is to build a single frame taht looks perfect

library(gganimate)

graph1 <- final.joined.data |> 
  #filter(Year == 2000) |> 
  ggplot() +
  geom_point(aes(x = income, y = LifeExpectancy, color = Region, size = pop)) +
  scale_x_log10() + theme_bw() +
  transition_time(Year) +
  labs(title = 'Year: {frame_time}')

animation1 <- animate(graph1,
        nframes = 223)

final.joined.data |> 
  filter(country %in% c("Canada", "China", "Angola")) |> 
  ggplot() +
  geom_line(aes(x = Year, y = income, color = country),
             size = 2) +
  theme_bw() +
  transition_reveal(Year) # to keep previous year lines


diamonds |> 
  ggplot() + 
  geom_point(aes(x = carat, y = price, color = color)) +
  transition_states(color, state_length = 1,
                    transition_length = 1) +
  enter_fade() +
  exit_fade()

library(fivethirtyeight)

bechdel


### 1/25/22
library(tidyverse)
#install.packages("rnaturalearth")
library(rnaturalearth)

world <- ne_countries(returnclass = "sf")

world |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = income_grp)) +
  theme_bw()

### Label Vienna Austria

vienna_data <- tibble(y = 48.2082,
                      x = 16.3738)

world |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = income_grp)) +
  geom_point(data = vienna_data, 
             aes(x = latitude, y = longitude,
                 size = 8, color = "red")) +
  theme_bw()


### Alcohol consumption by state

library(usmap)

plot_usmap(regions = "states")

url <- ("https://worldpopulationreview.com/state-rankings/alcohol-consumption-by-state")
alcohol_data <- read_csv("/Users/lawrence/Documents/GitHub/soci1230_w22/notes_slides/morning_sessions/data/csvData.csv")

alcohol_data <- alcohol_data |> 
  rename(state = State,
         alcohol = alcoholConsumptionGallons)

plot_usmap(data = alcohol_data,
           values = "alcohol") + 
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(option = "cividis") +
  guides(fill = "none")

library(leaflet) 

vienna_data |> 
leaflet() |> 
  addTiles() |> 
  addMarkers(lat = ~y,
             lng = ~x)

# Plot taco bell locations

tacobell <- read_csv("/Users/lawrence/Downloads/archive/FastFoodRestaurants.csv")

tacobell_vt <- tacobell |> 
  filter(province %in% c("CT", "MA", "ME", "NH", "RI", "VT")) |> 
  filter(province == "VT")

tacobell_vt

write.csv(tacobell_vt, "notes_slides/morning_sessions/data/tacobell_vt.csv", 
          row.names = TRUE)

tacobell_ny <- tacobell |> filter(city == "New York")

write.csv(tacobell_ny, "notes_slides/morning_sessions/data/tacobell_ny.csv", 
          row.names = TRUE)


## 1/26/21



profiles <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/13FuGzlObJQd3GyLHDpIC9n0IIVVGrPWk/SOCI 1230A: DataScience Across Disciplines - Winter 2022/Morning Activities/profiles.csv")

myword <- "baseball"

profiles |> 
  pivot_longer(names_to = "essay",
               values_to = "text",
               essay0:essay9) |> 
  filter(str_detect(text, myword)) |> 
  group_by(essay) |> 
  summarise(n = n())
