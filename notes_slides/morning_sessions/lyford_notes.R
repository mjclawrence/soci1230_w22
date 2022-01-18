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