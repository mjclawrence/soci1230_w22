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

filter(diamonds, carat>1.5 & clarity %in% desirable.clarities)

### %in% 