###### Data Science 1230 Class Notes ######

#Here is how to do math in R
2+2
10*5

#R is built around functions
#Here is our first function:
# the "c" function

#Suppose we want to put the numbers 1, 3, and 5 together
c(1, 3, 5)

#There are two ways to store objects in R
vector1 = c(1, 3, 5)
vector2 <- c(1, 3, 5)
x <- 5
y <- 3

#Suppose I want to take the mean of a bunch of numbers
mean()

#let's look at the help documentation for the mean function
?mean

#Let's try to take the mean of vector1
mean(x = vector1, trim = 0)
mean(vector1, 0)

#Suppose we have the heights of 5 males at Middlebury (in)
heights <- c(65, 68, 70, 66, NA)

#I want to calculate the mean height of these males
mean(x = heights)
mean(heights)


#Let's remove our missing values
mean(x = heights, na.rm = TRUE)
mean(heights, 0, TRUE)

mean(x = heights,
     trim = 0,
     na.rm = TRUE)

#Here is what a basic R plot looks like:
plot(diamonds$x, diamonds$y)

#We need to download some new packages
#install.packages("ggplot2")

#We need to load the ggplot2 library
library(ggplot2)

#This makes our blank canvas. All graphs will start
#with this.
ggplot()

#Let's look at the diamonds data set
View(diamonds)

#Let's graph the relationship between the weight (carat)
#of diamonds and their price.
ggplot(data = diamonds) + geom_point(aes(x = carat, y = price))


#### Class Notes 1/20/21 #####
library(ggplot2)

#How does the cut, clarity, and color of diamonds
#affect their price?
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, 
                           y = price, 
                           color = color))

#What if I want to graph the distribution of the prices
#of diamonds?
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price))

#Let's adjust the binwidth of our histogram
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price),
                 bins = 500)

#Let's see how we can "zoom in" on the graph
#Let's adjust our x-axis limits
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price),
                 bins = 500) +
  xlim(1000, 2000)


### Let's create a graph showing the relationship
#between the COLOR of diamonds and their PRICE.
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = color,
                             y = price))

###############
#Answer the following question:
#Why SPECIFICALLY do diamonds increase in price
#as the color gets worse?
#Answer with both a graph and a short explanation.
###############

#Let's talk about data wrangling (data cleaning).
# install.packages('tidyverse')
# 
# install.packages('dplyr')
# install.packages('magrittr')
library(tidyverse)

#Suppose I only want to buy the most expensive diamonds
#for my wife
expensiveDiamonds <- filter(.data = diamonds,
                            price > 10000)
expensiveHeavyDiamonds <- filter(.data = diamonds,
                                 price > 10000,
                                 carat > 3)
colorJDiamonds <- filter(.data = diamonds,
                         color == "J")

#Suppose I want to calculate the mean price of diamonds.
summarize(.data = diamonds,
          meanPrice = mean(price))

#Now suppose I want to calculate the mean price
#of EACH COLOR of diamond
#Let's divide our data into groups
groupedDiamonds <- group_by(.data = diamonds,
                            color)

summarize(.data = groupedDiamonds,
          meanPrice = mean(price))


### How to read data into R
peppermint <- read_csv(file.choose())


###### Class Notes 1/21/21 ######
library(tidyverse)

#Let's look at a data set with a "fixed" carat.
fixedCarat <- filter(.data = diamonds,
                     carat > 1,
                     carat < 1.5)

ggplot(data = fixedCarat) +
  geom_boxplot(mapping = aes(x = color,
                             y = price))

#Let's look at the distribution of weights (carat) for each color
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = color,
                             y = carat))

#This effect is called Simpson's Paradox!

#Introducing the pipe operator %>%
vector1 <- c(1, 2, 3)
mean(vector1)

vector1 %>% mean()

### Code from before
groupedDiamonds <- group_by(.data = diamonds,
                            color)

summarize(.data = groupedDiamonds,
          meanPrice = mean(price))

diamonds %>% 
  group_by(color) %>% 
  summarize(meanPrice = mean(price))

## Read in Peppermint data
peppermint <- read_csv(file.choose())

#Introducing the select() function
#Keeps or removes columns
select(.data = peppermint,
       Peppermint,
       Score)

peppermint %>%
  select(Peppermint,
         Score)

#Get rid of StudentID column
peppermint %>%
  select(-StudentID)

#Introducing mutate() function!
#Adds a new column to our data
peppermint %>%
  mutate(PeppermintScore = Peppermint*Score)

#Let's add a new column that checks if a student passed their test
peppermint %>%
  mutate(Pass = Score >= 70)

peppermint %>%
  mutate(Pass = ifelse(Score >= 70,
                       "Pass",
                       "Fail"))

#Let's quickly look at a discrete case
#Suppose I am only interested in color D, E, and F diamonds
diamonds %>%
  mutate(isDesirable = (color == "D" | color == "E" | color == "F"))

diamonds %>%
  mutate(isDesirable = color %in% c("D", "E", "F"))


#Download the "flights" data set from the "nycflights13" package
install.packages('nycflights13')
library(nycflights13)
?flights

#Calculate the average departure delay for flights
#into Boston's Logan Airport and Burlington's Airport
#Hint: Think about NA values.
View(flights)

flights %>%
  filter(dep_delay > 0) %>%
  group_by(dest) %>%
  filter(dest %in% c("BOS", "BTV")) %>%
  #na.omit() %>%
  summarize(avgDelay = mean(dep_delay, na.rm = TRUE))

flights %>%
  filter(dep_delay > 0) %>%
  group_by(dest) %>%
  summarize(avgDelay = mean(dep_delay, na.rm = TRUE)) %>%
  filter(dest %in% c("BOS", "BTV"))

#How to create and join data in R
favColor <- data.frame(name = c("Alex", "Alex", "Bob", "Charlie"),
                       location = c("A", "B", "B", "A"),
                       color = c("Red", "Yellow", "Blue", "Green"))

favNumber <- data.frame(name = c("Alex", "Alex", "Bill", "Charlie"),
                        location = c("A", "B", "C", "A"),
                        number = c(27, 60, 50, 1000))

#Let's suppose I want to combine the information
#in these data sets!
full_join(favColor, favNumber, by = c("name", "location"))

left_join(favColor, favNumber, by = c("name", "location"))


##### Class Notes 1/25/21 #####
library(tidyverse)
peppermint <- read_csv("Peppermint.csv")

### Does eating a peppermint lead to a higher test score?

#Let's start by calculating the mean test score
#for peppermint-eaters vs non-peppermint-eaters
peppermint %>%
  group_by(Peppermint) %>%
  summarize(mean = mean(Score))

#Did each student take all 5 tests?
peppermint %>%
  count(Test)

peppermint %>%
  count(Test, StudentID) %>%
  arrange(-n) #Arranges (or sorts) our data, based on a var

#How many exams did each student take (using graphs)
peppermint %>%
  ggplot() +
  geom_bar(mapping = aes(x = StudentID),
           stat = "count")

#How were students assigned peppermints?
peppermint %>%
  count(Peppermint)

peppermint %>%
  count(StudentID, Peppermint)


#Let's explore more graphically

#Let's look at each student's exam score distribution
peppermint %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = factor(StudentID),
                             y = Score))

peppermint %>%
  ggplot() +
  geom_point(mapping = aes(x = StudentID,
                           y = Score,
                           color = factor(Peppermint)),
             size = 3) +
  scale_color_manual(values = c("Red", "#306ccf")) +
  theme_minimal() +
  guides(color = guide_legend("Peppermint Status")) +
  theme(text = element_text(size = 20))

## Change Peppermint Values from 0/1 to Peppermint/No Peppermint

peppermint %>%
  mutate(Peppermint.clean = recode(Peppermint,
                                   "0" = "No Peppermint",
                                   "1" = "Peppermint"))





####
#install.packages('nycflights13')
library(nycflights13)
flights


## Code to make first graph
flights %>%
  filter(origin %in% c("EWR", "JFK", "LGA")) %>%
  ggplot() +
  geom_violin(mapping = aes(x = origin,
                            y = sched_arr_time,
                            fill = origin)) +
  ggtitle("Graph 1") +
  ylab("Scheduled Arrival Time") +
  theme(legend.position = "none")+
  scale_y_continuous() #Change our y-axis units


flights %>%
  ggplot() +
  geom_point(mapping = aes(x = sched_dep_time,
                           y = sched_arr_time))

#Deal with dates and times
as.Date()


#Make graphs 2 and 3
flights %>%
  #mutate(was.flight.delayed = dep_delay > 0) %>%
  mutate(was.flight.delayed = ifelse(dep_delay > 0,
                                     "Delayed",
                                     "Not Delayed")) %>%
  na.omit() %>%
  ggplot() +
  geom_bar(mapping = aes(x = carrier,
                         fill = was.flight.delayed),
           position = "fill",
           stat = "count")

#HW: Recreate Graph 4 (Hint: Use stat = "identity")




##### Class Notes 1/26/21 #####
library(tidyverse)
pregnancy.data <- read_csv("Pregnancy Data.csv")


#Let's make a scatterplot
state.subset <- pregnancy.data %>%
  filter(state %in% c("NJ", "NY", "DC"))

pregnancy.data %>%
  ggplot(aes(x = pregnancyrate1517,
             y = abortionrate1517)) +
  geom_point() +
  geom_text(aes(label = state),
            data = state.subset)

#Let's compare states with Parental Involvement
#laws (PI laws) versus states without PI laws
pregnancy.data %>%
  ggplot(aes(x = pregnancyrate1517,
             y = abortionrate1517)) +
  geom_point(aes(color = law.pi)) +
  facet_wrap(~law.pi)


####### Quick aside
diamonds %>%
  ggplot(aes(x = carat,
             y = price)) +
  geom_point(aes(color = color)) +
  facet_grid(cut~color)
#######


#Suppose I am only interested in graph the 15-17 age group
data1517 <- pregnancy.data %>%
  select(state, law.pi, contains("1517"))


#Calculate averages of ALL my variables for each law type
data1517 %>%
  group_by(law.pi)


#Let's reshape our data!
long.data1517 <- data1517 %>%
  pivot_longer(contains("1517"),
               names_to = "statistic")

long.data1517 %>%
  group_by(law.pi, statistic) %>%
  summarize(mean = mean(value))

### Data Scraping (Web Scraping)

#Something exists on the internet, and I want it in R
#install.packages('rvest')
library(rvest)

#Store URL of site we want to scrape
brady.url <- "https://en.wikipedia.org/wiki/Tom_Brady"
path1 <- '//*[@id="mw-content-text"]/div[1]/table[7]'

brady.scrape <- brady.url %>% #Scrape from this url 
  read_html() %>% #Read the site's HTML
  html_node(xpath = path1) %>% #Scrape only the object at this path
  html_table(fill = TRUE) #Convert to a meaningful R object (not HTML code)


matrix.url <- "https://www.imdb.com/title/tt0133093/reviews?ref_=tt_ov_rt"
path2 <- '//*[@id="main"]/section/div[2]/div[2]/div[1]/div[1]/div[1]/div[3]/div'

matrix.url %>%
  read_html() %>%
  html_node(xpath = path2) %>%
  html_text()


### Let's go back to our Brady data
#Let's graph Tom Brady's passing yards over time

#Let's make a copy of our data
brady.data <- brady.scrape

#change the column names of our data set
colnames(brady.data) <- brady.data[1, ]

#This extracts the 5th row and 7th column value
brady.data[5,7]

#Let's extract the entire first row
brady.data[1, ]

#Let's grab columns 1 and 8 and remove row 1
brady.subset <- brady.data[-c(1,23), c(1,8)]

#I want to graph the relationship between Year and Yards
#Let's start by making a bar charts
brady.subset %>%
  ggplot(aes(x = Year,
             y = Yds)) +
  geom_bar(stat = "identity")


#We need to convert our yards to numbers!
as.numeric()

brady.subset %>%
  ggplot(aes(x = Year,
             y = as.numeric(Yds))) +
  geom_bar(stat = "identity")



##### Class 1/27/21 #####
library(stringr)

#Remove a string from a vector
toy.vector <- c("dog", "cat", "pig")
str_remove_all(toy.vector, "g")

brady.subset %>%
  mutate(Yds = str_remove_all(Yds, ",")) %>%
  ggplot(aes(x = Year,
             y = as.numeric(Yds))) +
  geom_bar(stat = "identity")

brady.subset %>%
  mutate(Yds = str_remove_all(Yds, ",")) %>%
  ggplot(aes(x = as.numeric(Year),
             y = as.numeric(Yds))) +
  geom_line(size = 3)


### Read in our Reddit Data Set
reddit <- read_csv("RedditUsersFull.csv")

#Make a copy of my data set
reddit.data <- reddit

#I want to estimate the age distribution of users
#on Reddit.
reddit.toy.data <- reddit.data %>%
  head(5)

#Let's just look at usernames with numbers
toy.vector <- c("dog", "cat", "pig")
str_detect(toy.vector, "g")

reddit.toy.data %>%
  filter(str_detect(author, "[:digit:]"))

#What proportion of usernames have numbers?
reddit.toy.data %>%
  mutate(has.number = str_detect(author, "[:digit:]")) %>%
  summarize(proportion = mean(has.number))

#Suppose I have a vector <1, 0, 1>
#This is the same as <TRUE, FALSE, TRUE>

#I want to extract numbers that append usernames.
reddit.toy.data %>%
  mutate(numbers = str_extract(author, "([:alpha:]|[:punct:])[:digit:]{2}$"))

#Let's try it on our full data
reddit.numbers.data <- reddit.data %>%
  mutate(numbers = str_extract(author, "([:alpha:]|[:punct:])[:digit:]{2}$"))

#Here is your homework for tomorrow:
#Using this data set, remove the one excess character
#and make a visualization of the distribution of ages
#for users on Reddit
profiles <- read_csv("profiles.csv")

#Let's look at the height distribution of users of OKCupid
profiles %>%
  ggplot(aes(x = height)) +
  geom_histogram(fill = "blue",
                 color = "black",
                 bins = 20) +
  xlim(50,90)

#Let's treat height discretely
profiles %>%
  ggplot(aes(x = height)) +
  geom_bar(aes(fill = sex),
           color = "black") +
  xlim(50,90) +
  facet_wrap(~sex)

#Let's explore users essays
library(tidytext)
library(tidyverse)
profiles <- read_csv("profiles.csv")

profiles.subset <- profiles %>%
  head(100)

profiles.subset %>%
  select(essay5) %>%
  mutate(essay5 = str_remove_all(essay5, "<br />")) %>%
  unnest_tokens(output = "word",
                input = "essay5") %>%
  count(word) %>%
  arrange(-n)


##Let's remove stopwords!
stop_words


essay5.words <- profiles %>%
  select(essay5) %>%
  mutate(essay5 = str_remove_all(essay5, "<br />")) %>%
  unnest_tokens(output = "word",
                input = "essay5") %>%
  count(word) %>%
  arrange(-n) %>%
  anti_join(stop_words, by = "word")

#Let's visualize the top 10 most commonly-used words
essay5.words %>%
  na.omit() %>%
  filter(!str_detect(word, "[:digit:]")) %>%
  head(10) %>%
  ggplot(aes(x = reorder(word,-n),
             y = n)) +
  geom_bar(stat = "identity")





##### Class Notes 1/28/21 #####
reddit.no.na <- reddit.numbers.data %>%
  na.omit()

reddit.final <- reddit.no.na %>%
  mutate(final.number = str_sub(numbers, start = 2, end = 3))

#Let's graph our distribution of numbers
reddit.final %>%
  ggplot(aes(x = final.number)) +
  geom_bar()


##Let's talk about sentiment analysis
profiles <- read_csv("profiles.csv")

essay4.words <- profiles %>%
  select(essay4) %>%
  mutate(essay4 = str_remove_all(essay4, "<br />")) %>%
  unnest_tokens(output = "word",
                input = "essay4") %>%
  count(word) %>%
  arrange(-n) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% c("href", "class", "ilink"))

#How does a sentiment analysis work?
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("nrc")


#Let's start by calculating the overall sentiment 
# of all essay4 responses
essay4.sentiments <- essay4.words %>%
  left_join(get_sentiments("afinn"), by = "word")

essay4.sentiments %>%
  summarize(total = sum(n*value, na.rm = TRUE)/n())

#Let's switch gears a bit. Let's find the total
#sentiment of Donald Trump's wikipedia page.
library(rvest)
trump.url <- "https://en.wikipedia.org/wiki/Donald_Trump"

trump.text <- trump.url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text()

#Let's turn trump.text into a data set
trump.data <- tibble(text = trump.text)

#Let's tokenize!
trump.tokens <- trump.data %>%
  unnest_tokens("word", "text") %>%
  anti_join(stop_words, by = "word")

#Quick and dirty sentiment analysis
trump.tokens %>%
  count(word) %>%
  left_join(get_sentiments("afinn"), by = "word") %>%
  summarize(total = sum(n*value, na.rm = TRUE))

#Let's compare this sentiment to all past presidents
path1 <- '//*[@id="post-94"]/div[1]/p[3]'
url1 <- "https://textlists.info/history/list-of-all-presidents-of-the-united-states/"

list.of.pres <- url1 %>%
  read_html() %>%
  html_node(xpath = path1) %>%
  html_text() %>%
  str_split(pattern = "\n") %>%
  unlist() %>%
  str_replace_all(pattern = "[:space:]", replacement = "_")


#General pres scraping code
#This is a "for" loop

#This "initializes" or creates a new (empty) object 
sentiment <- NULL

for(i in 1:45){
  pres.url <- paste0("https://en.wikipedia.org/wiki/",list.of.pres[i])
  
  pres.text <- pres.url %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text()
  
  #Let's turn pres.text into a data set
  pres.data <- tibble(text = pres.text)
  
  #Let's tokenize!
  pres.tokens <- pres.data %>%
    unnest_tokens("word", "text") %>%
    anti_join(stop_words, by = "word")
  
  #Quick and dirty sentiment analysis
  sentiment[i] <- pres.tokens %>%
    count(word) %>%
    left_join(get_sentiments("afinn"), by = "word") %>%
    summarize(total = sum(n*value, na.rm = TRUE))
  
  #This will let me track the progress of my loop
  #by printing the value of i at each iteration
  print(i)
  }

#Make final data set (tibble)
final <- tibble(sentiment.value = unlist(sentiment),
       president = list.of.pres[1:10])


#### Class Notes 2/1/21 ####

#For making static "easy" maps,
#check out
geom_sf()
geom_map()
geom_polygon()

#leaflet library
#install.packages('leaflet')
library(leaflet)

#Makes our blank (mapping) canvas
leaflet() %>%
  addTiles()

#Let's make some toy data to explore
#how leaflet works
toy.data <- data.frame(x = c(37, 30, 33),
                       y = c(-95, -90, -92))


toy.data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat =~x,
             lng =~y)

#Now, let's read in some data and put
#markers on our map!

#Let's read in our volcano data
library(tidyverse)

volcano <- read_tsv(file.choose())

#Make copy of data and remove first row/column
volcano.data <- volcano[-1, -1]


#Let's map these volcanos!
volcano.data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers() %>%
  setView(-96, 37.8, 3) #This zooms in (x3 mag) on center of US

#Here's one way to clean these up
volcano.data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions()) %>%
  setView(-96, 37.8, 3)


#Let's color US states based on some variable
#Let's read our US geojson file into R
library(rgdal)
#geojson_read()

us.states <- readOGR(file.choose())

#Let's look at the "data" portion of my us.states SPDF
View(us.states@data)

us.states %>%
  leaflet() %>%
  addTiles() %>%
  setView(-96, 37.8, 3) %>%
  addPolygons()

#Let's color our states based on census area
#coloring on a gradient uses colorNumeric()

#What values does CENSUSAREA take?
us.states@data %>%
  ggplot(aes(x = CENSUSAREA)) +
  geom_histogram()

#I'm going to use colorBin()
bins <- c(0, 20000, 50000, 100000, 200000, 1000000)
state.colors <- colorBin(palette = "YlOrRd",
                         domain = us.states@data$CENSUSAREA,
                         bins = bins) 

#In addition to coloring our map, let's label each
#state

us.states %>%
  leaflet() %>%
  addPolygons(fillColor = ~state.colors(CENSUSAREA),
              fillOpacity = .7,
              color = "black",
              opacity = 1,
              label = ~NAME,
              popup = ~NAME) %>%
  setView(-96, 37.8, 3) %>%
  addLegend(pal = state.colors,
            values = us.states@data$CENSUSAREA,
            title = "Census Area (square miles)")

#Scrape in alcohol data
url <- "https://vinepair.com/articles/map-states-drink-alcohol-america-2020/"
path1 <- '//*[@id="pico"]/table[1]'

alcohol.data <- url %>%
  read_html() %>%
  html_node(xpath = path1) %>%
  html_table() 


#Let's join our alcohol data with our states SPDF

#Make a copy of my SPDF
us.states2 <- us.states

#When joining, I need to be VERY careful.
#I only want to join with the DATA part
#of us.states2, not the whole spdf
#AND, I need to be careful to only modify
#the data part of us.states2, not the whole spdf.


us.states2@data <- left_join(us.states2@data,
                             alcohol.data,
                             by = c("NAME" = "State"))



##### Class Notes 2/2/21 #####
library(shiny)



##Let's make our alcohol consumption map
library(leaflet)

us.states <- readOGR(file.choose())

#Scrape in alcohol data
url <- "https://vinepair.com/articles/map-states-drink-alcohol-america-2020/"
path1 <- '//*[@id="pico"]/table[1]'

alcohol.data <- url %>%
  read_html() %>%
  html_node(xpath = path1) %>%
  html_table() 

#Make a copy of my SPDF
us.states2 <- us.states

us.states2@data <- left_join(us.states2@data,
                             alcohol.data,
                             by = c("NAME" = "State"))



#Let's make our map
#start by making bins

#I'm going to use colorBin()
bins <- c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5)
state.colors <- colorBin(palette = "YlOrRd",
                         domain = us.states2@data$`Gallons of Ethanol Per Capita*`,
                         bins = bins) #I'm going to use colorBin()

us.states2 %>%
  leaflet() %>%
  addPolygons(fillColor = ~state.colors(`Gallons of Ethanol Per Capita*`),
              fillOpacity = .7,
              color = "black",
              opacity = 1) %>%
  setView(-96, 37.8, 3) %>%
  addLegend(pal = state.colors,
            values = us.states@data$`Gallons of Ethanol Per Capita*`,
            title = "Alcohol Consumption per year per capita (in gallons)")


##### Class Notes 2/3/21 #####

#Make a bar chart
alcohol.data %>%
  ggplot(aes(x = reorder(State, `Gallons of Ethanol Per Capita*`),
             y = `Gallons of Ethanol Per Capita*`)) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "blue") +
  coord_flip() +
  xlab("State") +
  theme_bw()


##### Class Notes 2/4/21 #####

IceCream %>%
  select(Brand, ChocolateCalories, VanillaCalories) %>%
  pivot_longer(cols = c(ChocolateCalories, VanillaCalories),
               names_to = "Flavor",
               values_to = "Calories") %>%
  mutate(Flavor = str_remove_all(Flavor, "Calories"))

#Let's save our IceCream Data
write_csv(IceCream, "IceCream.csv")


