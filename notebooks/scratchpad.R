library(tidyverse)
library(DT)

tfs <- read.csv("data/tfs_correlations.csv")
css <- read.csv("data/css_correlations.csv")

tfs_subset <- tfs |> 
  select(1:5)

css_subset <- css |> 
  select(1:5)

tfs_subset |> 
  filter(Name == "Choice") |> 
  ggplot(aes(x = Summary, y = wtdprop)) +
  geom_col()
