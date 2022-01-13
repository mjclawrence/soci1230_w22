library(tidyverse)

tfs <- read.csv("data/tfs_correlations.csv")
css <- read.csv("data/css_correlations.csv")

tfs_subset <- tfs |> 
  select(1:5)

css_subset <- css |> 
  select(1:5)

write.csv(tfs_subset, "data/tfs_question_summary.csv", row.names = FALSE)
write.csv(css_subset, "data/css_question_summary.csv", row.names = FALSE)
