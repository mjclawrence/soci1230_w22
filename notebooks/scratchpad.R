library(tidyverse)

tfs <- read.csv("data/tfs_correlations.csv")
css <- read.csv("data/css_correlations.csv")

tfs_subset <- tfs |> 
  select(1:5)

css_subset <- css |> 
  select(1:5)

write.csv(tfs_subset, "data/tfs_question_summary.csv", row.names = FALSE)
write.csv(css_subset, "data/css_question_summary.csv", row.names = FALSE)

###

tfs_means <- read.csv("data/tfs_means_mobility.csv")
css_means <- read.csv("data/css_means_mobility.csv")

tfs_means <- tfs_means |> 
  relocate(c(iclevel, type, tier_name), .after = n_responses)

css_means <- css_means |> 
  relocate(c(iclevel, type, tier_name), .after = n_responses)

#write.csv(tfs_means, "data/tfs_college_means.csv", row.names = FALSE)
#write.csv(css_means, "data/css_college_means.csv", row.names = FALSE)


tfs_means |> 
  group_by(tier_name) |> 
  summarise(CHOICE.lessthanthird = weighted.mean(CHOICE.lessthanthird_mean,
                                                 w = n_responses),
            CHOOSE05.veryimportant = weighted.mean(CHOOSE12.veryimportant_mean + CHOOSE12.somewhatimportant_mean, 
                                                   w = n_responses))
