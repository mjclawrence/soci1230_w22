library(tidyverse)

tfs <- read.csv("data/tfs_correlations_withno.csv")
css <- read.csv("data/css_correlations_withno.csv")

tfs_withno <- tfs |> 
  select(1:5) |> 
  mutate(Survey = "TFS",
         Survey_Name = "The Freshman Survey")

css_withno <- css |> 
  select(1:5) |> 
  mutate(Survey = "CSS",
         Survey_Name = "College Senior Survey")

write.csv(tfs_withno, "data/tfs_question_summary_withno.csv", row.names = FALSE)
write.csv(css_withno, "data/css_question_summary_withno.csv", row.names = FALSE)

tfs_withoutno <- read.csv("data/tfs_correlations.csv")
css_withoutno <- read.csv("data/css_correlations.csv")

tfs_withlabels <- read.csv("data/tfs_question_summary_labels.csv")
css_withlabels <- read.csv("data/css_question_summary_labels.csv")

tfs <- left_join(tfs_withno, tfs_withlabels)
css <- left_join(css_withno, css_withlabels)

write.csv(tfs, "data/tfs_question_summary_labels.csv", row.names = FALSE)
write.csv(css, "data/css_question_summary_labels.csv", row.names = FALSE)

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
