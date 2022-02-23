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

#tfs_withlabels <- read.csv("data/tfs_question_summary_labels.csv")
#css_withlabels <- read.csv("data/css_question_summary_labels.csv")


original_tfs_labels <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/tfs_question_summary_labels.csv")
original_tfs_labels <- original_tfs_labels |> 
  select(Summary, Label, Group)

original_css_labels <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/css_question_summary_labels.csv")
original_css_labels <- original_css_labels |> 
  select(Summary, Label, Group)

tfs <- left_join(tfs_withno, original_tfs_labels)
css <- left_join(css_withno, original_css_labels)

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



### Fixing k_rank_cond_parq123

library(tidyverse)
library(weights)

mobility <- read_csv("data/css_means_mobility_withno2.csv")
names <- read_csv("data/collnames.csv")
chetty <- read_csv("data/chetty_fouryr.csv")

wtd.quantile(chetty$mr_kq5_pq1, weights = chetty$count, prob = c(.9))

chetty2 <- chetty |> 
  mutate(k_rank_cond_parq123 = k_rank_cond_parq1*par_q1 + k_rank_cond_parq2*par_q1 + 
           k_rank_cond_parq3*par_q3,
         k_rank_cond_parq123_rev = k_rank_cond_parq123 / ((par_q1 + par_q2 + par_q3)),
         k_rank_manual = ((k_rank_cond_parq123 + k_rank_cond_parq4*par_q4 + k_rank_cond_parq5*par_q5) / 
                            (par_q1 + par_q2 + par_q3 + par_q4 + par_q5)),
         kq345_cond_parq1 = kq3_cond_parq1 + kq4_cond_parq1 + kq5_cond_parq1)

summary(chetty2$k_rank_manual)
summary(chetty2$k_rank)

`
|> 
  summarise(p75_mr_kq5_pq1 = wtd.quantile(mr_kq5_pq1, weights = count, prob = c(.75)),
            p75_kq5_cond_parq1 = wtd.quantile(kq5_cond_parq1, weights = count, prob = c(.75)),
            p75_k_rank_cond_parq123 = wtd.quantile(k_rank_cond_parq123, weights = count, prob = c(.75)),
            p75_k_rank_cond_parq123_rev = wtd.quantile(k_rank_cond_parq123_rev, weights = count, prob = c(.75)),
            p75_kq345_cond_parq1 = wtd.quantile(kq345_cond_parq1, weights = count, prob = c(.75)))


summary(chetty$k_rank_cond_parq1)

krank_vars <- c("k_rank_cond_parq1", "k_rank_cond_parq2", "k_rank_cond_parq3")

chetty |> mean(krank_vars, na.rm = TRUE)




middlebury <- chetty |> 
  filter(name == "Middlebury College") |> 
  select(name, starts_with("k_rank"), starts_with("par_q"))

middlebury %>%
  mutate(rank_1 = k_rank_cond_parq1 * par_q1,
         rank_2 = k_rank_cond_parq2 * par_q2,
         rank_3 = k_rank_cond_parq3 * par_q3,
         rank_4 = k_rank_cond_parq4 * par_q4,
         rank_5 = k_rank_cond_parq5 * par_q5,
         par_q123 = par_q1 + par_q2 + par_q3) %>%
  mutate(mymean = rowSums(select(., rank_1:rank_3)), na.rm = TRUE) %>% 
  mutate(k_rank_cond_parq123_rev = mymean / par_q123) |> 
  select(k_rank, mymean, par_q123, k_rank_cond_parq123_rev)

middlebury |> 
  mutate(newrank = 
           (k_rank_cond_parq1*par_q1 +
              k_rank_cond_parq2*par_q2 +
              k_rank_cond_parq3*par_q3) /
           (par_q1 + par_q2 + par_q3)
  ) |> 
  select(newrank)


mutate(rowMeans(select(., starts_with("imp")), na.rm = TRUE))


rowMeans(.[,-1:-2]))
