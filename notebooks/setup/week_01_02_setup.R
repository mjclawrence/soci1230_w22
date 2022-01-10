library(tidyverse)

mobility <- read.csv("https://opportunityinsights.org/wp-content/uploads/2018/04/mrc_table2.csv", stringsAsFactors = TRUE)

mean_rank <-  mobility |> 
    summarise(pq1 = weighted.mean(k_rank_cond_parq1, count*par_q1),
              pq2 = weighted.mean(k_rank_cond_parq2, count*par_q2),
              pq3 = weighted.mean(k_rank_cond_parq3, count*par_q3),
              pq4 = weighted.mean(k_rank_cond_parq4, count*par_q4),
              pq5 = weighted.mean(k_rank_cond_parq5, count*par_q5)) |> 
  mutate(tier_name = "Average") |> 
    pivot_longer(names_to = "pq", values_to = "k_rank", pq1:pq5) |> 
    mutate(k_rank = round(k_rank,2),
          tier = tier_name)

mean_rank_tier <- mobility |> 
  group_by(tier_name) |> 
  summarise(pq1 = weighted.mean(k_rank_cond_parq1, count*par_q1),
            pq2 = weighted.mean(k_rank_cond_parq2, count*par_q2),
            pq3 = weighted.mean(k_rank_cond_parq3, count*par_q3),
            pq4 = weighted.mean(k_rank_cond_parq4, count*par_q4),
            pq5 = weighted.mean(k_rank_cond_parq5, count*par_q5)) |> 
  pivot_longer(names_to = "pq", values_to = "k_rank", pq1:pq5) |> 
  mutate(k_rank = round(k_rank,2),
         tier = tier_name)

ggplotly(
  ggplot(mean_rank, aes(x = pq, y = k_rank, group = tier)) + 
    geom_point() + 
    geom_line(linetype = "longdash") +
    geom_point(data = mean_rank_tier, aes(x = pq, y = k_rank, color = tier_name)) +
    geom_line(data = mean_rank_tier, aes(x = pq, y = k_rank, color = tier_name, group = tier)) +
    theme_clean() + theme(legend.position = "none") + 
    labs(x = "Parent Quintile", y = "Average Child Rank",
          title = "Rank by Parent Quintile",
          subtitle = "Data from Opportunity Insights"),
  tooltip = c("pq","k_rank","tier")
)


mobsum <- mobility |> 
  group_by(tier_name) |> 
  summarise(mean_1 = mean(kq5_cond_parq1),
            mean_5 = mean(kq5_cond_parq5),
            wtd_mean_1 = weighted.mean(kq5_cond_parq1, count),
            wtd_mean_5 = weighted.mean(kq5_cond_parq5, count))

mobility |> 
  group_by(tier_name) |> 
  summarise(pq1 = weighted.mean(k_rank_cond_parq1, count*par_q1),
            pq2 = weighted.mean(k_rank_cond_parq2, count*par_q2),
            pq3 = weighted.mean(k_rank_cond_parq3, count*par_q3),
            pq4 = weighted.mean(k_rank_cond_parq4, count*par_q4),
            pq5 = weighted.mean(k_rank_cond_parq5, count*par_q5)) |> 
  pivot_longer(names_to = "pq", values_to = "k_rank", pq1:pq5) |> 
  ggplot(aes(x = pq, y = k_rank)) + geom_col() + facet_wrap(~tier_name) +
  geom_text(aes(label = round(k_rank,2), vjust = -0.5)) +
  ylim(c(0,1))

library(ggrepel)
library(plotly)
library(ggthemes)



ggplotly(
  mobility |> 
  summarise(pq1 = weighted.mean(k_rank_cond_parq1, count*par_q1),
            pq2 = weighted.mean(k_rank_cond_parq2, count*par_q2),
            pq3 = weighted.mean(k_rank_cond_parq3, count*par_q3),
            pq4 = weighted.mean(k_rank_cond_parq4, count*par_q4),
            pq5 = weighted.mean(k_rank_cond_parq5, count*par_q5)) |> 
  pivot_longer(names_to = "pq", values_to = "k_rank", pq1:pq5) |> 
  mutate(k_rank = round(k_rank,2),
         tier_name = "All") |> 
  ggplot(aes(x = pq, y = k_rank, color = tier_name)) + 
  geom_point() + geom_line(aes(group = tier_name)) +
  #geom_label_repel(data = . %>% filter(pq == "pq5"), aes(label = tier_name,
  #                                                             y = k_rank, 
  #                                                             color = tier_name)) +
  theme_clean() + theme(legend.position = "none"),
tooltip = c("tier_name", "pq", "k_rank")
)



