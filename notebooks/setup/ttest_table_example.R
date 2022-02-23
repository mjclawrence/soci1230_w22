library(tidyverse)
library(weights)

chetty <- read_csv("data/chetty_fouryr.csv")
css_means <- read_csv("data/css_means_mobility_withno2.csv")

chetty <- chetty |> 
  mutate(kq345_cond_parq1 = kq3_cond_parq1 + 
           kq4_cond_parq1 + 
           kq5_cond_parq1,
         k_rank_cond_parq123 = (k_rank_cond_parq1*par_q1 +
                                  k_rank_cond_parq2*par_q2 +
                                  k_rank_cond_parq3*par_q3) /
           (par_q1 + par_q2 + par_q3))

## quantiles

chetty_quantiles <- chetty |> 
  summarise(p50_mr_kq5_pq1 = wtd.quantile(mr_kq5_pq1, 
                                          weights = count, probs = c(.5)),
            p50_kq5_cond_parq1 = wtd.quantile(kq5_cond_parq1,
                                              weights = count, probs = c(.5)),
            p50_kq345_cond_parq1 = wtd.quantile(kq345_cond_parq1,
                                                weights = count, probs = c(.5)),
            p50_k_rank_cond_parq123 = wtd.quantile(k_rank_cond_parq123, 
                                                   weights = count, probs = c(.5)),
            p75_mr_kq5_pq1 = wtd.quantile(mr_kq5_pq1, 
                                          weights = count, probs = c(.75)),
            p75_kq5_cond_parq1 = wtd.quantile(kq5_cond_parq1,
                                              weights = count, probs = c(.75)),
            p75_kq345_cond_parq1 = wtd.quantile(kq345_cond_parq1,
                                                weights = count, probs = c(.75)),
            p75_k_rank_cond_parq123 = wtd.quantile(k_rank_cond_parq123, 
                                                   weights = count, probs = c(.75))
  )

css_means <- css_means |> 
  mutate(q_mr_kq5_pq1 = ifelse(mr_kq5_pq1 < chetty_quantiles$p50_mr_kq5_pq1, 1,
                               ifelse(mr_kq5_pq1 > chetty_quantiles$p75_mr_kq5_pq1, 3, 2)),
         q_kq5_cond_parq1 = ifelse(kq5_cond_parq1 < chetty_quantiles$p50_kq5_cond_parq1, 1,
                                   ifelse(kq5_cond_parq1 > chetty_quantiles$p75_kq5_cond_parq1, 3, 2)),
         q_kq345_cond_parq1 = ifelse(kq345_cond_parq1 < chetty_quantiles$p50_kq345_cond_parq1, 1,
                                     ifelse(kq345_cond_parq1 > chetty_quantiles$p75_kq345_cond_parq1, 3, 2)),
         q_k_rank_cond_parq123 = ifelse(k_rank_cond_parq123 < chetty_quantiles$p50_k_rank_cond_parq123, 1,
                                        ifelse(k_rank_cond_parq123 > chetty_quantiles$p75_k_rank_cond_parq123, 3, 2)))


q_k_rank_cond_parq123_summary <- css_means |> 
  group_by(q_k_rank_cond_parq123) |> 
  summarise(across(3:1059, ~ mean(.x, na.rm = TRUE))) |> 
  mutate(mobility_variable = "k_rank_cond_parq123") |> 
  relocate(mobility_variable)


# See income setup for example