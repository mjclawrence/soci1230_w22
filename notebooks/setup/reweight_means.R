library(tidyverse)
library(weights)

css_means <- read.csv("data/css_means_mobility_withno.csv")

css_weighted_means_by_question <- css_means |> 
  pivot_longer(names_to = "Summary",
               values_to = "prop",
               3:1059) |> 
  mutate(Summary = str_remove(Summary, "_mean")) |> 
  separate(Summary, c("Name", "response"),
           "[.]",
           remove = FALSE,
           extra = "merge", fill = "right") |> 
  mutate(response = ifelse(response == "no_maen", "no_mean", response)) |> 
  group_by(campus_id, Name) |> 
  mutate(propna = prop[str_detect(response, "propna")],
         n_responses_nona = n_responses * (1 - propna)) |> 
  ungroup() |> 
  filter(!str_detect(response, "propna")) |> 
  group_by(Summary) |> 
  summarise(wtdprop = weighted.mean(prop, w = n_responses_nona),
            n_colleges = sum(!is.na(prop)),
            mr_kq5_pq1 = wtd.cor(prop, mr_kq5_pq1, 
                                      weight = n_responses_nona)[1],
            mr_ktop1_pq1 = wtd.cor(prop, mr_ktop1_pq1, 
                                        weight = n_responses_nona)[1],
            k_rank = wtd.cor(prop, k_rank, 
                                  weight = n_responses_nona)[1],
            k_q1 = wtd.cor(prop, k_q1, 
                                weight = n_responses_nona)[1],
            k_q2 = wtd.cor(prop, k_q2, 
                                weight = n_responses_nona)[1],
            k_q3 = wtd.cor(prop, k_q3, 
                                weight = n_responses_nona)[1],
            k_q4 = wtd.cor(prop, k_q4, 
                                weight = n_responses_nona)[1],
            k_q5 = wtd.cor(prop, k_q5, 
                                weight = n_responses_nona)[1],
            k_top10pc = wtd.cor(prop, k_top10pc, 
                                     weight = n_responses_nona)[1],
            k_top5pc = wtd.cor(prop, k_top5pc, 
                                    weight = n_responses_nona)[1],
            k_top1pc = wtd.cor(prop, k_top1pc, 
                                    weight = n_responses_nona)[1],
            k_rank_cond_parq1 = wtd.cor(prop, k_rank_cond_parq1, 
                                             weight = n_responses_nona)[1],
            k_rank_cond_parq2 = wtd.cor(prop, k_rank_cond_parq2, 
                                             weight = n_responses_nona)[1],
            k_rank_cond_parq3 = wtd.cor(prop, k_rank_cond_parq3, 
                                             weight = n_responses_nona)[1],
            k_rank_cond_parq4 = wtd.cor(prop, k_rank_cond_parq4, 
                                             weight = n_responses_nona)[1],
            k_rank_cond_parq5 = wtd.cor(prop, k_rank_cond_parq5, 
                                             weight = n_responses_nona)[1],
            kq1_cond_parq1 = wtd.cor(prop, kq1_cond_parq1, 
                                          weight = n_responses_nona)[1],
            kq2_cond_parq1 = wtd.cor(prop, kq2_cond_parq1, 
                                          weight = n_responses_nona)[1],
            kq3_cond_parq1 = wtd.cor(prop, kq3_cond_parq1, 
                                          weight = n_responses_nona)[1],
            kq4_cond_parq1 = wtd.cor(prop, kq4_cond_parq1, 
                                          weight = n_responses_nona)[1],
            kq5_cond_parq1 = wtd.cor(prop, kq5_cond_parq1, 
                                          weight = n_responses_nona)[1],
            kq1_cond_parq2 = wtd.cor(prop, kq1_cond_parq2, 
                                          weight = n_responses_nona)[1],
            kq2_cond_parq2 = wtd.cor(prop, kq2_cond_parq2, 
                                          weight = n_responses_nona)[1],
            kq3_cond_parq2 = wtd.cor(prop, kq3_cond_parq2, 
                                          weight = n_responses_nona)[1],
            kq4_cond_parq2 = wtd.cor(prop, kq4_cond_parq2, 
                                          weight = n_responses_nona)[1],
            kq5_cond_parq2 = wtd.cor(prop, kq5_cond_parq2, 
                                          weight = n_responses_nona)[1],
            kq1_cond_parq3 = wtd.cor(prop, kq1_cond_parq3, 
                                          weight = n_responses_nona)[1],
            kq2_cond_parq3 = wtd.cor(prop, kq2_cond_parq3, 
                                          weight = n_responses_nona)[1],
            kq3_cond_parq3 = wtd.cor(prop, kq3_cond_parq3, 
                                          weight = n_responses_nona)[1],
            kq4_cond_parq3 = wtd.cor(prop, kq4_cond_parq3, 
                                          weight = n_responses_nona)[1],
            kq5_cond_parq3 = wtd.cor(prop, kq5_cond_parq3, 
                                          weight = n_responses_nona)[1],
            kq1_cond_parq4 = wtd.cor(prop, kq1_cond_parq4, 
                                          weight = n_responses_nona)[1],
            kq2_cond_parq4 = wtd.cor(prop, kq2_cond_parq4, 
                                          weight = n_responses_nona)[1],
            kq3_cond_parq4 = wtd.cor(prop, kq3_cond_parq4, 
                                          weight = n_responses_nona)[1],
            kq4_cond_parq4 = wtd.cor(prop, kq4_cond_parq4, 
                                          weight = n_responses_nona)[1],
            kq5_cond_parq4 = wtd.cor(prop, kq5_cond_parq4, 
                                          weight = n_responses_nona)[1],
            kq1_cond_parq5 = wtd.cor(prop, kq1_cond_parq5, 
                                          weight = n_responses_nona)[1],
            kq2_cond_parq5 = wtd.cor(prop, kq2_cond_parq5, 
                                          weight = n_responses_nona)[1],
            kq3_cond_parq5 = wtd.cor(prop, kq3_cond_parq5, 
                                          weight = n_responses_nona)[1],
            kq4_cond_parq5 = wtd.cor(prop, kq4_cond_parq5, 
                                          weight = n_responses_nona)[1],
            kq5_cond_parq5 = wtd.cor(prop, kq5_cond_parq5, 
                                          weight = n_responses_nona)[1],
            par_q1 = wtd.cor(prop, par_q1, 
                                  weight = n_responses_nona)[1],
            par_q2 = wtd.cor(prop, par_q2, 
                                  weight = n_responses_nona)[1],
            par_q3 = wtd.cor(prop, par_q3, 
                                  weight = n_responses_nona)[1],
            par_q4 = wtd.cor(prop, par_q4, 
                                  weight = n_responses_nona)[1],
            par_q5 = wtd.cor(prop, par_q5, 
                                  weight = n_responses_nona)[1]) |> 
  mutate(Summary = ifelse(Summary == "NATENGSP.no_maen", "NATENGSP.no_mean", Summary)) |> 
  mutate(across(everything(), ~na_if(.,"NaN"))) |> 
  select(-starts_with("RACEGRP"))

css_weighted_means_by_college <- css_means |> 
  pivot_longer(names_to = "variable",
               values_to = "prop",
               3:1059) |> 
  select(campus_id, n_responses, variable, prop) |> 
  separate(variable, c("question", "response"),
           "[.]",
           remove = FALSE,
           extra = "merge", fill = "right") |> 
  mutate(response = ifelse(response == "no_maen", "no_mean", response)) |> 
  group_by(campus_id, question) |> 
  mutate(propna = prop[str_detect(response, "propna")],
         n_responses_nona = n_responses * (1 - propna)) |> 
  ungroup() |> 
  filter(!str_detect(response, "propna")) |> 
  group_by(campus_id, variable) |> 
  summarise(weighted_mean = weighted.mean(prop, w = n_responses_nona)) |> 
  mutate(variable = ifelse(variable == "NATENGSP.no_maen", "NATENGSP.no_mean", variable)) |> 
  pivot_wider(names_from = "variable", values_from = "weighted_mean") |> 
  mutate(across(everything(), ~na_if(.,"NaN"))) |> 
  select(-starts_with("RACEGRP"))


tfs_means <- read.csv("data/tfs_means_mobility_withno.csv")

tfs_weighted_means_by_question <- tfs_means |> 
  pivot_longer(names_to = "Summary",
               values_to = "prop",
               3:438) |> 
  mutate(Summary = str_remove(Summary, "_mean")) |> 
  separate(Summary, c("Name", "response"),
           "[.]",
           remove = FALSE,
           extra = "merge", fill = "right") |> 
  mutate(response = ifelse(response == "no_maen", "no_mean", response)) |> 
  group_by(campus_id, Name) |> 
  mutate(propna = prop[str_detect(response, "propna")],
         n_responses_nona = n_responses * (1 - propna)) |> 
  ungroup() |> 
  filter(!str_detect(response, "propna")) |> 
  group_by(Summary) |> 
  summarise(wtdprop = weighted.mean(prop, w = n_responses_nona),
            n_colleges = sum(!is.na(prop)),
            mr_kq5_pq1 = wtd.cor(prop, mr_kq5_pq1, 
                                 weight = n_responses_nona)[1],
            mr_ktop1_pq1 = wtd.cor(prop, mr_ktop1_pq1, 
                                   weight = n_responses_nona)[1],
            k_rank = wtd.cor(prop, k_rank, 
                             weight = n_responses_nona)[1],
            k_q1 = wtd.cor(prop, k_q1, 
                           weight = n_responses_nona)[1],
            k_q2 = wtd.cor(prop, k_q2, 
                           weight = n_responses_nona)[1],
            k_q3 = wtd.cor(prop, k_q3, 
                           weight = n_responses_nona)[1],
            k_q4 = wtd.cor(prop, k_q4, 
                           weight = n_responses_nona)[1],
            k_q5 = wtd.cor(prop, k_q5, 
                           weight = n_responses_nona)[1],
            k_top10pc = wtd.cor(prop, k_top10pc, 
                                weight = n_responses_nona)[1],
            k_top5pc = wtd.cor(prop, k_top5pc, 
                               weight = n_responses_nona)[1],
            k_top1pc = wtd.cor(prop, k_top1pc, 
                               weight = n_responses_nona)[1],
            k_rank_cond_parq1 = wtd.cor(prop, k_rank_cond_parq1, 
                                        weight = n_responses_nona)[1],
            k_rank_cond_parq2 = wtd.cor(prop, k_rank_cond_parq2, 
                                        weight = n_responses_nona)[1],
            k_rank_cond_parq3 = wtd.cor(prop, k_rank_cond_parq3, 
                                        weight = n_responses_nona)[1],
            k_rank_cond_parq4 = wtd.cor(prop, k_rank_cond_parq4, 
                                        weight = n_responses_nona)[1],
            k_rank_cond_parq5 = wtd.cor(prop, k_rank_cond_parq5, 
                                        weight = n_responses_nona)[1],
            kq1_cond_parq1 = wtd.cor(prop, kq1_cond_parq1, 
                                     weight = n_responses_nona)[1],
            kq2_cond_parq1 = wtd.cor(prop, kq2_cond_parq1, 
                                     weight = n_responses_nona)[1],
            kq3_cond_parq1 = wtd.cor(prop, kq3_cond_parq1, 
                                     weight = n_responses_nona)[1],
            kq4_cond_parq1 = wtd.cor(prop, kq4_cond_parq1, 
                                     weight = n_responses_nona)[1],
            kq5_cond_parq1 = wtd.cor(prop, kq5_cond_parq1, 
                                     weight = n_responses_nona)[1],
            kq1_cond_parq2 = wtd.cor(prop, kq1_cond_parq2, 
                                     weight = n_responses_nona)[1],
            kq2_cond_parq2 = wtd.cor(prop, kq2_cond_parq2, 
                                     weight = n_responses_nona)[1],
            kq3_cond_parq2 = wtd.cor(prop, kq3_cond_parq2, 
                                     weight = n_responses_nona)[1],
            kq4_cond_parq2 = wtd.cor(prop, kq4_cond_parq2, 
                                     weight = n_responses_nona)[1],
            kq5_cond_parq2 = wtd.cor(prop, kq5_cond_parq2, 
                                     weight = n_responses_nona)[1],
            kq1_cond_parq3 = wtd.cor(prop, kq1_cond_parq3, 
                                     weight = n_responses_nona)[1],
            kq2_cond_parq3 = wtd.cor(prop, kq2_cond_parq3, 
                                     weight = n_responses_nona)[1],
            kq3_cond_parq3 = wtd.cor(prop, kq3_cond_parq3, 
                                     weight = n_responses_nona)[1],
            kq4_cond_parq3 = wtd.cor(prop, kq4_cond_parq3, 
                                     weight = n_responses_nona)[1],
            kq5_cond_parq3 = wtd.cor(prop, kq5_cond_parq3, 
                                     weight = n_responses_nona)[1],
            kq1_cond_parq4 = wtd.cor(prop, kq1_cond_parq4, 
                                     weight = n_responses_nona)[1],
            kq2_cond_parq4 = wtd.cor(prop, kq2_cond_parq4, 
                                     weight = n_responses_nona)[1],
            kq3_cond_parq4 = wtd.cor(prop, kq3_cond_parq4, 
                                     weight = n_responses_nona)[1],
            kq4_cond_parq4 = wtd.cor(prop, kq4_cond_parq4, 
                                     weight = n_responses_nona)[1],
            kq5_cond_parq4 = wtd.cor(prop, kq5_cond_parq4, 
                                     weight = n_responses_nona)[1],
            kq1_cond_parq5 = wtd.cor(prop, kq1_cond_parq5, 
                                     weight = n_responses_nona)[1],
            kq2_cond_parq5 = wtd.cor(prop, kq2_cond_parq5, 
                                     weight = n_responses_nona)[1],
            kq3_cond_parq5 = wtd.cor(prop, kq3_cond_parq5, 
                                     weight = n_responses_nona)[1],
            kq4_cond_parq5 = wtd.cor(prop, kq4_cond_parq5, 
                                     weight = n_responses_nona)[1],
            kq5_cond_parq5 = wtd.cor(prop, kq5_cond_parq5, 
                                     weight = n_responses_nona)[1],
            par_q1 = wtd.cor(prop, par_q1, 
                             weight = n_responses_nona)[1],
            par_q2 = wtd.cor(prop, par_q2, 
                             weight = n_responses_nona)[1],
            par_q3 = wtd.cor(prop, par_q3, 
                             weight = n_responses_nona)[1],
            par_q4 = wtd.cor(prop, par_q4, 
                             weight = n_responses_nona)[1],
            par_q5 = wtd.cor(prop, par_q5, 
                             weight = n_responses_nona)[1]) |> 
  mutate(Summary = ifelse(Summary == "NATENGSP.no_maen", "NATENGSP.no_mean", Summary)) |> 
  mutate(across(everything(), ~na_if(.,"NaN")))
