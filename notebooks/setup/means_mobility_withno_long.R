library(tidyverse)
library(weights)

tfs_df <- read_csv("data/tfs_means_mobility_withno.csv")
css_df <- read_csv("data/css_means_mobility_withno.csv")

tfs_mobility <- tfs_df |> 
  relocate(type:par_q5, .after = n_responses) |> 
  pivot_longer(names_to = "response_id",
               values_to = "prop",
               54:ncol(tfs_df)) |>
  mutate(response_id = str_remove(response_id, "_mean")) |> 
  separate(response_id, c("Name", "response"),
           "[.]",
           remove = FALSE,
           extra = "merge", fill = "right") |>
  group_by(campus_id, Name) |> 
  mutate(propna = prop[str_detect(response, "propna")],
         n_responses_nona = n_responses * (1 - propna)) |> 
  ungroup() |> 
  select(mr_kq5_pq1, 1:4, Name, response_id, prop, propna, n_responses_nona)

write.csv(tfs_mobility, "data/tfs_means_mobility_withno_long.csv", row.names = FALSE)

css_mobility <- css_df |> 
  relocate(type:par_q5, .after = n_responses) |> 
  pivot_longer(names_to = "response_id",
               values_to = "prop",
               54:ncol(css_df)) |>
  mutate(response_id = str_remove(response_id, "_mean")) |> 
  separate(response_id, c("Name", "response"),
           "[.]",
           remove = FALSE,
           extra = "merge", fill = "right") |>
  group_by(campus_id, Name) |> 
  mutate(propna = prop[str_detect(response, "propna")],
         n_responses_nona = n_responses * (1 - propna)) |> 
  ungroup() |> 
  select(mr_kq5_pq1, 1:4, Name, response_id, prop, propna, n_responses_nona)

write.csv(css_mobility, "data/css_means_mobility_withno_long.csv", row.names = FALSE)


