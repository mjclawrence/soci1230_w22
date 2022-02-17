library(tidyverse)
library(priceR)

country <- "US"
inflation_dataframe <- retrieve_inflation_data(country)

oi_dollars <- read_csv("https://opportunityinsights.org/wp-content/uploads/2018/04/mrc_table9.csv")

oi_dollars <- oi_dollars |> 
  mutate(year_16 = cohort + 16)

oi_par_hh_inc <- oi_dollars |> pull(par_hh_inc)
oi_year_16 <- oi_dollars |> pull(year_16)
oi_cohort <- oi_dollars |> pull(cohort)
oi_pctile <- oi_dollars |> pull(pctile)

dollars_adj <-  
  adjust_for_inflation(oi_par_hh_inc, 2015, 
                       "US", to_date = oi_year_16)

oi_dollars_adj <- bind_cols(oi_pctile, oi_cohort, oi_par_hh_inc,
                            oi_year_16, dollars_adj)

colnames(oi_dollars_adj) <- c("pctile",
                              "cohort",
                              "par_hh_inc_2015_dollars",
                              "year16",
                              "par_hh_inc_year16_dollars")

oi_dollars_adj |> 
  filter(pctile %in% c(20, 40, 50, 60, 80)) |>
  select(-c(cohort, par_hh_inc_2015_dollars)) |> 
  pivot_wider(names_from = year16, values_from = par_hh_inc_year16_dollars)

# Looks like 40k is roughly the median in all years
## That corresponds to INCOME_TFS <=8 in CSS years before 2000
## and INCOME_TFS <=7 in CSS years after 2000
