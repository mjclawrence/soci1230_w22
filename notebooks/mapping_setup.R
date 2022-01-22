library(tidyverse)

colleges_all <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/colleges_all.csv")
colleges_mobility <- read_csv("https://opportunityinsights.org/wp-content/uploads/2018/04/mrc_table2.csv")
colleges_ids <- read_csv("https://opportunityinsights.org/wp-content/uploads/2018/04/mrc_table11.csv")
ipeds <- read_csv("/Users/lawrence/desktop/ipeds.csv")

ipeds <- ipeds |> 
  mutate(opeid = str_remove(opeid, "^0+"),
         opeid = str_remove(opeid, "0+$"))

###

colleges_geo <- colleges_all |> 
  select(latitude, longitude, institution, unitid) |> 
  rename(name = institution)

geodata <- left_join(colleges_geo, ipeds)

colleges_ids <- colleges_ids |> 
  mutate(opeid = as.character(opeid))

mrc_geodata <- left_join(geodata, colleges_ids)

###
colleges_mobility_subset <- colleges_mobility |> 
  select(super_opeid, name, par_q1, kq5_cond_parq1, mr_kq5_pq1,
         type, iclevel)

colleges_join <- left_join(mrc_geodata, colleges_mobility_subset)

colleges_join <- colleges_join |> 
  filter(!is.na(super_opeid), super_opeid>0, !is.na(par_q1), iclevel==1, type!=3)

colleges_join <- colleges_join |> 
  select(latitude, longitude,
         name, super_opeid,
         type,
         par_q1, kq5_cond_parq1, mr_kq5_pq1) |> 
  mutate(type = factor(type,
                       labels = c("Public", "Private not for profit")))

write.csv(colleges_join, "data/colleges_geo.csv", row.names = FALSE)


myiqr <- quantile(colleges_join$par_q1, probs = c(0.25, .75))
myiqr[1]

colleges_join |> 
  mutate(quartile = ifelse(par_q1 < myiqr[1], 1,
                           ifelse(par_q1 > myiqr[2],3,2))) |> 
  count(quartile) |> 
  mutate(total = sum(n))

