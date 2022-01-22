library(tidyverse)

colleges_all <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1001/master/data/colleges_all.csv?token=GHSAT0AAAAAABQH4ODFGJNTDVMMFDXM4LFIYPMDPPA")
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
  select(super_opeid, name, par_q1, kq5_cond_parq1, mr_kq5_pq1)

colleges_join <- left_join(mrc_geodata, colleges_mobility_subset)

colleges_join <- colleges_join |> 
  filter(!is.na(super_opeid), super_opeid>0, !is.na(par_q1))

write.csv(colleges_join, "desktop/colleges_geo.csv", row.names = FALSE)
