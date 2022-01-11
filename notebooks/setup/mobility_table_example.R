library(tidyverse)

chetty <- read_csv("data/chetty_table2.csv")
chetty2 <- read.csv("data/chetty_table10.csv")

mobility_table <- function(x) {
  df <- chetty |> 
    filter(name == x)
  
  pq1 <- c(df$kq1_cond_parq1,
           df$kq2_cond_parq1,
           df$kq3_cond_parq1,
           df$kq4_cond_parq1,
           df$kq5_cond_parq1)
  
  pq2 <- c(df$kq1_cond_parq2,
           df$kq2_cond_parq2,
           df$kq3_cond_parq2,
           df$kq4_cond_parq2,
           df$kq5_cond_parq2)
  
  pq3 <- c(df$kq1_cond_parq3,
           df$kq2_cond_parq3,
           df$kq3_cond_parq3,
           df$kq4_cond_parq3,
           df$kq5_cond_parq3)
  
  pq4 <- c(df$kq1_cond_parq4,
           df$kq2_cond_parq4,
           df$kq3_cond_parq4,
           df$kq4_cond_parq4,
           df$kq5_cond_parq4)
  
  pq5 <- c(df$kq1_cond_parq5,
           df$kq2_cond_parq5,
           df$kq3_cond_parq5,
           df$kq4_cond_parq5,
           df$kq5_cond_parq5)
  
  college_table <- rbind(pq1, pq2, pq3, pq4, pq5)
  
  colnames(college_table) <- c("kq1", "kq2", "kq3", "kq4", "kq5")
  
  round(college_table,3)
}

mobility_table("Middlebury College")


mobility_table_wtd <- function(x) {
  df <- chetty |> 
    filter(name == x)
  
  pq1 <- c(df$kq1_cond_parq1,
           df$kq2_cond_parq1,
           df$kq3_cond_parq1,
           df$kq4_cond_parq1,
           df$kq5_cond_parq1) * df$par_q1
  
  pq2 <- c(df$kq1_cond_parq2,
           df$kq2_cond_parq2,
           df$kq3_cond_parq2,
           df$kq4_cond_parq2,
           df$kq5_cond_parq2) * df$par_q2
  
  pq3 <- c(df$kq1_cond_parq3,
           df$kq2_cond_parq3,
           df$kq3_cond_parq3,
           df$kq4_cond_parq3,
           df$kq5_cond_parq3) * df$par_q3
  
  pq4 <- c(df$kq1_cond_parq4,
           df$kq2_cond_parq4,
           df$kq3_cond_parq4,
           df$kq4_cond_parq4,
           df$kq5_cond_parq4) * df$par_q4
  
  pq5 <- c(df$kq1_cond_parq5,
           df$kq2_cond_parq5,
           df$kq3_cond_parq5,
           df$kq4_cond_parq5,
           df$kq5_cond_parq5) * df$par_q5
  
  college_table <- rbind(pq1, pq2, pq3, pq4, pq5)
  
  colnames(college_table) <- c("kq1", "kq2", "kq3", "kq4", "kq5")
  
  round(college_table,3)
}

mobility_table_wtd("City College Of New York - CUNY")
mobility_table("City College Of New York - CUNY")

