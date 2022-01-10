#library(data.table)
library(tidyverse)

#NOTES
#obs survey response
#1440 columns
#2-65 are college mobility data
#65-1440 are the survey responses
#filter 1995-2005
#see how many are left
#any question asked in 1995-2005 but doesn't need be asked in all those years
#sort questions into Academic, social, non-Academic
#questions
#0 student info
#1 academics
#2 student life
#3 about social/personal/political life

#usable 
#1
#0

#EDA
#descriptive
#clean 
#get college level proportions

#add the goals go back and relook at 

#_________________________________________________________________________________

#READ DATA

#mobility data
#college_mobility <- read.csv("/Users/alderik/Documents/College/Senior/Fall21/Independent Study/data/college_mobility.csv")
college_mobility <- read_csv("data/college_mobility.csv")

#css questions and usability
#css_names.labeled <- read.csv("/Users/alderik/Documents/College/Senior/Fall21/Independent Study/data/css_questions - Sheet1.csv")
css_names.labeled <- read_csv("data/css_questions_usable.csv")

#college names
collnames <- read_csv("data/collnames.csv")

#tfs questions
tfs <- college_mobility |> select(campus_id, ends_with("TFS"))

tfs_longitudinal <- read_csv("codebooks/tfs_question_index.csv")
tfs_longitudinal2 <- tfs_longitudinal |>
  pivot_longer(names_to = "year", values_to = "asked", "2020":"1990") |>
  filter(year %in% 1995:2001) |>
  group_by(Name) |>
  mutate(n_asked = sum(asked)) |>
  ungroup() |>
  select(Name, Description, n_asked) |>
  distinct() |>
  filter(n_asked>0)

#write.csv(tfs_longitudinal2, "data/tfs_usable.csv", row.names = FALSE)

#filter for usable
tfs_names.labeled <- read_csv("data/tfs_usable.csv")
tfs_names.labeled <- tfs_names.labeled |> 
  filter(usable_m == 1)

css_names.labeled <- css_names.labeled %>% 
  filter(usable_m == 1) |> 
  select(Name, Description, n_asked, usable_m)

## TFS Data

usable_tfs_names <- tfs_names.labeled$Name

choice_tfs <- read_csv("data/tfs/choice.csv")
choice_tfs_subset <- choice_tfs |> 
  filter(YEAR %in% 1995:2001) |> 
  select(ACERECODE, any_of(usable_tfs_names))

choice_tfs_subset <- left_join(collnames, choice_tfs_subset) |> 
  select(-c("name", "super_opeid", "ACERECODE"))

demographics_tfs <- read_csv("data/tfs/demographics.csv")
demographics_tfs_subset <- demographics_tfs |> 
  filter(YEAR %in% 1995:2001) |> 
  select(ACERECODE, any_of(usable_tfs_names))

demographics_tfs_subset <- left_join(collnames, demographics_tfs_subset) |> 
  select(-c("name", "super_opeid", "ACERECODE", "STUDWGT", "YEAR"))

disagg_tfs <- read_csv("data/tfs/disagg.csv")
disagg_tfs_subset <- disagg_tfs |> 
  filter(YEAR %in% 1995:2001) |> 
  select(ACERECODE, any_of(usable_tfs_names))

disagg_tfs_subset <- left_join(collnames, disagg_tfs_subset) |> 
  select(-c("name", "super_opeid", "ACERECODE", "STUDWGT", "YEAR"))

funds_tfs <- read_csv("data/tfs/funds.csv")
funds_tfs_subset <- funds_tfs |> 
  filter(YEAR %in% 1995:2001) |> 
  select(ACERECODE, any_of(usable_tfs_names))

funds_tfs_subset <- left_join(collnames, funds_tfs_subset) |> 
  select(-c("name", "super_opeid", "ACERECODE", "STUDWGT", "YEAR"))

hs_tfs <- read_csv("data/tfs/hs.csv")
hs_tfs_subset <- hs_tfs |> 
  filter(YEAR %in% 1995:2001) |> 
  select(ACERECODE, any_of(usable_tfs_names))

hs_tfs_subset <- left_join(collnames, hs_tfs_subset) |> 
  select(-c("name", "super_opeid", "ACERECODE", "STUDWGT", "YEAR"))

plans_tfs <- read_csv("data/tfs/plans.csv")
plans_tfs_subset <- plans_tfs |> 
  filter(YEAR %in% 1995:2001) |> 
  select(ACERECODE, any_of(usable_tfs_names))

plans_tfs_subset <- left_join(collnames, plans_tfs_subset) |> 
  select(-c("name", "super_opeid", "ACERECODE", "STUDWGT", "YEAR"))

view_tfs <- read_csv("data/tfs/view.csv")
view_tfs_subset <- view_tfs |> 
  filter(YEAR %in% 1995:2001) |> 
  select(ACERECODE, YEAR, any_of(usable_tfs_names))

view_tfs_subset <- left_join(collnames, view_tfs_subset) |> 
  select(-c("name", "super_opeid", "ACERECODE", "STUDWGT", "YEAR"))

tfs_subset_master <- bind_cols(choice_tfs_subset,
                               demographics_tfs_subset,
                               disagg_tfs_subset,
                               funds_tfs_subset,
                               hs_tfs_subset,
                               plans_tfs_subset,
                               view_tfs_subset) |> 
  mutate(campus_id = campus_id...1,
         SELECTIVITY = SELECTIVITY...29) |> 
  select(-starts_with(c("campus_id...", "SELECTIVITY..."))) |> 
  relocate(campus_id) |> 
  mutate(SELECTIVITY = na_if(SELECTIVITY, 0))

# bind tfs and css usable variables

css_names.labeled <- bind_rows(css_names.labeled, tfs_names.labeled)

#arrange alphabetically
keep <- css_names.labeled %>% 
  dplyr::select(2:3) %>%
  arrange("Name")

#filter mobility data for column names we care about 
usable_questions <- css_names.labeled$Name
all_colnames <- colnames(college_mobility)
intersection <- Reduce(intersect,list(usable_questions,all_colnames))
#mobility columns here
#mobility_cols <- colnames(college_mobility)[0:65]
#add campus_id
mobility_cols <- colnames(college_mobility)[1]
all_filter_cols<- c(intersection,mobility_cols)
#filter for desired columns
#college_mobility.filtered <- college_mobility[, all_filter_cols]

college_mobility.filtered <- college_mobility |> 
  select(all_of(all_filter_cols))

table(college_mobility.filtered$YEAR)

#check NAs
colSums(is.na(college_mobility.filtered))


#COLLEGES WITH MORE THAN 100 RESPONSES
x <- college_mobility.filtered %>% 
  count(campus_id)

x <- x  %>% filter(n>100)
college_mobility.filtered <- left_join(x, college_mobility.filtered, by = "campus_id")


#proportion missing for each question by school
#number of observations per school
set.seed(1)

college_proportion_answered <- list()
test <- list()

#loop through every campusid
for(i in unique(college_mobility.filtered$campus_id)){
  #get the 
  cm<-college_mobility.filtered %>% 
    filter(campus_id == i) 
  
  na<-colSums(is.na(cm))
  not.na<-colSums(!is.na(cm))
  prop<- na/(na+not.na)
  test[[i]]<- prop
  
}

# Bind rows from each iteration
big_data = do.call(rbind, test)

# Make matrix a data frame
big_data <- as.data.frame(big_data)

# Drop prop missing campus_id
big_data <- dplyr::select(big_data, -c("campus_id"))

# Vector of all campus_ids 
campus_id <- c(unique(college_mobility.filtered[,"campus_id"]))

# Bind vector and data frame
big_data <- cbind.data.frame(campus_id, big_data)

tfs_means <- tfs_subset_master |> 
  group_by(campus_id) |> 
  summarise(across(starts_with("CHOICE"),
                   list(mean = ~mean(.x==4, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()), 
                   .names = "{.col}.firstchoice_{.fn}"),
            across(starts_with("CHOOSE"),
                   list(mean = ~mean(.x==3, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()), 
                   .names = "{.col}.veryimportant_{.fn}"),
            across(starts_with("FIRSTGEN"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()), 
                   .names = "{.col}.yes_{.fn}")
  )

css_means <- college_mobility.filtered %>%
  select(-ends_with("TFS")) |> 
  group_by(campus_id) %>%
  summarise(
            across(starts_with("ACT"),
                   list(mean = ~mean(.x==3, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()), 
                   .names = "{.col}.frequently_{.fn}"),
            across(starts_with("COLACT02"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.race/culture_workshop_{.fn}"),
            across(starts_with("COLACT07"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.honors_classes_{.fn}"),
            across(starts_with("COLACT11"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.pt_job_off_{.fn}"),
            across(starts_with("COLACT12"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.pt_job_on_{.fn}"),
            across(starts_with("COLACT13"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.roommate_dif_eth/race_{.fn}"),
            across(starts_with("COLACT14"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.greeklife_member_{.fn}"),
            across(starts_with("COLACT16"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.study_abroad_{.fn}"),
            across(starts_with("COLACT17"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.in_eth/race_org_{.fn}"),
            across(starts_with("COLACT18"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.internship_{.fn}"),
            across(starts_with("COLACT19"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.campus_protest_{.fn}"),
            across(starts_with("COLACT21"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.leadership_training_{.fn}"),
            across(starts_with("COLACT23"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.student_gov_{.fn}"),
            across(starts_with("COLACT27"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.took_remedial_course_{.fn}"),
            across(starts_with("COLACT32"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.community_col_transfer_{.fn}"),
            across(starts_with("COLACT33"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.4y_col_transfer_{.fn}"),
            across(starts_with("COLACT34"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.college_transfer_{.fn}"),
            across(starts_with("COLACT36"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.voted_state/nat_election_{.fn}"),
            across(starts_with("COLACT40"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.ft_job_{.fn}"),
            across(starts_with("COLACT41"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.research_w/_prof_{.fn}"),
            across(starts_with("CSV"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.{.fn}"),
            across(starts_with("FACPRV"),
                   list(mean = ~mean(.x==3, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.frequently_{.fn}"),
            across(starts_with("GENACT"),
                   list(mean = ~mean(.x==3, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.frequently_{.fn}"),
            across(starts_with("GOAL"),
                   list(mean = ~mean(.x>=3, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.important_{.fn}"),
            across(starts_with("HPW"),
                   list(mean = ~mean(.x>=6, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.11hrs_plus_{.fn}"),
            across(starts_with("MAJOR"),
                   list(mean = ~mean(.x==3, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.business_major_{.fn}"),
            across(starts_with("NATENG"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.{.fn}"),
            across(starts_with("PLANS"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.yes_{.fn}"),
            across(starts_with("POLIVIEW"),
                   list(mean = ~mean(.x>=4, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.liberal/farleft_{.fn}"),
            across(starts_with("RACE"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.{.fn}"),
            across(starts_with("RATE"),
                   list(mean = ~mean(.x>=5, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.abv_avg_{.fn}"),
            across(starts_with("REENROLL"),
                   list(mean = ~mean(.x>=4, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.def_yes_{.fn}"),
            across(starts_with("SREL"),
                   list(jewish_mean = ~mean(.x==6, na.rm = TRUE),
                        catholic_mean = ~mean(.x==12, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.{.fn}"),
            across(starts_with("SATIS"),
                   list(mean = ~mean(.x>=4, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.satisfied_or_very_satisfied_{.fn}"),
            across(starts_with("SEX"),
                   list(mean = ~mean(.x==2, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.female_{.fn}"),
            across(starts_with("SLFCH"),
                   list(mean = ~mean(.x>=4, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.stronger_or_much_stronger_{.fn}"),
            across(starts_with("SUCC"),
                   list(mean = ~mean(.x==3, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.{.fn}"),
            across(starts_with("VIEW"),
                   list(mean = ~mean(.x>=3, na.rm = TRUE), 
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.agree_somewhat_or_strongly_{.fn}"),
            across(starts_with("ACADEMIC_SELFCONCEPT"),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.{.fn}"),
            across(starts_with("SOCIAL_SELFCONCEPT"),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.{.fn}"),
            across(starts_with("SOCIAL_AGENCY"),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.{.fn}"),
            across(starts_with("FAC_INTERACTION"),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        propna = ~sum(is.na(.)) / n()),
                   .names = "{.col}.{.fn}")
  ) %>%
  select(sort(names(.))) %>%
  relocate(campus_id) %>%
  mutate(across(everything(), ~na_if(.,"NaN")))

# Keep only the variable names
varnames_original <- gsub("(.+?)(\\..*)", "\\1", names(test))
varnames_summarize <- names(test)
varnames <- bind_cols(varnames_original, varnames_summarize)
colnames(varnames) <- c("Name", "Summarized")
varnames <- left_join(varnames, css_names.labeled)
varnames <- varnames |> 
  filter(!str_detect(Summarized, "propna"))

# Chetty Variables
chetty <- read_csv("data/chetty_table2.csv")
chetty_vars <- chetty |> 
  select(super_opeid, mr_kq5_pq1,
         mr_ktop1_pq1,
                 k_rank, k_q5, k_top10pc,
                 k_top5pc, k_top1pc,
                 starts_with("k_rank_cond_parq"),
                 starts_with("kq5_cond_parq"),
                 par_q1, par_q5)

college_master <- read_csv("data/collnames.csv")

chetty_vars <- left_join(college_master, chetty_vars)

chetty_vars <- chetty_vars |> 
  select(-c(ACERECODE, name, super_opeid))

chetty_means <- left_join(test, chetty_vars)

cortest1 <- chetty_means |> 
  select(-c(campus_id, ends_with("propna"))) |> 
  summarise(across(everything(),
                   list(cor_mr_kq5_pq1 = ~cor(.x, mr_kq5_pq1, use = "complete"),
                        cor_mr_ktop1_pq1 = ~cor(.x, mr_ktop1_pq1, use = "complete"),
                        cor_k_rank = ~cor(.x, k_rank, use = "complete"),
                        cor_k_q5 = ~cor(.x, k_q5, use = "complete"),
                        cor_k_top10pc = ~cor(.x, k_top10pc, use = "complete"),
                        cor_k_top5pc = ~cor(.x, k_top5pc, use = "complete"),
                        cor_k_top1pc = ~cor(.x, k_top1pc, use = "complete"),
                        cor_k_rank_cond_parq1 = ~cor(.x, k_rank_cond_parq1, use = "complete"),
                        cor_k_rank_cond_parq2 = ~cor(.x, k_rank_cond_parq2, use = "complete"),
                        cor_k_rank_cond_parq3 = ~cor(.x, k_rank_cond_parq3, use = "complete"),
                        cor_k_rank_cond_parq4 = ~cor(.x, k_rank_cond_parq4, use = "complete"),
                        cor_k_rank_cond_parq5 = ~cor(.x, k_rank_cond_parq5, use = "complete"),
                        cor_kq5_cond_parq1 = ~cor(.x, kq5_cond_parq1, use = "complete"),
                        cor_kq5_cond_parq5 = ~cor(.x, kq5_cond_parq5, use = "complete"),
                        cor_par_q1 = ~cor(.x, par_q1, use = "complete"),
                        cor_par_q5 = ~cor(.x, par_q5, use = "complete")),
                   .names = "{.col}_{.fn}")
  ) |>
  pivot_longer(names_to = "Variable", values_to = "Correlation", 1:3776) |>
  filter(str_detect(Variable, "mean")) |> 
  separate(Variable, c("Name", "Mobility"), sep = ".mean") |> 
  separate(Mobility, c("Drop", "Mobility"), sep = "or_") |> 
  select(-Drop) |> 
  pivot_wider(names_from = "Mobility", values_from = "Correlation")

cortest1$Name <- gsub("(.+?)(\\..*)", "\\1", cortest1$Name)

cortest1 <- left_join(varnames, cortest1)
cortest1 <- cortest1 |> 
  select(-c(4:8)) |> 
  filter(Name != "campus_id")


# Why isn't group_by working to get prop na?


# CLEANUP TFS

library(haven)
#css <- read_spss("/Users/lawrence/Documents/heri_data/css/CSS.TRENDS.94.08.ARCHIVED DATA.SAV")


# heri_choice = 14235467
# restrict years = 4213528
# restrict css college = 1937983

## Demographics
heri_demographics <- read_sav("/Users/lawrence/Documents/heri_data/tfs/master/1 DEMOGRAPHICS.SAV")
heri_demographics <- heri_demographics |> 
  filter(YEAR %in% 1995:2005) |> 
  filter(ACERECODE %in% acerecodes)

gc()

## High school
heri_hs <- read_sav("/Users/lawrence/Documents/heri_data/tfs/master/2 HIGH SCHOOL.SAV")
heri_hs <- heri_hs |> 
  filter(YEAR %in% 1995:2005) |> 
  filter(ACERECODE %in% acerecodes)

gc()


## Choice
heri_choice <- read_sav("/Users/lawrence/Documents/heri_data/tfs/master/3 CHOICE.SAV")
heri_choice <- heri_choice |> 
  filter(YEAR %in% 1995:2005) |> 
  filter(ACERECODE %in% acerecodes)

gc()


## Plans
heri_plans <- read_sav("/Users/lawrence/Documents/heri_data/tfs/master/4 PLANS.SAV")
heri_plans <- heri_plans |> 
  filter(YEAR %in% 1995:2005) |> 
  filter(ACERECODE %in% acerecodes)

gc()


## View
heri_view <- read_sav("/Users/lawrence/Documents/heri_data/tfs/master/5 VIEW.SAV")
heri_view <- heri_view |> 
  filter(YEAR %in% 1995:2005) |> 
  filter(ACERECODE %in% acerecodes)

gc()


## Funds
heri_funds <- read_sav("/Users/lawrence/Documents/heri_data/tfs/master/6 FUNDS.SAV")
heri_funds <- heri_funds |> 
  filter(YEAR %in% 1995:2005) |> 
  filter(ACERECODE %in% acerecodes)

gc()


## Disagg
heri_disagg <- read_sav("/Users/lawrence/Documents/heri_data/tfs/master/7 DISAGG.SAV")
heri_disagg <- heri_disagg |> 
  filter(YEAR %in% 1995:2005) |> 
  filter(ACERECODE %in% acerecodes)

gc()



fwrite(heri_choice, "/users/lawrence/desktop/heri/tfs/choice.csv",
       row.names = FALSE)

fwrite(heri_demographics, "/users/lawrence/desktop/heri/tfs/demographics.csv",
       row.names = FALSE)

fwrite(heri_hs, "/users/lawrence/desktop/heri/tfs/hs.csv",
       row.names = FALSE)

fwrite(heri_plans, "/users/lawrence/desktop/heri/tfs/plans.csv",
       row.names = FALSE)

fwrite(heri_view, "/users/lawrence/desktop/heri/tfs/view.csv",
       row.names = FALSE)

fwrite(heri_funds, "/users/lawrence/desktop/heri/tfs/funds.csv",
       row.names = FALSE)

fwrite(heri_disagg, "/users/lawrence/desktop/heri/tfs/disagg.csv",
       row.names = FALSE)


## Constructs
FAC_INTERACTION
ACADEMIC_SELFCONCEPT
SOCIAL_SELFCONCEPT
SOCIAL_AGENCY
