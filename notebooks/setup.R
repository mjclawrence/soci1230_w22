#library(data.table)
library(tidyverse)
library(googlesheets4)

library(haven)
css <- read_spss("/Users/lawrence/Documents/heri_data/css/CSS.TRENDS.94.08.ARCHIVED DATA.SAV")

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
college_mobility <- read.csv("/Users/lawrence/OneDrive - Middlebury College/college_mobility/data/college_mobility.csv")

#css questions and usability
#css_names.labeled <- read.csv("/Users/alderik/Documents/College/Senior/Fall21/Independent Study/data/css_questions - Sheet1.csv")
css_names.labeled <- read_sheet("https://docs.google.com/spreadsheets/d/1nhfJ0bDN26kIhkwvsoH_nvzR3tfliJJNXfKSdBAFrYE/edit#gid=0")

#filter for usable
css_names.labeled <- css_names.labeled %>% 
  filter(usable_m == 1)
#arrange alphabetically
keep <- css_names.labeled %>% 
  dplyr::select(2:3) %>%
  arrange(Name)

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

test <- college_mobility.filtered %>%
  group_by(campus_id) %>%
  summarise(across(starts_with("POLIVIEW"),
                   ~ mean(.x >= 4, na.rm = TRUE),
                   .names = "{.col}_liberal_mean"),
            across(starts_with("ACT"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_Occasionally_mean"),
            across(starts_with("COLACT02"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_race/culture_workshop_mean"),
            across(starts_with("COLACT07"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_honors_classes_mean"),
            across(starts_with("COLACT11"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_pt_job_off_mean"),
            across(starts_with("COLACT12"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_pt_job_on_mean"),
            across(starts_with("COLACT13"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_roommate_dif_eth/race_mean"),
            across(starts_with("COLACT14"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_greeklife_member_mean"),
            across(starts_with("COLACT16"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_study_abroad_mean"),
            across(starts_with("COLACT17"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_in_eth/race_org_mean"),
            across(starts_with("COLACT18"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_internship_mean"),
            across(starts_with("COLACT19"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_campus_protest_mean"),
            across(starts_with("COLACT21"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_leadership_training_mean"),
            across(starts_with("COLACT23"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_student_gov_mean"),
            across(starts_with("COLACT27"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_took_remedial_course_mean"),
            across(starts_with("COLACT32"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_community_col_transfer_mean"),
            across(starts_with("COLACT33"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_4y_col_transfer_mean"),
            across(starts_with("COLACT33"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_college_transfer_mean"),
            across(starts_with("COLACT36"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_voted_state/nat_election_mean"),
            across(starts_with("COLACT40"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_ft_job_mean"),
            across(starts_with("COLACT41"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_research_w/_prof_mean"),
            across(starts_with("CSV"),
                   ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_mean"),
            across(starts_with("FACPRV"),
                   ~ mean(.x==3, na.rm = TRUE), .names = "{.col}_frequently_mean"),
            across(starts_with("GENACT"),
                   ~ mean(.x==3, na.rm = TRUE), .names = "{.col}_frequently_mean"),
            across(starts_with("GOAL"),
                   ~ mean(.x>=3, na.rm = TRUE), .names = "{.col}_important_mean"),
  )
#            across(starts_with("COLACT"),
# ~ mean(.x==2, na.rm = TRUE), .names = "{.col}_mean")

test2 <- mutate(test, RACE5_mean = ifelse(RACE5_mean=="NaN", 0, RACE5_mean))

# Why isn't group_by working to get prop na?

test_na <- college_mobility.filtered %>%
  group_by(campus_id) %>%
  summarise(across(starts_with("GOAL"),
                   ~ sum(is.na(.x))/n(), # add () after n
                   .names = "{.col}_frequently_mean"))


library(haven)


# CLEANUP TFS

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
