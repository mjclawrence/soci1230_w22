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
college_mobility <- read.csv("/Users/alderik/Documents/College/Senior/Fall21/Independent Study/data/college_mobility.csv")
#css questions and usability
css_names.labeled <- read.csv("/Users/alderik/Documents/College/Senior/Fall21/Independent Study/data/css_questions - Sheet1.csv")

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
college_mobility.filtered <- college_mobility[, all_filter_cols]

table(college_mobility.filtered$YEAR)

#check NAs
col_NA <- data.frame(colSums(is.na(college_mobility.filtered))/323514)
names(col_NA)[1] <- 'Percent_Missing_Values'

over_half_missing <- col_NA %>% 
  filter(Percent_Missing_Values >.50)

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
for(i in unique(college_mobility.filtered[,"campus_id"])){
  #get the 
  cm<-college_mobility.filtered %>% 
    filter(campus_id ==i) 
  
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

#TODO
#FIX COLACT
#REPLACE TEST NANS with 0
#JOIN TEST AND T2 on campus id
#GRADMAJOR pullout the ones over 10%
#DEGASP, 123 less than BA, 4 BA, 5 Masters, 6+ professional

test <- test[,order(colnames(test))]


college_mobility.filtered <- college_mobility.filtered[,order(colnames(college_mobility.filtered))]


#EXAMPLE
# across(ends_with("b"),
#        ~ mean(.x, na.rm = TRUE),
#        .names = "{.col}_liberal_mean"))

t<- college_mobility.filtered %>%
  group_by(campus_id) %>%
  summarise(across(starts_with("GRAD"),
                   ~ mean(.x, na.rm = TRUE), .names = "{.col}_frequently_mean"))


t2<- college_mobility.filtered %>%
  group_by(campus_id) %>%
  summarise(across(,
                   ~ sum(is.na(.x))/n(), .names = "{.col}_percent_NA"))
#loop through each campus id and filter for each campus id colSum is.na for each question


prop.table(table(college_mobility$career))

#calculate on school my school basis 
num_schools_missing <- data.frame(colSums(is.na(t))/318)
            
#Weird ones
#CAREERA_9416
#FRDMAJOR 81%missing
#CMPSAT 70% school missing the satisfaction values DROP
#FACULTYPRV 75% MISSING



test2 <- mutate(test, RACE5 = ifelse(RACE5=="NaN", 0, RACE5))
