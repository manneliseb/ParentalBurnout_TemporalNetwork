#-----------------------------------------------------------------------------------------#
# Demographic statistics/descriptive information (from baseline measures)                 #
#-----------------------------------------------------------------------------------------#

# This R code examines the descriptive baseline demographic data for the sample
# related to this project: https://osf.io/5sgkr/

# Libraries: 
library(here) # for relative paths within project
library(tidyverse) 
library(skimr) # summary functions for descriptive data  

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Importing Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Demographic data
data_demo <- readr::read_csv(here::here("data", "processed", "ESM2_PB_Demographic_General.csv"))



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Demographic Information ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### 2.1 Demographic descriptives  -----------------------------------------------------

# Number of participants
length(unique(data_demo$ID)) # 40 (not counting 1 excluded, already taken out of dataset)

# Descriptives for numeric variables

numeric_descriptives <- data_demo %>% 
  dplyr::select(age, num_kids, num_kids_house, years_w_spouse, years_of_study, PBA_Exh, PBA_Dist, PBA_FedUp, PBA_Con, PBA_total, GAD, BR2, BDI) %>% 
  skimr::skim() 

# Check full descriptive info
print(numeric_descriptives)

# Save relevant columns to export & round values
numeric_descriptives <- numeric_descriptives %>%
  dplyr::select(skim_variable, numeric.mean, numeric.p50, numeric.sd, numeric.p0, numeric.p100) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 2)) 

# Descriptives for categorical variables (counts & percentages)

# Gender: 
  data_demo %>% dplyr::group_by(gender) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::mutate(perc = prop.table(n)*100 ) 
  # 4 men; 36 women
  
# Relationship/coparenting status:  
    data_demo %>% dplyr::count(couple_status) %>% dplyr::mutate(perc = prop.table(n)*100)
  # 30 in a couple; 10 single parents 
    
# Gender breakdown of relationship/coparenting status: 
    data_demo %>% group_by(gender) %>% dplyr::count(couple_status) %>% dplyr::mutate(perc = prop.table(n)*100)
    # All single parents are women
    
# Gender breakdown for relationships: 
    data_demo %>% filter(couple_status == "En couple") %>% dplyr::count(spouse_gender) %>% dplyr::mutate(perc = prop.table(n)*100)
    # 29 couples in man/woman relationship; 1 in woman/woman relationship
    
# Employment status:
  data_demo %>% dplyr::group_by(occupation) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::mutate(perc = prop.table(n)*100 ) 
  # 21 parents work full-time (= option 1)
  # 10 parents work part-time ( = option 2)
  # 1 parent unemployed ( = option 3)
  # 2 parent unable to work (disability) (= option 5)
  # 3 parent on parental leave (= option 7)
  # 3 parent a stay-at-home parent (= option 8)


# Net monthly income for the family:
  data_demo %>% count(income) %>% mutate(perc = prop.table(n)*100 )
  # income bracket 1 (1-1500EUR): 2 participants (5%)
  # income bracket 2 (1500-2500EUR): 9 participants (22.5%)
  # income bracket 3 (2500-3500EUR): 8 participants (20%)
  # income bracket 4 (3500-4500EUR): 11 people (27.5%)
  # income bracket 5 (+4500EUR): 7 people (17.5%)
  # income bracket 6 (prefer not to say): 3 people (7.5%)
  
# Psych treatment
  data_demo %>% count(psych_treatment) %>%mutate(perc = prop.table(n)*100)
  # 19 participants (47.5): answered 1 (="yes")
  # 21 participants (52.5): answered 2 (="no")
  
# Length of psych treatment
  data_demo$psych_treatment_length[data_demo$psych_treatment==1]
    # ranging from 3 months to 15 years; but mostly less than a year

#### 2.1 Kids' Ages Info  -----------------------------------------------------

data_demo_age <- data_demo %>%
  dplyr::mutate(age_eldestkid = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_eldestkid)), "years"), 1)) %>%
  dplyr::mutate(age_kid2 = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_kid2)), "years"), 1)) %>%
  dplyr::mutate(age_kid3 = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_kid3)), "years"), 1)) %>%
  dplyr::mutate(age_kid4 = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_kid4)), "years"), 1)) %>%
  dplyr::mutate(age_kid5 = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_kid5)), "years"), 1)) %>%
  dplyr::mutate(age_kid6 = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_kid6)), "years"), 1)) %>%
  dplyr::mutate(age_kid7 = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_kid7)), "years"), 1)) %>%
  dplyr::mutate(age_kid8 = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_kid8)), "years"), 1)) %>%
  dplyr::mutate(age_kid9 = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_kid9)), "years"), 1)) %>%
  dplyr::mutate(age_youngestkid = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_youngestkid)), "years"), 1)) %>%
  dplyr::mutate(age_onlykid = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_onlykid)), "years"), 1)) %>%
  dplyr::select(contains("age_"))

# Overall Mean & SD of kids' ages
data_demo_age %>% 
  tidyr::pivot_longer(everything(), names_to = "kid_number", values_to = "age", values_drop_na = TRUE) %>% 
  dplyr::summarise(dplyr::across(age, c(mean, sd))) # mean = 8.40 ; sd = 5.51

# Median:
median(as.vector(as.matrix(data_demo_age)), na.rm = T) #7.4

# Descriptive info on kids' ages
range(data_demo_age, na.rm = T) #0.2 to 20.2

# Getting the age of the youngest kid in months (exact) 
data_demo %>%
  dplyr::mutate(age_youngestkid = round(lubridate::time_length(difftime(lubridate::dmy_hm(ended), lubridate::ym(age_youngestkid)), "months"), 1)) %>%
  dplyr::select(age_youngestkid) %>% 
  dplyr::slice(which.min(age_youngestkid)) %>% 
  print # youngest: 2 months old (by end of study)

#### 2.3 Questionnaire Cut-offs -----------------------------------------------------

# Parental burnout:
  # Severe cut-off 
  sum(data_demo$PBA_total > 86.3) # 13 participants (32.5%)
  # Moderate severity
  sum(data_demo$PBA_total > 52.7) # 28 participants (70%)
  
# Balance of Risks & Resources:
  sum(data_demo$BR2 < 0) # 16 participants (40%)

# Generalized Anxiety Disorder Questionnaire
  # Moderate anxiety: 10-14
  sum(data_demo$GAD > 10) # 18 participants (45%)
  # Severe anxiety (over score of 15)
  sum(data_demo$GAD > 15) # 8 participants (20%)
  
# Beck Depression Inventory (BDI-II)
  # Severe to extreme depression:
  sum(data_demo$BDI > 31) # 4 participants (10%)
  # At least moderate depression:
  sum(data_demo$BDI > 21) #16 participants (40%)
  

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Exporting cleaned data files & descriptive info ------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# All numeric demographic descriptive information   
readr::write_csv(numeric_descriptives, here::here("data", "processed_sharing", "ESM2_PB_Demographic_descriptives.csv"))

# Saving dataframe just with ID and gender, to allow us to later make network with just mothers (as vast majority of sample): 
data_gender <- data_demo %>% dplyr::select(ID, gender)
readr::write_csv(data_gender, here::here("data", "processed_sharing", "ESM2_PB_gender"))
  