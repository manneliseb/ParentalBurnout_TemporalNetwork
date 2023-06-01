#---------------------------------------------------------------#
# Reading in raw data & creating processed data file            #
#---------------------------------------------------------------#

# This is the R code used to format and clean the data collected for this project: https://osf.io/5sgkr/

# More specifically, this code prepares the various data files (ESM data &
# demographic data) for further analysis. It takes as input the raw data = .csv
# files as generated from formr (where data collection took place) & takes out
# potentially identifying information, formats various columns (dates, etc), and
# otherwise cleans data.

# This dataset also includes data from one participant who switched midway
# through the study to another data collection platform (mpath) since this
# participant changed to a nocturnal work schedule & the ESM scheduled
# timepoints could not be changed within formr.

# We do not share the raw data files used in this R script on OSF, since they
# include potentially identifying info, but we do share the cleaned ESM data
# (output of this R script). 

# This script also includes questionnaire reliability information since that
# uses the raw/full data of the questionnaires

# Libraries: 
library(here) # for relative paths within project
library(tidyverse) # also loads lubridate (02)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Importing Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Demographic data
data_demo_raw <- readr::read_csv2(here::here("data", "raw", "demographics_PB.csv"))
enrolling_survey_batch2 <- readr::read_csv2(here::here("data", "raw", "Enrolling_survey_tz_anon_SecondBatch.csv")) #includes notification time info
enrolling_survey_batch1 <- readr::read_csv2(here::here("data", "raw", "enrolling_survey_anon_FirstBatch.csv")) #includes notification time info

# ESM data
data_mpath <- readr::read_csv2(here::here("data", "raw", "mpath", "basic.csv")) # participant who completed half of data collection via other app, mpath
data_formr <- readr::read_csv(here::here("data", "raw", "ESM_PB.csv"))

# Post study data
data_feedback <- readr::read_csv(here::here("data", "raw", "PostStudyQuestions_fr.csv"))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Processing data -----------------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 2.1 Demographic & Questionnaire: Formatting -----------------------------------------------------

# Formatting demographics info, getting subscale scores
data_demo <- data_demo_raw %>% 
  # taking out test entries & researcher accounts
  dplyr::filter(session != "") %>% 
  dplyr::filter(!grepl('testaccount', session, ignore.case = T)) %>%
  dplyr::select(-created, -modified, -expired) %>%
  # Shortening ID string
  dplyr::mutate(ID = stringr::str_sub(session, start = 7, end = 10)) %>% 
  dplyr::relocate(ID) %>%
  #Aggregating PBA features and total scores
  dplyr::mutate(PBA_Exh = rowSums(dplyr::select(., contains("_Exh")))) %>%
  dplyr::mutate(PBA_Dist = rowSums(dplyr::select(., contains("_Dist")))) %>%
  dplyr::mutate(PBA_FedUp = rowSums(dplyr::select(., contains("_Fed")))) %>%
  dplyr::mutate(PBA_Con = rowSums(dplyr::select(., contains("_Con"))))  %>%
  dplyr::mutate(PBA_total = rowSums(dplyr::select(., PBA_Exh, PBA_Dist, PBA_FedUp, PBA_Con)))  %>%
  # Aggregating GAD scores (-1 because they were recorded as 1-4 when in the scale they're scored 0-3)
  dplyr::mutate(GAD = rowSums(dplyr::select(., contains("GAD_"))-1)) %>%
  # Aggregating Balance betweeen Risks & Resources (BR2) scores
  dplyr:: mutate(BR2 = rowSums(dplyr::select(., contains("BRR_")))) %>%
  # Aggregating BDI scores (-1 because they were recorded as 1-4 when in the scale they're scored 0-3)
  dplyr::mutate(BDI = rowSums(dplyr::select(., contains("BDI2_"))-1)) %>%
  dplyr::select(-session, -(PBA1_Exh3:BDI2_21)) 


# KIDS' AGES - Getting age info of kids (originally as YYYY/MM) into years

# # first have to correct the info for participants who entered info in incorrect format
data_demo$age_eldestkid[data_demo$age_eldestkid == "03/2014"] <- "2014/03"
data_demo$age_kid2[data_demo$age_kid2 == "12/2017"] <- "2017/12"
data_demo$age_youngestkid[data_demo$age_youngestkid == "05/2022"] <- "2022/05"

# When parent just put year, adding month so that it's read correctly later; will add june (halfway through year)
data_demo$age_eldestkid[data_demo$age_eldestkid == "2005"] <- "2005/06"
data_demo$age_youngestkid[data_demo$age_youngestkid == "2008"] <- "2008/06"

# One parent put month wrong - making June (midway through year)
data_demo$age_kid2[data_demo$age_kid2 == "2010/27"] <- "2010/06"

# Two parents gave incorrect info; correcting after checking with them
data_demo$age_onlykid[data_demo$age_onlykid == "08/1970"] <- "2012/12"
data_demo$age_onlykid[data_demo$age_onlykid == "1987/01"] <- "2022/03"




### 2.2 Questionnaires (Reliability, etc)  -----------------------------------------------------

# Dataframe with just subscale/total demographic questionnaire scores....
demo_questionnaires <- data_demo %>% 
  dplyr::select(ID, PBA_Exh:BDI)

demo_rel <- data_demo_raw %>%
  # taking out test entries & researcher accounts
  dplyr::filter(session != "") %>% 
  dplyr::filter(!grepl('testaccount', session, ignore.case = T))

# Cronbach's alpha for all questionnaires:
demo_rel %>% dplyr::select(contains("PBA")) %>% psych::alpha(title = "PBA") # a = .97
demo_rel %>% dplyr::select(contains("GAD")) %>% psych::alpha(title = "GAD") # a = .89
demo_rel %>% dplyr::select(contains("BDI2")) %>% psych::alpha(title = "BDI2") # a = .88

# For PBA subscales (since reliability of subscales also reported in original article):
demo_rel %>% dplyr::select(contains("Exh")) %>% psych::alpha(title = "PBA_Exh") # a = .93
demo_rel %>% dplyr::select(contains("Dist")) %>% psych::alpha(title = "PBA_Dist") # a = .87
demo_rel %>% dplyr::select(contains("Fed")) %>% psych::alpha(title = "PBA_Fed") # a = .94
demo_rel %>% dplyr::select(contains("Con")) %>% psych::alpha(title = "PBA_Con") # a = .93

### 2.2 ESM  -----------------------------------------------------

#### 2.2.1 m-path data  -----------------------------------------------------
# One participant switched shift schedule midway through the study & we couldn't
# alter notification time in formr, so we switched that participant for half the
# study to m-path

data_mpath <- data_mpath %>%
  dplyr::filter(alias != "Blanchard") %>% # taking out test timestamps from me as researcher

  # Converting from UNIX to Date object (to match formr) + date column: 
  dplyr::mutate(
    timeStampStart = as.POSIXct(timeStampStart, origin = "1970-01-01"),
    timeStampStop = as.POSIXct(timeStampStop, origin = "1970-01-01"),
    date = as.Date(as.character(timeStampStart), format="%Y-%m-%d"),
    ID = "gCdQ") %>%
  

  dplyr::rename(created = timeStampStart, ended = timeStampStop) %>%
  dplyr::rename_with(~ sub("_sliderNegPos", "", .x), everything()) %>%
  dplyr::rename_with(~ sub("_sliderNeutralPos", "", .x), everything()) %>%
  dplyr::select(ID, created, ended, date, ESM_exhaustion:ESM_timewithkids) 


#### 2.2.2 formr data   -----------------------------------------------------

# Getting dataframe with ID & preferred time for each participant
preferred_time_batch1 <- enrolling_survey_batch1 %>% 
  # taking out researcher accounts & test accounts (no data)
  dplyr::filter(!is.na(preferred_time)) %>% 
  dplyr::filter(!grepl('testaccount', session, ignore.case = T)) %>%
  dplyr::filter(session != "") %>% 
  
  #just keep first 4 letters of session ID and time
  dplyr::mutate(ID = stringr::str_sub(session, start = 7, end = 10)) %>% 
  dplyr::select(ID, preferred_time)

preferred_time_batch2 <- enrolling_survey_batch2 %>% 
  # taking out researcher accounts & test accounts (no data)
  dplyr::filter(!is.na(preferred_time)) %>% 
  dplyr::filter(!grepl('testaccount', session, ignore.case = T)) %>%
  dplyr::filter(session != "") %>% 
  
  #just keep first 4 letters of session ID and time
  dplyr::mutate(ID = stringr::str_sub(session, start = 7, end = 10)) %>% 
  dplyr::select(ID, preferred_time)

# Joining two preferred time datasets...
preferred_time_df <- rbind(preferred_time_batch1, preferred_time_batch2)

# Preparing ESM data (e.g., turning 'session' column into 'ID' with only 4 characters, taking out test accounts)

ESM_clean <- data_formr %>%
  dplyr::filter(!grepl('testaccount', session, ignore.case = T)) %>%
  dplyr::filter(session != "") %>% # session ID can't be blank (means not a real participant)
  dplyr::filter(!is.na(ESM_exhaustion)) %>% #taking out timepoints without answers (since participants had to answer to submit the questionnaire)
  dplyr::mutate(ID = stringr::str_sub(session, start = 7, end = 10)) %>%
  dplyr::mutate(date = ifelse(is.na(ended), as.Date(created), as.Date(ended))) %>% # creating a date column (without time)
  dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>% # Making sure 'date' column is formatted as a date 

  # combine with m-path data
  dplyr::bind_rows(data_mpath) %>%
  
  # take out session
  dplyr::select(-session) %>%
  
  # Adding column with preferred time for each participant  
  dplyr::left_join(., preferred_time_df, by = "ID")
  



#### 2.2.3 Combined ESM data   -----------------------------------------------------

ESM_clean <- ESM_clean %>%
  # Counting it as the day before when participant answered before 4AM
  dplyr::mutate(date = dplyr::case_when(is.na(ended) & lubridate::hour(created) < 4 ~ date - 1,
                           is.na(ended) & lubridate::hour(created) > 4 ~ date,
                           !is.na(ended) & lubridate::hour(ended) < 4 ~ date - 1,
                           !is.na(ended) & lubridate::hour(ended) > 4 ~ date)) %>%
  
  # Creating a 'time to complete' column (how long it took them to answer after opening the web page)
  dplyr::mutate(time_to_complete = difftime(lubridate::ymd_hms(ended), lubridate::ymd_hms(created), units='mins')) %>%
  
  # Creating a 'first day' column (the day each participant started)
  dplyr::group_by(ID) %>% 
  dplyr::mutate(first_day = min(lubridate::date(created))) %>%
  dplyr::ungroup() %>%
  
  dplyr::mutate(n_obs = as.integer(date - first_day + 1))  %>%  # Timepoint, counting from first day
  dplyr::mutate(weekday = weekdays(date)) %>%
  
  # Adding column with notification time (ALTHOUGH the system did sometimes send them out a few hours late when there were errors)
  dplyr::mutate(notification_time = date + lubridate::hours(preferred_time)) %>%
  dplyr::mutate(time_to_click = difftime(lubridate::ymd_hms(created), notification_time, units='mins')) 
  

#### 2.2.4 Hours since submitted last survey + reversing   -----------------------------------------------------

# So don't have to include exact date/times, starting a new column with # of
# hours since first survey (so have exact length of time)
ESM_clean <- ESM_clean %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(firstTimepoint = min(date) + lubridate::hours(preferred_time)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(NumberHours = difftime(ended, firstTimepoint, units = "hours"))

# Reversing distance (formulated as 'closeness') & resources (phrased as a lack of)
ESM_clean <- ESM_clean %>%
  dplyr::mutate(ESM_distance_Rev = 100 - ESM_distance) %>%
  dplyr::mutate(ESM_resources_Rev  = 100 - ESM_resources)

#### 2.2.5 ESM: Removing duplicates & adding NAs for missed days  -----------------------------------------------------

# Check to see if there are any duplicates & how many: 
n_duplicates <- ESM_clean %>% 
  dplyr::arrange(ID, created) %>%
  dplyr::group_by(ID, n_obs) %>% 
  dplyr::mutate(num_dups = dplyr::n(), 
         dup_id = dplyr::row_number()) %>% 
  dplyr::ungroup() %>% 
  subset(num_dups > 1) %>% dplyr::count() %>% as.numeric

print(paste0("There were ", n_duplicates, " duplicate ESM entries (two entries or more for one timepoint/observation)."))
    # There are 2 duplicates - this is someone who answered at 11h and then
    # 21h on the same day, but did not answer at all the previous day (must not
    # have expired correctly).

# Since mlVAR doesn't like two datapoints with same observation/day #, will
# delete the first (from 11h) - as the participant shouldn't have been able to
# answer then in any case...
ESM_clean <- ESM_clean %>% filter(as.character(created)!="2023-01-15 11:53:36")

# Add NAs for days where participant did not answer
ESM_clean <- ESM_clean %>% 
  dplyr::arrange(ID, n_obs) %>%
  tidyr::complete(ID, n_obs = 1:56) %>%
  dplyr::select(-created)

summary(ESM_clean) #length should = # of pns * max # of observations 
  # But in this dataset, there are a few people with 57 timepoints (added one if they missed one during the study & requested an additional one)


    
#### 2.2.6 ESM: Checking exclusions  -----------------------------------------------------
# Checking all parents have answered at least 30% of ESM prompts; if not,
# excluding them (as preregistered)
ESM_clean %>%
  dplyr::filter(!is.na(ESM_exhaustion)) %>% # taking out days they didn't answer
  dplyr::group_by(ID) %>%
  dplyr::count() %>%
  dplyr::filter(n < (56*.3))    
  # 1 participant only answered 5 items (withdrew from study)

# Removing this participant from datasets: 
ESM_clean <- ESM_clean %>% dplyr::filter(ID!="-yPo")
demo_questionnaires <- demo_questionnaires %>% dplyr::filter(ID!="-yPo")
data_demo <- data_demo %>% dplyr::filter(ID!="-yPo")

#### 2.2.7 ESM: Generating saved datasets  -----------------------------------------------------

# First dataset: Only with anonymized data & variables relevant for further analyses

ESM_clean_anon <- ESM_clean %>%
  dplyr::select(ID, n_obs, NumberHours, weekday, ESM_exhaustion, ESM_distance_Rev, ESM_fedup, ESM_partnersupport,
        ESM_partnerconflict, ESM_kids, ESM_bxtokids_pos, ESM_bxtokids_neg, ESM_resources_Rev, ESM_socialsupport,
        ESM_timewithkids, ESM_hours_slept, time_to_complete, time_to_click) 

# Second dataset: Only for internal use, with exact date/times
ESM_clean_exporting <- ESM_clean %>%
  dplyr::select(ID, n_obs, NumberHours, weekday, ESM_exhaustion, ESM_distance_Rev, ESM_fedup, ESM_partnersupport,
                ESM_partnerconflict, ESM_kids, ESM_bxtokids_pos, ESM_bxtokids_neg, ESM_resources_Rev, ESM_socialsupport,
                ESM_timewithkids, ESM_hours_slept, ESM_context, time_to_complete, time_to_click) 


#### 2.2.8 ESM: Getting time when study ran  -----------------------------------------------------
 # Removing specific date/times for shared dataset, so including this code here where still have date/time info...

# Dates of participation
participant_first <- lubridate::as_date(min(ESM_clean$date, na.rm = TRUE))
participant_last  <- lubridate::as_date(max(ESM_clean$date, na.rm = TRUE))

length_esm_study <- as.numeric(participant_last - participant_first)

print(paste0("ESM data was collected between ", participant_first, " and ", participant_last, "."))
#"ESM data was collected between 2022-03-17 and 2023-03-08."

    
### 2.3 Feedback questionnaire (post ESM): Cleaning  -----------------------------------------------------
    
feedback <- 
  data_feedback %>% 
  dplyr::filter(!is.na(Feedback_burden)) %>% #taking out any participants who didn't answer the first question (it was mandatory to answer it to submit, means they didn't answer anything/were a test account)
  dplyr::filter(!grepl('testaccount', session, ignore.case = T)) %>%
  # Shortening ID string
  dplyr::mutate(ID = stringr::str_sub(session, start = 7, end = 10)) %>% 
  dplyr::select(-(session:expired)) %>%
  dplyr::relocate(ID) 



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) Exporting cleaned data files & descriptive info ------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Demographic Information
readr::write_csv(data_demo, here::here("data", "processed", "ESM2_PB_Demographic_General.csv"))

# Questionnaires per participant (to look at correlations with ESM data)
readr::write_csv(demo_questionnaires, here::here("data", "processed", "ESM2_PB_Demographic_questionnaires.csv"))
    
# ESM data - full info
readr::write_csv(ESM_clean_exporting, here::here("data", "processed", "ESM2_PB_clean.csv"))    

# ESM data - more fully de-identified
readr::write_csv(ESM_clean_anon, here::here("data", "processed_sharing", "ESM2_PB_clean_anon.csv"))    

# Feedback post-ESM survey
readr::write_csv(feedback, here::here("data", "processed", "ESM2_PB_feedback.csv"))    


    
    
    
    