# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Summary Statistics (ESM) and Assumption Checking           
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This R code computes ESM summary statistics & assesses assumptions for the ESM
# data from this project: https://osf.io/5sgkr/

# Summary statistics include compliance rates, time to answer ESM questions,
# participant-specific means/sds/etc, ICCs, etc. Assumptions include especially
# normality & stationarity.


# Libraries: 
library(here) # for relative paths within project
library(tidyverse) 
library(psychometric) # calculate ICCs 
library(qgraph) # for cor_auto 
library(networktools) # for goldbricker function, i.e. node redundancy 
library(moments) # used for skewness/kurtosis 
library(LambertW) #to correct non-normal data 
library(tseries) #for KPSS stationarity test 
library(stats) # for Shapiro-Wilkes test + linear models when looking at stationarity

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Importing Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Importing ESM data
ESM_clean <- readr::read_csv(here::here("data", "processed_sharing", "ESM2_PB_clean_anon.csv"))

# selecting relevant columns, renaming columns for Tables/etc
ESM <- ESM_clean %>%
  dplyr::select (ID, n_obs, NumberHours, weekday, Exh = ESM_exhaustion, Dist = ESM_distance_Rev, FedUp = ESM_fedup, PartSup = ESM_partnersupport,
                 PartConf = ESM_partnerconflict, DiffKids = ESM_kids, PosMoKids = ESM_bxtokids_pos, AngKids = ESM_bxtokids_neg, 
                 Res = ESM_resources_Rev, SocSup = ESM_socialsupport, TimeKids = ESM_timewithkids, time_to_complete)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Descriptive Information -------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 2.1 Compliance & timepoint Info -----------------------------------------------------

# Time to complete
mean(ESM$time_to_complete, na.rm = TRUE) #16.71 min
max(ESM$time_to_complete, na.rm = TRUE) # at least one big outlier: 339 min (almost 6 hours)

median(ESM$time_to_complete, na.rm = TRUE) #1.38 min (very similar to unselected sample from prev study)

# Compliance rate (overall)
compliance = ESM %>%
  dplyr::filter(!is.na(Exh)) %>%
  dplyr::group_by(ID) %>%
  dplyr::count() %>%
  dplyr::mutate (compliance = n / 56)

print (paste0("mean compliance overall :", round(mean (compliance$compliance, na.rm = TRUE), 2) ))
# "mean compliance overall :0.93"


# Total # of observations (where participants answered ESM questions)
dplyr::count(tidyr::drop_na(ESM)) #1541

# Number of obs per person
obs_per_person <- ESM %>% 
  dplyr::group_by(ID) %>% 
  dplyr::filter(!is.na(Exh)) %>%# only include days participants answer survey
  dplyr::count(.) %>%
  dplyr::mutate(PercAnswered = round(n/56*100, 0)) 

obs_per_person %>% dplyr::filter(PercAnswered < 80) 
# Only 1 participants answered < 80% of questionnaires (77%)

mean(obs_per_person$n) # mean obs answered = 51.82 [also = to mean days in study]



### 2.2 Participant specific info (Mean, SDs, etc) -----------------------------------------------------

# Mean, SDs, min & max of variables PER PARTICIPANT in a table
mean_data_pn <- ESM %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise (mean_Exh = mean(Exh, na.rm=TRUE),  sd_Exh = sd(Exh, na.rm=TRUE), min_Exh = min(Exh, na.rm = TRUE), max_Exh = max(Exh, na.rm = T),
             mean_Dist = mean(Dist, na.rm=TRUE),  sd_Dist = sd(Dist, na.rm=TRUE), min_Dist = min(Dist, na.rm = TRUE), max_Dist = max(Dist, na.rm = T),
             mean_FedUp = mean(FedUp, na.rm=TRUE),  sd_FedUp = sd(FedUp, na.rm=TRUE), min_FedUp = min(FedUp, na.rm = TRUE), max_FedUp = max(FedUp, na.rm = T),
             mean_PartSup = mean(PartSup, na.rm=TRUE),  sd_PartSup = sd(PartSup, na.rm=TRUE), min_PartSup = min(PartSup, na.rm = TRUE), max_PartSup = max(PartSup, na.rm = T),
             mean_PartConf = mean(PartConf, na.rm=TRUE),  sd_PartConf = sd(PartConf, na.rm=TRUE), min_PartConf = min(PartConf, na.rm = TRUE), max_PartConf = max(PartConf, na.rm = T),
             mean_DiffKids = mean(DiffKids, na.rm=TRUE),  sd_DiffKids = sd(DiffKids, na.rm=TRUE), min_DiffKids = min(DiffKids, na.rm = TRUE), max_DiffKids = max(DiffKids, na.rm = T),
             mean_PosMoKids = mean(PosMoKids, na.rm=TRUE),  sd_PosMoKids = sd(PosMoKids, na.rm=TRUE), min_PosMoKids = min(PosMoKids, na.rm = TRUE), max_PosMoKids = max(PosMoKids, na.rm = T),
             mean_AngKids = mean(AngKids, na.rm=TRUE),  sd_AngKids = sd(AngKids, na.rm=TRUE), min_AngKids = min(AngKids, na.rm = TRUE), max_AngKids = max(AngKids, na.rm = T),
             mean_Res = mean(Res, na.rm=TRUE),  sd_Res = sd(Res, na.rm=TRUE), min_Res = min(Res, na.rm = TRUE), max_Res = max(Res, na.rm = T),
             mean_Social = mean(SocSup, na.rm=TRUE),  sd_Social = sd(SocSup, na.rm=TRUE), min_Social = min(SocSup, na.rm = TRUE), max_Social = max(SocSup, na.rm = T),
             mean_TimeKids = mean(TimeKids, na.rm = TRUE), sd_TimeKids = sd(TimeKids, na.rm = TRUE), min_TimeKids = min(TimeKids, na.rm = TRUE), max_TimeKids = max(TimeKids, na.rm = T))

# (MAB note - min/max going to infinity for partner-related means bc not all parents answered partner-related ESM items)


# INTRAINDIVIDUAL MEANS & SDs (iiM & iiSD)
iimean_data <- mean_data_pn %>% 
  dplyr::summarise(dplyr::across(mean_Exh:max_TimeKids, mean, na.rm = T))

iimean_data <- iimean_data %>% 
  tidyr::pivot_longer(starts_with("mean") | starts_with("sd") | starts_with("min") | starts_with("max"), 
               names_to = c("Stat","ESM_item"), 
               names_sep = "_",
               values_to = "score") %>%
  tidyr::pivot_wider(names_from = Stat, values_from = score )


### 2.3 Intraclass Correlation Coefficients (ICC) -----------------------------------------------------

# = proportion of the total variability (between +  within person variability) due to between-person variability
# ICC = between variance(intercept)/between variance+within variance(=residual)

# This specifically is the ICC1 with an empty model, as specific by Gabriel et al. (2019) as common to use for ESM data

ICC_exhaust <- psychometric::ICC1.lme(Exh, ID, ESM) 
ICC_distance <- psychometric::ICC1.lme(Dist, ID, ESM) 
ICC_fedup <- psychometric::ICC1.lme(FedUp, ID, ESM)
ICC_partnersupport <- psychometric::ICC1.lme(PartSup, ID, ESM)
ICC_partnerconflict <- psychometric::ICC1.lme(PartConf, ID, ESM)
ICC_kids <- psychometric::ICC1.lme(DiffKids, ID, ESM)
ICC_bxtokids_pos <- psychometric::ICC1.lme(PosMoKids, ID, ESM)
ICC_bxtokids_neg <- psychometric::ICC1.lme(AngKids, ID, ESM)
ICC_resources <- psychometric::ICC1.lme(Res, ID, ESM)
ICC_socialsupport <- psychometric::ICC1.lme(SocSup, ID, ESM)
ICC_timewithkids <- psychometric::ICC1.lme(TimeKids, ID, ESM)

ICC_data =  as.data.frame(rbind(ICC_exhaust, ICC_distance, ICC_fedup,ICC_partnersupport, ICC_partnerconflict, ICC_kids, ICC_bxtokids_pos,
                                ICC_bxtokids_neg, ICC_resources, ICC_socialsupport, ICC_timewithkids))
colnames(ICC_data) = "ICC"


# Getting a table for the intra-individual means & SDs, and ICCs for each variable
table_desc <- dplyr::bind_cols(iimean_data, ICC_data)
table_desc <- dplyr::mutate(table_desc, dplyr::across(where(is.numeric), round, 2))



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) Assumption Checks -------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### 3.1 Redundancy check between nodes -----------------------------------------------------

# (Using Goldbricker function; used in cross-sectional networks)

corpcor::is.positive.definite(qgraph::cor_auto(ESM[,5:15])) #true
gb <- networktools::goldbricker(ESM[,5:15], p = 0.05, method = "hittner2003", threshold=0.25, corMin=.50)
gb # no suggested reductions 


### 3.2 Assessing Normality -----------------------------------------------------

# First, just visually check histograms of raw data: 
variables_names  = c("Exh", "Dist", "FedUp","PartSup","PartConf","DiffKids",
                     "PosMoKids","AngKids","Res","SocSup","TimeKids") 

for (v in variables_names){
  hist(ESM[[v]], main = paste0("Histogram: ", v))
} 
# Overall: none look normal, very strong tails etc

#### 3.2.1 Shapiro-Wilk tests on residuals of mlVAR model  -----------------------------------------------------

# As preregistered, performing "Shapiro-Wilk tests to check whether the
# residuals of the multilevel VAR model are normally distributed for every
# variable" (https://osf.io/52wx3)

# Note - pulling the residuals from the saved R .rds object from estimated model (from script "05_MainAnalyses_mlVARNetworkModel") 
network_mothers <- readr::read_rds(here::here("output", "R_Objects", "network_mothers.rds"))

stats::shapiro.test(residuals(network_mothers$output$temporal$Exh))
  # Example with correcting for multiple testing with Bonferroni: 
  round(p.adjust(shapiro.test(residuals(network_mothers$output$temporal$Exh)), "bonferroni"), 10) # Bonferroni corrected - no difference
stats::shapiro.test(residuals(network_mothers$output$temporal$Dist))
stats::shapiro.test(residuals(network_mothers$output$temporal$FedUp))
# (Note: No residuals for PartSup & PartConf as they are not estimated in the main model)
stats::shapiro.test(residuals(network_mothers$output$temporal$DiffKids))
stats::shapiro.test(residuals(network_mothers$output$temporal$PosMoKids))
stats::shapiro.test(residuals(network_mothers$output$temporal$AngKids))
stats::shapiro.test(residuals(network_mothers$output$temporal$Res))
stats::shapiro.test(residuals(network_mothers$output$temporal$SocSup))

#### 3.2.2 QQplots: residuals of mlVAR model  -----------------------------------------------------

# Also preregistered visually examining plots of residuals:
qqnorm(residuals(network_mothers$output$temporal$Exh)); qqline(residuals(network_mothers$output$temporal$Exh))
qqnorm(residuals(network_mothers$output$temporal$Dist)); qqline(residuals(network_mothers$output$temporal$Dist))
qqnorm(residuals(network_mothers$output$temporal$FedUp)); qqline(residuals(network_mothers$output$temporal$FedUp))
qqnorm(residuals(network_mothers$output$temporal$DiffKids)); qqline(residuals(network_mothers$output$temporal$DiffKids))
qqnorm(residuals(network_mothers$output$temporal$PosMoKids)); qqline(residuals(network_mothers$output$temporal$PosMoKids))
qqnorm(residuals(network_mothers$output$temporal$AngKids)); qqline(residuals(network_mothers$output$temporal$AngKids))
qqnorm(residuals(network_mothers$output$temporal$Res)); qqline(residuals(network_mothers$output$temporal$Res))
qqnorm(residuals(network_mothers$output$temporal$SocSup)); qqline(residuals(network_mothers$output$temporal$SocSup))

# Note - the residuals for exhaustion & resources look relatively normal; all others have a tail/etc 


#### 3.2.3 Within-person centered data -----------------------------------------------------

# Because we're collecting many measurements within-person, it makes sense to
# look at normality of raw data after within-person centering (as that is what
# is done in the mlVAR model after all)

# First, centering within person (adapting code from https://kzee.github.io/Centering_Demo.html)
ESM_cent <- ESM %>% group_by(ID) %>%
  mutate(
    Exh_wmean = mean(Exh, na.rm = T),
    Dist_wmean = mean(Dist, na.rm = T),
    Exh_wmean = mean(Exh, na.rm = T),
    FedUp_wmean = mean(FedUp, na.rm = T),
    PartSup_wmean = mean(PartSup, na.rm = T),
    PartConf_wmean = mean(PartConf, na.rm = T),
    DiffKids_wmean = mean(DiffKids, na.rm = T),
    PosMoKids_wmean = mean(PosMoKids, na.rm = T),
    AngKids_wmean = mean(AngKids, na.rm = T),
    Res_wmean = mean(Res, na.rm = T),
    SocSup_wmean = mean(SocSup, na.rm = T)
  ) %>% ungroup() %>%
  mutate(
    Exh_cw = Exh - Exh_wmean,
    Dist_cw = Dist - Dist_wmean,
    FedUp_cw = FedUp - FedUp_wmean,
    PartSup_cw = PartSup - PartSup_wmean,
    PartConf_cw = PartConf - PartConf_wmean,
    DiffKids_cw = DiffKids - DiffKids_wmean,
    PosMoKids_cw = PosMoKids - PosMoKids_wmean,
    AngKids_cw = AngKids - AngKids_wmean,
    Res_cw = Res - Res_wmean,
    SocSup_cw = SocSup - SocSup_wmean
  )

# Also checking histograms: 
  variables_names_cent  = c("Exh_cw", "Dist_cw", "FedUp_cw","PartSup_cw","PartConf_cw","DiffKids_cw",
                            "PosMoKids_cw","AngKids_cw","Res_cw","SocSup_cw") 
  
  for (v in variables_names_cent){
    hist(ESM_cent[[v]], main = paste0("Histogram: ", v))
  } 
  # Overall: much more like normal dist, although all/many still skewed etc (but no high tails)

# Checking normality of within-person centered data with Shapiro-Wilk test
stats::shapiro.test(ESM_cent$Exh_cw)
  # Example with correcting for multiple testing with Bonferroni: 
  round(p.adjust(shapiro.test(ESM_cent$Exh_cw), "bonferroni"), 10) # Bonferroni corrected - no difference
stats::shapiro.test(ESM_cent$Dist_cw)
stats::shapiro.test(ESM_cent$FedUp_cw)
stats::shapiro.test(ESM_cent$PartSup_cw)
stats::shapiro.test(ESM_cent$PartConf_cw)
stats::shapiro.test(ESM_cent$DiffKids_cw)
stats::shapiro.test(ESM_cent$PosMoKids_cw)
stats::shapiro.test(ESM_cent$AngKids_cw)
stats::shapiro.test(ESM_cent$Res_cw)
stats::shapiro.test(ESM_cent$SocSup_cw)


# Look at skewness & kurtosis of each variable
skew_kurt = data.frame()
for (v in variables_names_cent[1:10]){
  var_norm = c(moments::skewness(ESM_cent[[v]], na.rm = T), moments::kurtosis(ESM_cent[[v]], na.rm = T))
  skew_kurt = rbind(skew_kurt, var_norm)
}
colnames(skew_kurt) <- c("Skew", "Kurtosis")
rownames(skew_kurt) <- print(variables_names_cent[1:10])
print(skew_kurt)
# Almost all skew values are between -1 and 1, but many kurtosis values over 3

# FYI: When looking at
# https://www.r-bloggers.com/2020/01/a-guide-to-data-transformation/, the most
# important is typically to correct skew; deviations of the tail (e.g.,
# kurtosis) are usually less critical to transform


#### 3.2.4 Correcting Normality -----------------------------------------------------

# To correct: 
# - log transform specific items that have skew or kurtosis outside acceptable range of -2 to 2
# - info from here: https://www.r-bloggers.com/2020/01/a-guide-to-data-transformation/

# Log transformation (preregistered transformation)
# - Within-person mean centered variables have negative/0 values, so can't perform easily

# Trying an automatic transformation 
# Gaussian transformation (from LambertW package; article: https://doi.org/10.1155/2015/909231 )

# Variable names:
variables_names_norm  = c("Exh_norm", "Dist_norm", "FedUp_norm","PartSup_norm","PartConf_norm","DiffKids_norm",
                          "PosMoKids_norm","AngKids_norm","Res_norm","SocSup_norm") 

ESM_cent_norm <- ESM_cent %>%
  drop_na(!Exh_cw) %>%  # Gaussianize will not take NAs so taking them out for now
  dplyr::mutate(
    Exh_norm = LambertW::Gaussianize(data = Exh_cw, type = "s", method = "MLE"),
    Dist_norm = LambertW::Gaussianize(data = Dist_cw, type = "s", method = "MLE"),
    FedUp_norm = LambertW::Gaussianize(data = FedUp_cw, type = "s", method = "MLE"),
    PartSup_norm = LambertW::Gaussianize(data = PartSup_cw, type = "s", method = "MLE"),
    PartConf_norm = LambertW::Gaussianize(data = PartConf_cw, type = "s", method = "MLE"),
    DiffKids_norm = LambertW::Gaussianize(data = DiffKids_cw, type = "hh", method = "MLE"),
    PosMoKids_norm = LambertW::Gaussianize(data = PosMoKids_cw, type = "s", method = "MLE"),
    AngKids_norm = LambertW::Gaussianize(data = AngKids_cw, type = "h", method = "MLE"),
    Res_norm = LambertW::Gaussianize(data = Res_cw, type = "s", method = "MLE"),
    SocSup_norm = LambertW::Gaussianize(data = SocSup_cw, type = "hh", method = "MLE")) %>%
  dplyr::mutate(across(.cols = all_of(variables_names_norm), as.numeric))

# Skewness & kurtosis for each variable
skew_kurt_norm = data.frame()
for (v in variables_names_cent){
  var_norm = c(moments::skewness(ESM_cent_norm[[v]]), moments::kurtosis(ESM_cent_norm[[v]]))
  skew_kurt_norm = rbind(skew_kurt_norm, var_norm)
}
colnames(skew_kurt_norm) <- c("Skew", "Kurtosis")
rownames(skew_kurt_norm) <- print(variables_names_norm)
print(skew_kurt_norm) 

# Note: When comparing to skew_kurt (from within-person centered raw data): 
# - Skew is worse for Dist; PosMoKids; AngKids; Res; SocSup
# - Kurtosis is worse for Exh; Dist; FedUp; PosMoKids; Res; SocSup
# And in general, the values are very similar overall


# Checking if data now normal: 
for (v in variables_names_norm){
  hist(ESM_cent_norm[[v]], main = paste0("Histogram: ", v))
  print(paste0(v, "  p-value: ", shapiro.test(ESM_cent_norm[[v]])[2]))
} 
# Still no variables normal even after transformation

# Because the transformation does not much help the data, and because most of
# the raw data (within-person centered) is within skew of -2 and 2 and kurtosis
# of -5 to 5 (with worse values for the corrected data), we won't do analyses
# with corrected data

# Preparing table with skewness/kurtosis values to export
skew_kurt_table <- round(rbind(skew_kurt, skew_kurt_norm),2)

### 3.3 Assessing Stationarity -----------------------------------------------------

# This code was adapted from code created by Yorgo Hoebeke to check & correct non-stationary ESM data

# Part 1: 
# Use KPSS test to check for trend stationarity

# Part 2: Correct nonstationarity data
# To correct: 
# - replace values with residuals from a linear regression model that has the
#   non-stationary time series as the outcome variable & the exact measurement
#   time as predictor variable
# - also add within-person mean value of non-detrended time series to each
#   detrended series (so no mean level of zero)


# Adding within-person means to the data
# (In case need to detrend data, add the WP mean to the residuals so there's no mean level = 0)
data_corrected <- ESM %>%
  group_by(ID) %>% 
  dplyr::mutate(dplyr::across(.cols = all_of(variables_names), ~ mean(.x, na.rm=TRUE), .names = "{.col}.wp_mean"))

esm_before_stationary <- data_corrected
esm_after_detrend <- data_corrected
data_stat <- data_corrected


{
  bonferroni_n <- 10 # for each variable 
  kpss_tests <- list()
  kpss_p <- list()
  
  # loop over each variable
  for(y in names(subset(data_stat, select = variables_names))){
    # test stationarity
    kpss_p_values <- c()
    lm_trends <- list()
    lm_summary <- list()
    
    nid <- 1
    
    #KPSS Test for Trend Stationarity
    # MAB - below link doesn't work; remove? 
    # https://nwfsc-timeseries.github.io/atsa-labs/sec-boxjenkins-kpss.html
    # H0: trend stationarity 
    # H1: not trend stationary
    for(i in unique(data_stat$ID))
    {
      # test whether there is a significant trend in the answers of the subject so we can adapt the null hypothesis of the kpss test
      Data <- data_stat[data_stat$ID == i,]
      # first to avoid errors, test whether mean of participant answers = 0, sd > 0, and all answers aren't NA 
      if(mean(Data[[y]], na.rm = TRUE) > 0 & sd(Data[[y]], na.rm = TRUE) > 0 & !all(is.na(Data[[y]]))){
        lm_trends[[y]] <- stats::lm(as.formula(paste0(y," ~ n_obs")), data = Data, na.action=na.omit) 
        lm_summary[[y]] <- summary(lm_trends[[y]])
        
        # if there is a trend, then we test for trend stationarity
        # lm_summary[[y]]$coefficients[2,4] -> gives the p.value of the regression
        if(lm_summary[[y]]$coefficients[2,4]< 0.05/bonferroni_n ){
          print(paste0("Subject ", i, " has a trend (will check if it's nonstationary)")) 
          
          kpss_tests[[y]][[nid]] <- tseries::kpss.test(na.exclude(data_stat[[y]][data_stat$ID == i]), lshort = TRUE, null = "Trend")
          kpss_p_values <- c(kpss_p_values, kpss_tests[[y]][[nid]]$p.value)
          
        } else {
          # otherwise
          print(paste0("Subject ", i, " has no trend for variable ", y)) 
          kpss_tests[[y]][[nid]] <- tseries::kpss.test(na.exclude(data_stat[[y]][data_stat$ID == i]), lshort = TRUE, null = "Level")
          kpss_p_values <- c(kpss_p_values, kpss_tests[[y]][[nid]]$p.value)
        }
        
      } else{
        print(paste0("Subject ", i, " has a mean or SD of 0 for the variable ", y))
        # MAB - removing these bc there's an error if you run them but the pn has all NAs (e.g., single parents for PartSup or PartConf)
        #kpss_tests[[y]][[nid]] <- tseries::kpss.test(na.exclude(data_stat[[y]][data_stat$ID == i]), lshort = TRUE, null = "Level")
        #kpss_p_values <- c(kpss_p_values, kpss_tests[[y]][[nid]]$p.value)
      }
      
      
      nid <- nid + 1
    }
    # put them together in matrix
    kpss_p[[y]]  <- cbind(unique(Data$ID),kpss_p_values )
    print(paste0(y, " : ", sum(kpss_p[[y]] [,2] < 0.05/bonferroni_n), " participants with non stationary data"))
    
    
    #_______________________________________________________
    # DETREND for participants that have non stationary data
    
    detrendslm_trends <- list()
    detrendslm_summary <- list()
    detrendslm_trends1 <- list()
    
    for(i in 1:length(unique(kpss_p[[y]][,1])[kpss_p[[y]][,2] < 0.05/bonferroni_n]))
    {
      
      # only do this if there are participants with non-stationary data for that particular variable: 
      if((sum(kpss_p[[y]] [,2] < 0.05/bonferroni_n) > 0)) { 
        
        Data1 <- data_stat[data_stat$ID == unique(kpss_p[[y]][,1])[kpss_p[[y]][,2] < 0.05/bonferroni_n][i],]
        
        detrendslm_trends[[y]] <- lm(as.formula(paste0(y," ~ n_obs")), data = Data1)
        plot(Data1$n_obs,Data1[[y]], type="b", ylim=c(0,100), main = "before")
        abline(detrendslm_trends[[y]], col="red")
        print(paste0("Looping through subject #", i, " for variable ", y))
        
        
        Data1[[y]][!is.na(Data1[[y]])] <- residuals(detrendslm_trends[[y]])
        
        detrendslm_trends1[[y]] <- lm(as.formula(paste0(y," ~ n_obs")), data = Data1)
        
        plot(Data1$n_obs,Data1[[y]], type="b", ylim=c(-100,100), main = "after")
        abline(detrendslm_trends1[[y]], col="red")
        
        replace <- as.data.frame(data_stat[data_stat$ID == unique(kpss_p[[y]][,1])[kpss_p[[y]][,2] < 0.05/bonferroni_n][i], y])
        
        replace[!is.na(replace)] <- Data1[[y]][!is.na(Data1[[y]])]
        replace <- replace + Data1[[paste0(y,".wp_mean")]][kpss_p[[y]][,2] < 0.05/bonferroni_n][i]
        
        length(Data1$n_obs)
        
        plot(Data1$n_obs, replace[[y]], type="b", ylim=c(-100,100), main = "after + mean")
        abline(detrendslm_trends1[[y]], col="red")
        
        
        esm_after_detrend[esm_after_detrend$ID == unique(kpss_p[[y]][,1])[kpss_p[[y]][,2] < 0.05/bonferroni_n][i], y] <- as.integer(replace[[y]])
        print(past0("Detrended data for participant "))
      } else {
        print(paste0("No participants needed detrending for variable ", y))
      }
    }
    
  }
  
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4) Preparing ESM summary stats from previous study (unselected sample) -----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### 4.1 Previous dataset (genpop) --------------------------------------------

# Data from unselected participants (i.e., generap population), from data
# collected previously with same methodology (full project info:
# https://osf.io/pshdn/)

# Importing data (not sharing in project data file because already accessible
# online here: https://osf.io/e5qru)
  ESM_genpop <- readr::read_csv(here::here("data", "raw", "ESM_pb_clean_FromUnselectedSample.csv"))
# Also PBA scores per participant (not publically available):
  PBA_genpop <- readr::read_csv(here::here("data", "raw", "ESM_pb_demographic_questionnaires_8w_FromUnselectedSample.csv"))

# Formatting names in similar way + reversing pertinent variables
ESM_genpop <- ESM_genpop %>%
  dplyr::select (ID, Exh = ESM_exhaustion, Dist = ESM_distance, FedUp = ESM_fedup, PartSup = ESM_partnersupport,
                 PartConf = ESM_partnerconflict, DiffKids = ESM_kids, PosMoKids = ESM_bxtokids_pos, AngKids = ESM_bxtokids_neg, 
                 Res = ESM_resources, SocSup = ESM_socialsupport, TimeKids = ESM_timewithkids, time_to_complete) %>%
  #reversing Distance + Resources to match PB dataset
  dplyr::mutate(Dist = 100-Dist,
                Res = 100-Res)
  
# Getting individual means & SDs
mean_data_pn_genpop <- ESM_genpop %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise (mean_Exh = mean(Exh, na.rm=TRUE),  sd_Exh = sd(Exh, na.rm=TRUE), 
                    mean_Dist = mean(Dist, na.rm=TRUE),  sd_Dist = sd(Dist, na.rm=TRUE), 
                    mean_FedUp = mean(FedUp, na.rm=TRUE),  sd_FedUp = sd(FedUp, na.rm=TRUE), 
                    mean_PartSup = mean(PartSup, na.rm=TRUE),  sd_PartSup = sd(PartSup, na.rm=TRUE), 
                    mean_PartConf = mean(PartConf, na.rm=TRUE),  sd_PartConf = sd(PartConf, na.rm=TRUE), 
                    mean_DiffKids = mean(DiffKids, na.rm=TRUE),  sd_DiffKids = sd(DiffKids, na.rm=TRUE), 
                    mean_PosMoKids = mean(PosMoKids, na.rm=TRUE),  sd_PosMoKids = sd(PosMoKids, na.rm=TRUE), 
                    mean_AngKids = mean(AngKids, na.rm=TRUE),  sd_AngKids = sd(AngKids, na.rm=TRUE), 
                    mean_Res = mean(Res, na.rm=TRUE),  sd_Res = sd(Res, na.rm=TRUE), 
                    mean_Social = mean(SocSup, na.rm=TRUE),  sd_Social = sd(SocSup, na.rm=TRUE),
                    mean_TimeKids = mean(TimeKids, na.rm = TRUE), sd_TimeKids = sd(TimeKids, na.rm = TRUE)) 


# Adding PBA data
mean_data_pn_genpop <- merge(mean_data_pn_genpop, PBA_genpop, by ="ID")
  
### 4.2 Joining with current dataset (PB) --------------------------------------------

# Getting PBA scores
PBA_pb <- readr::read_csv(here::here("data", "processed", "ESM2_PB_Demographic_questionnaires.csv"))

# Getting only needed variables in participant dataset
mean_data_pn <- mean_data_pn %>% dplyr::select(ID | contains("mean_") | contains("sd_"))

# Adding PBA data
mean_data_pn_PBA <- merge(mean_data_pn, PBA_pb, by ="ID")

# Merging two datasets with group identifier 
ESM_summarystats_PBA <- dplyr::bind_rows(list(PB = mean_data_pn_PBA, genpop = mean_data_pn_genpop), .id = 'group') %>%
  # just keeping needed rows
  dplyr::select(group | PBA_total | contains("mean_") | contains("sd_"))  %>%
  dplyr::select(group | PBA_total | contains("Exh") | contains("Dist") | contains("FedUp")) 
  

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5) Exporting Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Dataframe with intraindividual summary data + PBA scores (across this PB sample + previous genpop sample)
readr::write_csv(ESM_summarystats_PBA, here::here("data", "processed_sharing", "ESM_summarystats_PBA.csv"))

# Table with descriptive ESM statistics
readr::write_csv(table_desc, here::here("data", "processed_sharing", "Table_ESMdescriptives_PB.csv"))    

# Table with skew/kurtosis values for within-person centered raw data + Gaussianize-corrected data
# This is Table S1 in the supplementary materials
readr::write_csv(skew_kurt_table, here::here("output", "supplementary_materials", "skew_kurtosis_table.csv"))

