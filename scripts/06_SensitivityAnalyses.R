# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sensitivity Analyses           
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This R code contains sensitivity analyses for this project: https://osf.io/5sgkr/

# Specifically, it uses the same analysis method as the main analysis in the
# manuscript (temporal network analyses with sequential univariate estimation
# via mlVAR) and alters a few key details about the data and/or variables, to
# examine the impact on results.

# Sensitivity analyses include: 
# 1. Looking at all parents (n=40) including 4 fathers not included in main manuscript
# 2. Looking at including coparenting variables (Partner Conflict + Partner
    # Support); n=30 (not including single parents)
# 3. Including the "Time Spent with Kids" variable in the network

# Libraries: 
library(qgraph) # Visualizing networks
library(mlVAR) # Computing multilevel vector autoregressive models
library(devEMF) # To save figures as EMF 
library(patchwork) # to arrange random effects plots (with axes aligned)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Importing Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Importing ESM data
ESM_clean <- readr::read_csv(here::here("data", "processed_sharing", "ESM2_PB_clean_anon.csv"))

# selecting relevant columns, renaming columns for graphs
ESM <- ESM_clean %>%
  dplyr::select (ID, n_obs, NumberHours, weekday, Exh = ESM_exhaustion, Dist = ESM_distance_Rev, FedUp = ESM_fedup, PartSup = ESM_partnersupport,
                 PartConf = ESM_partnerconflict, DiffKids = ESM_kids, PosMoKids = ESM_bxtokids_pos, AngKids = ESM_bxtokids_neg, 
                 Res = ESM_resources_Rev, SocSup = ESM_socialsupport, TimeKids = ESM_timewithkids, time_to_complete)

# Getting variable names as a list for mlVAR
variables_names  = c("Exh", "Dist", "FedUp", "PartSup","PartConf","DiffKids", "PosMoKids",
                     "AngKids", "Res","SocSup", "TimeKids")


# MAB - below is to get dataset with just info from mothers...
# Importing gender info
sample_gender <- readr::read_csv(here::here("data", "processed_sharing", "ESM2_PB_gender"))
# Making sample that includes only mothers (n=36)
ESM_mothers <- merge(ESM, sample_gender, by = "ID") %>% filter(gender == "femme")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Network with all parents (including 4 fathers) --------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sensitivity Analysis Network #1
  # Note - not including partner variables bc including single parents
  # n = 40 parents (36 mothers + 4 fathers); nodes = 8 (no partner variables)

### 2.1 Estimate mlVAR model -----------------------------------------------------

# First, using mlVAR to generate the network structure
network_allparents <- mlVAR::mlVAR(ESM, vars = variables_names[c(1:3, 6:10)], idvar = "ID", lags = 1, beepvar = "n_obs", estimator = "lmer", 
                                contemporaneous = "correlated", temporal = "correlated")

# Code to save R object of estimated model, or read saved one
#readr::write_rds(network_allparents, here::here("output", "R_Objects", "network_allparents.rds"))
#network_allparents <- readr::read_rds(here::here("output", "R_Objects", "network_allparents.rds"))

# Save all edge values into a table
edgevalues_allparents <- summary (network_allparents)
edgevalues_temp_allparents <- edgevalues_allparents$temporal
edgevalues_contemp_allparents <- edgevalues_allparents$contemporaneous
edgevalues_between_allparents <- edgevalues_allparents$between

readr::write_csv(edgevalues_temp_allparents, here::here("output", "supplementary_materials", "tables", "Table_EdgeValues_Temp_allparents.csv"))    
readr::write_csv(edgevalues_contemp_allparents, here::here("output","supplementary_materials", "tables", "Table_EdgeValues_Contemp_allparents.csv"))    
readr::write_csv(edgevalues_between_allparents, here::here("output","supplementary_materials", "tables", "Table_EdgeValues_Between_allparents.csv"))    

### 2.2 Visualize network -----------------------------------------------------

# Saving file as EMF in output/Figures file, with width/height specified
devEMF::emf(file = here::here("output", "Figures", "network_allparents.emf"), width = 8, height = 8, bg = "transparent", pointsize = 12)

layout(matrix(c(0,1,1,0,
                2,2,3,3), nrow = 2, byrow = T))

temporal<-plot(network_allparents, "temporal", layout = "circle", nonsig = "hide", theme = "colorblind", title = "Temporal",
               labels = vars, vsize = 16, asize = 10, mar = c(5,5,5,5),  negDashed = T)

contemporaneous<-plot(network_allparents, "contemporaneous", layout = "circle", nonsig = "hide", theme = "colorblind", title = "Contemporaneous",
                      labels = vars, vsize = 16, rule = "and",  negDashed = T)

between_subjects<-plot(network_allparents, "between", layout = "circle", nonsig = "hide",  theme = "colorblind", title = "Between-subjects",
                       labels = vars, vsize = 16, rule = "and",  negDashed = T)

dev.off()
par(mfrow=c(1,1))



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) Network all 10 variables (& n=30, no single parents) --------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sensitivity Analysis Network #2
  # Note - not including single parents because they did not answer questions on
  # Partner Conflict or Partner Support
  # n = 30 parents (26 mothers + 4 fathers); nodes = 8 (no partner variables)

# Getting data of just single parents:
ID_no_coparent <- ESM %>%
  dplyr::group_by(ID) %>%
  dplyr::filter(all(is.na(PartSup))) %>%
  dplyr::distinct(ID) 

ESM_coparents <- ESM %>%
  dplyr::filter(!ID %in% ID_no_coparent[[1]])

### 3.1 Estimate mlVAR model -----------------------------------------------------
  # Note - when attempting to estimate with correlated random effects, the model
  # had issues (specifically, an error about there being a SD of 0 for the
  # (estimated?) mu of the DiffKids variable. It's possible this could be due to
  # convergence issues since the sample is small + more nodes, so estimating it
  # with orthogonal random effects...)

network_coparents <- mlVAR::mlVAR(ESM_coparents, vars = variables_names[c(1:10)], idvar = "ID", lags = 1, beepvar = "n_obs", estimator = "lmer", 
                                   contemporaneous = "orthogonal", temporal = "orthogonal")


#readr::write_rds(network_coparents, here::here("output", "R_Objects", "network_coparents.rds"))
#network_coparents <- readr::read_rds(here::here("output", "R_Objects", "network_coparents.rds"))


### 3.2 Visualize network -----------------------------------------------------

devEMF::emf(file = here::here("output", "Figures", "network_coparents.emf"), width = 8, height = 8, bg = "transparent", pointsize = 12)

layout(matrix(c(0,1,1,0,
                2,2,3,3), nrow = 2, byrow = T))

temporal<-plot(network_coparents, "temporal", layout = "circle", nonsig = "hide", theme = "colorblind", title = "Temporal",
               labels = vars, vsize = 16, asize = 10, mar = c(5,5,5,5),  negDashed = T)

contemporaneous<-plot(network_coparents, "contemporaneous", layout = "circle", nonsig = "hide", theme = "colorblind", title = "Contemporaneous",
                      labels = vars, vsize = 16, rule = "and",  negDashed = T)

between_subjects<-plot(network_coparents, "between", layout = "circle", nonsig = "hide",  theme = "colorblind", title = "Between-subjects",
                       labels = vars, vsize = 16, rule = "and",  negDashed = T)

dev.off()
par(mfrow=c(1,1))



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4) Network with "Time with Kids" variable (whole sample) ----------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sensitivity Analysis Network #3
  # Note - Not including partner variables because would have to remove 10
  # participants (all single parents)
  # n = 40 parents (whole sample, 36 mothers + 4 fathers); nodes = 9 (no partner
  # variables but with 'Time spent with Kids')

### 4.1 Estimate mlVAR model -----------------------------------------------------

# First, using mlVAR to generate the network structure
network_allparents_TimeKids <- mlVAR::mlVAR(ESM, vars = variables_names[c(1:3, 6:11)], idvar = "ID", lags = 1, beepvar = "n_obs", estimator = "lmer", 
                                   contemporaneous = "correlated", temporal = "correlated")

# Save estimated mlVAR model as .rds object
#readr::write_rds(network_allparents_TimeKids, here::here("output", "R_Objects", "network_allparents_TimeKids.rds"))
#network_allparents_TimeKids <- readr::write_rds(here::here("output", "R_Objects", "network_allparents_TimeKids.rds"))

                   
### 4.2 Visualize network -----------------------------------------------------

# Saving file as EMF in output/Figures file, with width/height specified
devEMF::emf(file = here::here("output", "Figures", "network_allparents_TimeKids.emf"), width = 8, height = 8, bg = "transparent", pointsize = 12)

layout(matrix(c(0,1,1,0,
                2,2,3,3), nrow = 2, byrow = T))

temporal<-plot(network_allparents_TimeKids, "temporal", layout = "circle", nonsig = "hide", theme = "colorblind", title = "Temporal",
               labels = vars, vsize = 16, asize = 10, mar = c(5,5,5,5),  negDashed = T)

contemporaneous<-plot(network_allparents_TimeKids, "contemporaneous", layout = "circle", nonsig = "hide", theme = "colorblind", title = "Contemporaneous",
                      labels = vars, vsize = 16, rule = "and",  negDashed = T)

between_subjects<-plot(network_allparents_TimeKids, "between", layout = "circle", nonsig = "hide",  theme = "colorblind", title = "Between-subjects",
                       labels = vars, vsize = 16, rule = "and",  negDashed = T)

dev.off()
par(mfrow=c(1,1))
