# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Other Supplementary Materials           
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This R code contains supplementary analyses for this project: https://osf.io/5sgkr/

# Supplementary materials include 
# - centrality analyses, 
# - stability analyses for centrality (e.g., generating new samples dropping
# random 20% of participants, and examining how correlated results are with
# original centrality indices)
# - plotting random effects of network model (MAB maybe remove if in main analyses?)

# Libaries: 
library(qgraph) # For centrality estimates etc
library(ggpubr) # to arrange plots
library(mlVAR) # Computing mlVAR models (for stability analyses re: centrality)
library(devEMF) # To save figures as EMF (if doing so)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Importing Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### 1.1 ESM data -------------------------------------------------------------

# Importing ESM data
ESM_clean <- readr::read_csv(here::here("data", "processed_sharing", "ESM2_PB_clean_anon.csv"))

# Importing gender info
sample_gender <- readr::read_csv(here::here("data", "processed_sharing", "ESM2_PB_gender"))

ESM <- ESM_clean %>%
  dplyr::select (ID, n_obs, NumberHours, weekday, Exh = ESM_exhaustion, Dist = ESM_distance_Rev, FedUp = ESM_fedup, PartSup = ESM_partnersupport,
                 PartConf = ESM_partnerconflict, DiffKids = ESM_kids, PosMoKids = ESM_bxtokids_pos, AngKids = ESM_bxtokids_neg, 
                 Res = ESM_resources_Rev, SocSup = ESM_socialsupport, TimeKids = ESM_timewithkids, time_to_complete)

# Making sample that includes only mothers (n=36)
ESM_mothers <- merge(ESM, sample_gender, by = "ID") %>% filter(gender == "femme")

### 1.1 R objects-------------------------------------------------------------

# Importing estimated model (from main manuscript code, with only mothers), for
# centrality analyses
network_mothers <- readr::read_rds(here::here("output", "R_Objects", "network_mothers.rds"))

# Importing estimated model with all parents (from sensitivity analysis code),
# for stability analyses re: centrality
network_allparents <- readr::read_rds(here::here("output", "R_Objects", "network_allparents.rds"))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Centrality Analyses -----------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Original network visualizations:
contemporaneous <- plot(network_mothers, "contemporaneous", DoNotPlot = T)
temporal <- plot(network_mothers, "temporal", DoNotPlot = T)
between_subjects <- plot(network_mothers, "between", DoNotPlot = T)

### 2.1 Strength--------------------------------------------------------------

# Strength indices per network
centrality_temporal_strength <- qgraph::centralityPlot(temporal, include = c("OutStrength", "InStrength"), decreasing = TRUE, scale = 'raw0') +
  ggtitle("A. Temporal")

centrality_contemp_strength <- qgraph::centralityPlot(contemporaneous, include = c("Strength"), decreasing = TRUE, scale = 'raw0') +
  theme(axis.text.y = element_blank()) + 
  ggtitle("B. Contemporaneous")

centrality_bs_strength <- qgraph::centralityPlot(between_subjects, include = c("Strength"), decreasing = TRUE, theme_bw = TRUE, scale = 'raw0') +
  theme(axis.text.y = element_blank()) + 
  ggtitle("C. Between-Subjects")

# Save figures 
devEMF::emf(file = here::here("output", "Figures", "SFig_CentralityStrength_mothers.emf"), width = 8, height = 5, pointsize = 10)
ggpubr::ggarrange(centrality_temporal_strength, centrality_contemp_strength, centrality_bs_strength,
          common.legend = TRUE, legend = "left", widths = c(2,1,1), ncol = 3)
dev.off()

### 2.1 Expected Influence -------------------------------------------

# Note - have to graph these with 'raw' (vs 'raw0' as in strength analyses),
# since there are negative EI values

# Strength indices per network
centrality_temporal_EI <- qgraph::centralityPlot(temporal, include = c("OutExpectedInfluence", "InExpectedInfluence"), decreasing = TRUE, scale = 'raw') +
  ggtitle("A. Temporal")

centrality_contemp_EI <- qgraph::centralityPlot(contemporaneous, include = c("ExpectedInfluence"), decreasing = TRUE, scale = 'raw') +
  theme(axis.text.y = element_blank()) + 
  ggtitle("B. Contemporaneous")

centrality_bs_EI <- qgraph::centralityPlot(between_subjects, include = c("ExpectedInfluence"), decreasing = TRUE, theme_bw = TRUE, scale = 'raw') +
  theme(axis.text.y = element_blank()) + 
  ggtitle("C. Between-Subjects")

# Save figures 
devEMF::emf(file = here::here("output", "Figures", "SFig_CentralityEI_mothers.emf"), width = 8, height = 5, pointsize = 10)
ggpubr::ggarrange(centrality_temporal_EI, centrality_contemp_EI, centrality_bs_EI,
                  common.legend = TRUE, legend = "left", widths = c(2,1,1), ncol = 3)
dev.off()

# Note - not including this figure in the final supplementary materials because
# it's difficult to interpret with negative/positive EI values, as not all nodes
# are straightforward to interpret as only negative (e.g., symptoms) or positive
# (e.g., resources)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) Stability Analyses (Centrality) -----------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Dropping participants and seeing if centrality estimates still stay similar (stability analyses)
# Method & code adapted from Jongeneel et al. (2020): https://doi.org/10.1016/j.schres.2019.10.055

  # Note - to maximize the sample for this stability analysis, we use the full
  # sample - this means n=40 parents, including the 4 fathers (particularly as
  # the results for the full sample & just mothers are very similar), and not
  # including the two partner-related nodes, so that we can include all single
  # parents as well. We're also estimating orthogonal random effects (and not
  # random ones) for time & because the sample size is smaller than in the main
  # manuscript


# 80% of full sample = 32 participants in each network below
sample_80 <- length(unique(ESM$ID))*.80

# Set up variables to hold centrality indices for each network type
c_strength <- NULL  
t_in_strength <- NULL
t_out_strength <- NULL
bs_strength <- NULL

# Getting variable names as a list for mlVAR
variables_names  = c("Exh", "Dist", "FedUp", "PartSup","PartConf","DiffKids", "PosMoKids",
                     "AngKids", "Res","SocSup", "TimeKids")

set.seed(46)

  # Note: not all models converged, likely due to the sample size of only 32 (80%
  # of full sample, dropping a random 20% each time). I was not able to correctly
  # implement a way to keep the loop going with an error (e.g., TryCatch), but
  # with the current seed the loop runs for 199 iterations before a model doesn't
  # converge

counter <- 0
for(i in 1:200){ # generating 200 networks 
  tryCatch({
    # Each time: randomly sample 32 participants (~80% of participants)
    mod <- mlVAR::mlVAR(ESM[ESM$ID %in% sample(unique(ESM$ID), sample_80),],
                 vars = variables_names[c(1:3,6:10)], 
                 idvar = "ID", 
                 lags = 1, 
                 beepvar = "n_obs",
                 estimator = "lmer",
                 contemporaneous = "orthogonal",
                 temporal = "orthogonal")
    
    c_strength <- rbind(c_strength, qgraph::centrality_auto(qgraph::getWmat(plot(mod, "contemporaneous", DoNotPlot = TRUE)))$node.centrality$Strength)
    t_in_strength <- rbind(t_in_strength, qgraph::centrality_auto(qgraph::getWmat(plot(mod, "temporal", DoNotPlot = TRUE)))$node.centrality$InStrength)
    t_out_strength <- rbind(t_out_strength, qgraph::centrality_auto(qgraph::getWmat(plot(mod, "temporal", DoNotPlot = TRUE)))$node.centrality$OutStrength)
    bs_strength <- rbind(bs_strength, qgraph::centrality_auto(qgraph::getWmat(plot(mod, "between", DoNotPlot = TRUE)))$node.centrality$Strength)
    
    counter <- counter + 1
    print(counter) # to know how far along     
    },
    error= function(e) {print(paste("Error with model, counter: ", counter))}
)}

# Save R objects
# readr::write_rds(c_strength, here::here("output", "R_Objects", "stability_c_strength.rds"))
# readr::write_rds(t_in_strength, here::here("output", "R_Objects", "stability_t_in_strength.rds"))
# readr::write_rds(t_out_strength, here::here("output", "R_Objects", "stability_t_out_strength.rds"))
# readr::write_rds(bs_strength, here::here("output", "R_Objects", "stability_bs_strength_199"))

# Read
c_strength <- readr::read_rds(here::here("output", "R_Objects", "stability_c_strength_199.rds"))
t_in_strength <- readr::read_rds(here::here("output", "R_Objects", "stability_t_in_strength_199.rds"))
t_out_strength <- readr::read_rds(here::here("output", "R_Objects", "stability_t_out_strength_199.rds"))
bs_strength <- readr::read_rds(here::here("output", "R_Objects", "stability_bs_strength_199.rds"))


# (If looking at code later & just want to read in R objects):
# c_strength <- readr::read_rds(here::here("output", "R_Objects", "stability_c_strength.rds"))
# t_in_strength <- readr::read_rds(here::here("output", "R_Objects", "stability_t_in_strength.rds"))
# t_out_strength <- readr::read_rds(here::here("output", "R_Objects", "stability_t_out_strength.rds"))
# bs_strength <- readr::read_rds(here::here("output", "R_Objects", "stability_bs_strength.rds"))


# Original network visualizations (for full sample):
contemporaneous_allparents <- plot(network_allparents, "contemporaneous", DoNotPlot = T)

temporal_allparents <- plot(network_allparents, "temporal", DoNotPlot = T)

between_allparents <- plot(network_allparents, "between", DoNotPlot = T)

# then calculate spearman rank correlations between centrality measures of orig model & of new models
cor1 <- c()
cor2 <- c()
cor3 <- c()
cor4 <- c()


for(i in 1:199){
  cor1 <- c(cor1,cor(c_strength[i,], qgraph::centrality_auto(contemporaneous_allparents)$node.centrality$Strength, method = 'spearman'))
  cor2 <- c(cor2,cor(t_in_strength[i,], qgraph::centrality_auto(temporal_allparents)$node.centrality$InStrength, method = 'spearman'))
  cor3 <- c(cor3,cor(t_out_strength[i,], qgraph::centrality_auto(temporal_allparents)$node.centrality$OutStrength, method = 'spearman'))
  cor4 <- c(cor4,cor(bs_strength[i,], qgraph::centrality_auto(between_allparents)$node.centrality$Strength, method = 'spearman'))
}

# Save histograms

devEMF::emf(file = here::here("output", "Figures", "SFig_stability_strength_allparents.emf"), width = 6, height = 6, bg = "transparent", pointsize = 10)

layout(matrix(1:4,2,2))

hist_strength_contemp <- hist(cor1, main = "Strength (Contemporaneous Network)", xlab = "Spearman's rank order correlation", breaks = seq(-.4,1,by=.05), ylim = c(0,115), xlim = c(-0.4,1))
abline(v=0,lty='dashed', col='lightblue')

hist_instrength_temp <- hist(cor2, main = "In-Strength (Temporal Network)", xlab = "Spearman's rank order correlation", breaks = seq(-.4,1,by=.05), ylim = c(0,115), xlim = c(-0.4,1))
abline(v=0,lty='dashed', col='lightblue')

hist_outstrength_temp <- hist(cor3, main = "Out-Strength (Temporal Network)", xlab = "Spearman's rank order correlation", breaks = seq(-.4,1,by=.05), ylim = c(0,115), xlim = c(-0.4,1))
abline(v=0,lty='dashed', col='lightblue')

hist_strength_bs <- hist(cor4, main = "Strength (Between-Subjects Network)", xlab = "Spearman's rank order correlation", breaks = seq(-.4,1,by=.05), ylim = c(0,115), xlim = c(-0.4,1))
abline(v=0,lty='dashed', col='lightblue')

dev.off() 

