# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Network Visualizations (Main Analyses)         
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This R code contains the main analyses for this project: https://osf.io/5sgkr/

# Specifically, it includes estimating a multilevel VAR model (estimated with
# sequential univariate models using the mlVAR package, with correlated random
# effects), saving the resulting R object & edge values, and then plotting three
# network visualizations (temporal, contemporaneous, and between-subjects)

# Libraries: 
library(tidyverse)
library(qgraph) # Visualizing networks
library(mlVAR) # Computing multilevel vector autoregressive models
#library(devEMF) # To save figures as EMF (if using)
library(patchwork) # to arrange random effects plots (with axes aligned)
library(ggbeeswarm) # For jitter in variance (fixed vs random effects) boxplot figure


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Importing Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Importing ESM data
ESM_clean <- readr::read_csv(here::here("data", "processed_sharing", "ESM2_PB_clean_anon.csv"))

# Importing gender info
sample_gender <- readr::read_csv(here::here("data", "processed_sharing", "ESM2_PB_gender"))

# Importing PBA scores with participant IDs (not sharing data on OSF)
ID_PBA <- readr::read_csv(here::here("data", "processed", "ESM2_PB_Demographic_questionnaires.csv")) %>%
  dplyr::select(ID, PBA_total)


# Importing participant ESM means & SDs (table generated in script 04_ESM_summarystats...)
ESM_summarystats_PBA <- readr::read_csv(here::here("data", "processed_sharing", "ESM_summarystats_PBA.csv"))

# selecting relevant columns, renaming columns for graphs
ESM <- ESM_clean %>%
  dplyr::select (ID, n_obs, NumberHours, weekday, Exh = ESM_exhaustion, Dist = ESM_distance_Rev, FedUp = ESM_fedup, PartSup = ESM_partnersupport,
                 PartConf = ESM_partnerconflict, DiffKids = ESM_kids, PosMoKids = ESM_bxtokids_pos, AngKids = ESM_bxtokids_neg, 
                 Res = ESM_resources_Rev, SocSup = ESM_socialsupport, TimeKids = ESM_timewithkids, time_to_complete)

# Getting variable names as a list for mlVAR
variables_names  = c("Exh", "Dist", "FedUp", "PartSup","PartConf","DiffKids", "PosMoKids",
                     "AngKids", "Res","SocSup", "TimeKids")

# Making sample that includes only mothers (n=36)
ESM_mothers <- merge(ESM, sample_gender, by = "ID") %>% filter(gender == "femme")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Comparing samples on ESM summary stats ----------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### 2.1 t-tests (SDs) -------------------------------------------------------------
  # FYI - Full results reported in Table S2 (in supplementary materials)

# Exhaustion - SD
t.test(sd_Exh ~ group, data = ESM_summarystats_PBA, alternative = "greater")
  # alternative = greater bc expect iiSD to be larger for genpop
  # no support found: t = -3.2879, df = 84.43, p-value = 0.9993
  # means: genpop = 24.31208; PB = 28.44924
  
  # Also get SDs per group to report in Table S2:
  ESM_summarystats_PBA %>% dplyr::group_by(group) %>% dplyr::summarize(sd(sd_Exh))

# Distance - SD
t.test(sd_Dist ~ group, data = ESM_summarystats_PBA, alternative = "greater")
  # no support found: t = -3.9512, df = 83.116, p-value = 0.9999
  # means: genpop = 15.74033; PB = 21.66518

  # Also get SDs per group to report in Table S2:
  ESM_summarystats_PBA %>% dplyr::group_by(group) %>% dplyr::summarize(sd(sd_Dist))

# Fed Up - SD
t.test(sd_FedUp ~ group, data = ESM_summarystats_PBA, alternative = "greater")
# no support found: t = -3.0751, df = 87.847, p-value = 0.9986
# means: genpop = 21.89417; PB = 25.91867  

  # Also get SDs per group to report in Table S2:
  ESM_summarystats_PBA %>% dplyr::group_by(group) %>% dplyr::summarize(sd(sd_FedUp))

### 2.2 t-tests (means) -------------------------------------------------------------
  # Looking at means as a check (to make sure the means are higher for the clinical group)
  # FYI - full results reported in Table S3 (in supplementary materials)
  
  # Exhaustion - means
  t.test(mean_Exh ~ group, data = ESM_summarystats_PBA, alternative = "less")
  # Alternative = 'less' bc expected greater mean levels for PB sample
  # significant: t = -3.4144, df = 69.958, p-value = 0.0005337
  # means: genpop = 29.19830; PB = 41.60531
  
  # Also get SDs per group to report in Table S3:
  ESM_summarystats_PBA %>% dplyr::group_by(group) %>% dplyr::summarize(sd(mean_Exh))
  
  
  # Distance - means
  t.test(mean_Dist ~ group, data = ESM_summarystats_PBA, alternative = "less")
  # significant: t = -2.0771, df = 70.852, p-value = 0.02071
  # means: genpop = 23.89294; PB = 31.00340
  
  # Also get SDs per group to report in Table S3:
  ESM_summarystats_PBA %>% dplyr::group_by(group) %>% dplyr::summarize(sd(mean_Dist))
  
  # Fed Up - means
  t.test(mean_FedUp ~ group, data = ESM_summarystats_PBA, alternative = "less")
  # significant: t = -2.6603, df = 63.487, p-value = 0.004937
  # means: genpop = 22.52222; PB = 30.53250
  
  # Also get SDs per group to report in Table S3:
  ESM_summarystats_PBA %>% dplyr::group_by(group) %>% dplyr::summarize(sd(mean_FedUp))
  
### 2.3 Correlation with PBA scores -------------------------------------------------------------
 
# Correlations between participant ESM SD and PBA score...
  cor.test(~ sd_Exh + PBA_total, data = ESM_summarystats_PBA, method = "pearson", 
           alternative = "greater") # Alternative = expect positive association
  # significant: t = 2.1548, df = 88, p-value = 0.01696
  # cor = 0.2238677 
  
  cor.test(~ sd_Dist + PBA_total, data = ESM_summarystats_PBA, method = "pearson", 
           alternative = "greater") # Alternative = expect positive association
  # significant: t = 4.4703, df = 88, p-value = 1.159e-05
  # cor = 0.4301901
  
  
  cor.test(~ sd_FedUp + PBA_total, data = ESM_summarystats_PBA, method = "pearson", 
           alternative = "greater") # Alternative = expect positive association
  # significant: t = 3.3484, df = 88, p-value = 0.0005989
  # cor = 0.3361674 
  

  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) Network Analysis --------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# First, using mlVAR to generate the network structure

# Network with raw data & not including PartSup & PartConf (bc many single mothers)
# Note - sensitivity analyses (network with all parents, including n=4 fathers;
# with all nodes, including partner variables but without single parents; 
# are included in Script 06_SensitivityAnalyses.R, and the results reported in
# the supplementary materials) 
network_mothers <- mlVAR::mlVAR(ESM_mothers, vars = variables_names[c(1:3, 6:10)], idvar = "ID", lags = 1, beepvar = "n_obs", estimator = "lmer", 
                              contemporaneous = "correlated", temporal = "correlated")

#readr::write_rds(network_mothers, here::here("output", "R_Objects", "network_mothers.rds"))
#network_mothers <- readr::read_rds(here::here("output", "R_Objects", "network_mothers.rds"))

# Save all edge values into a table
edgevalues <- summary(network_mothers)
edgevalues_temp <- edgevalues$temporal
edgevalues_contemp <- edgevalues$contemporaneous
edgevalues_between <- edgevalues$between

readr::write_csv(edgevalues_temp, here::here("output", "supplementary_materials", "tables", "Table_EdgeValues_Temp"))    
readr::write_csv(edgevalues_contemp, here::here("output","supplementary_materials", "tables", "Table_EdgeValues_Contemp"))    
readr::write_csv(edgevalues_between, here::here("output","supplementary_materials", "tables", "Table_EdgeValues_Between"))    


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4) Visualizing Network------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Saving file as EMF in output/Figures file, with width/height specified
#devEMF::emf(file = here::here("output", "Figures", "./Fig1_network_moms_labeled.emf"), width = 9, height = 9, bg = "transparent", pointsize = 14)
setEPS()
postscript(file = here::here("output", "Figures", "./Fig1_network_moms_labeled.eps"), width = 9, height = 9, bg = "transparent", pointsize = 14)

# Putting all 3 network figues in one layout
layout(matrix(c(0,1,1,0,
                2,2,3,3), nrow = 2, byrow = T))

temporal<-plot(network_mothers, "temporal", layout = "circle", nonsig = "hide", theme = "colorblind", title = "A. Temporal",
               labels = vars, vsize = 16, asize = 10, mar = c(5,5,5,5),  negDashed = T, title.cex = 1.5)

contemporaneous<-plot(network_mothers, "contemporaneous", layout = "circle", nonsig = "hide", theme = "colorblind", title = "B. Contemporaneous",
                      labels = vars, vsize = 16, rule = "and",  negDashed = T, title.cex = 1.5)

between_subjects<-plot(network_mothers, "between", layout = "circle", nonsig = "hide",  theme = "colorblind", title = "C. Between-subjects",
                       labels = vars, vsize = 16, rule = "and",  negDashed = T, title.cex = 1.5)

dev.off()
par(mfrow=c(1,1))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5) Plotting variance: fixed vs random effects-------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Code adapted from M. Veenman, found here:
# https://github.com/MyrtheV/Family_Affect_Systems/blob/main/R%20code/Functions/famvargeneff_function.R
# and
# https://github.com/MyrtheV/Family_Affect_Systems/blob/main/R%20code/Figures%20/Figure1_panelnomotheticnetworks.R

### 5.1 Temporal: random vs fixed effects -------------------------------------------------------------

# Getting general (group-level) effects (specifically non-zero edges)
nonzerogentemp <- which(mlVAR::getNet(network_mothers, "temporal", nonsig = "hide") != 0)

# Get list of edges/names (so they can be included in graph later): 
net_edgelist <- mlVAR::getNet(network_mothers, "temporal", nonsig = "hide")
tidyr::crossing(colnames(net_edgelist), rownames(net_edgelist))
edgelist_all <- c(outer(rownames(net_edgelist), colnames(net_edgelist), FUN = paste, sep=" > "))

# Only looking at edges that are nonzero in the group-level model
edgelist_all[nonzerogentemp] 

# random effects (for edges that are nonzero in gen model)
beta_pns <- c()
for(i in 1:length(network_mothers$IDs)){
  beta_pns <- c(beta_pns, network_mothers$results$Beta$subject[[i]][,,1][nonzerogentemp])
}

# Example: Coefficients for subject one (for temp network)
network_mothers$results$Beta$subject[[1]][,,1]

# Group-level effects from mlVAR model (dataframe with edge name & weight etc)
tempgeneff <- data.frame(pn = rep("General", length(nonzerogentemp)),
                         edge_num = 1:length(nonzerogentemp), 
                         edge_name = edgelist_all[nonzerogentemp],
                         weight = mlVAR::getNet(network_mothers, "temporal", nonsig = "hide")[nonzerogentemp])

# Creating a dataframe with the temp random effects
numpn <- length(network_mothers$ID)
tempeffnum <- rep(1:length(nonzerogentemp), numpn)
tempeff_name <- rep(edgelist_all[nonzerogentemp], numpn)
#numpnvar <- rep(1:numpn, each = length(nonzerogentemp))
numpnvar <- rep(network_mothers$IDs, each = length(nonzerogentemp))


#Putting the general group-level coefficients (fixed effects) into a dataframe
#with the individual estimates (coefficients)
temp_randomeff <- data.frame(pn = numpnvar, edge = tempeffnum, edge_name = tempeff_name,weight = beta_pns)

# Adding PBA scores to this data (to colorgrade boxplots)
temp_randomeff <- merge(x = temp_randomeff, y=ID_PBA, by.x = "pn", by.y ="ID")


# Figure - boxplots with temporal random effects (black squares: fixed effects)

plot_temprand <- ggplot(temp_randomeff, mapping = aes(x = weight, y = as.factor(edge_name), color = PBA_total)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.5, width = 0.2, dodge.width=1, groupOnX=FALSE) + 
  geom_vline(xintercept = 0, color = "sky blue", linetype = 1, size = 2, alpha = .5) + 
  geom_boxplot(temp_randomeff, mapping = aes(x = weight, y = as.factor(edge_name)), alpha = .1) + 
  geom_point(tempgeneff, mapping = aes(x = weight,  y = as.factor(edge_name)), color = "black", shape = 15, size = 3) + 
  theme_classic() + 
  labs(x = "Weight", y = "Edge", color = "PBA Score") +
  scale_color_viridis_c(option="plasma") +
  xlim(-1, .6) +
  ggtitle("A. Random Effects from the Temporal Network")



### 5.2 Contemporaneous: random vs fixed effects -------------------------------------------------------------

# Getting general (group-level) effects 
# Only the lower triangle, don't need double values (undirected network)
lowertrianglecon <- mlVAR::getNet(network_mothers, "contemporaneous", nonsig = "hide", rule = "and")[which(lower.tri(mlVAR::getNet(network_mothers, "contemporaneous", nonsig = "hide", rule = "and")))]
nonzerogen_contemp <- which(lowertrianglecon != 0) #(specifically non-zero edges)

# Get list of edges/names: 
net_edgelist <- mlVAR::getNet(network_mothers, type = "contemporaneous", nonsig = "hide", rule = "and")
net_edgelist[lower.tri(net_edgelist)]

# Putting into matrix
edgelist_all <- c(outer(rownames(net_edgelist), colnames(net_edgelist), FUN = paste, sep=" & "))
edgelist_all <- matrix(edgelist_all, nrow=8, ncol=8) # Note: nrow & ncol should = # of nodes in network
edgelist_all <- edgelist_all[lower.tri(edgelist_all)]

# Only keeping non-zero edges
edgelist_all[nonzerogen_contemp]

# Getting individual estimate (random effects), for edges that are nonzero in group level gen model
theta_pns <- c()
for(i in 1:length(network_mothers$IDs)){
  lowertri_pn <- network_mothers$results$Theta$pcor$subject[[i]][which(lower.tri(network_mothers$results$Theta$pcor$subject[[i]]))]
  theta_pns <- c(theta_pns, lowertri_pn[nonzerogen_contemp])
}

# Group-level effects from mlVAR model
contemp_geneff <- data.frame(pn = rep("General", length(nonzerogen_contemp)),
                             edge_num = 1:length(nonzerogen_contemp), 
                             edge_name = edgelist_all[nonzerogen_contemp],
                             weight = lowertrianglecon[nonzerogen_contemp])

# Creating a dataframe with the contemp random effects
numpn <- length(network_mothers$ID)
contempnum <- rep(1:length(nonzerogen_contemp), numpn)
contemp_name <- rep(edgelist_all[nonzerogen_contemp], numpn)
#numpnvar <- rep(1:numpn, each = length(nonzerogen_contemp))
numpnvar <- rep(network_mothers$IDs, each = length(nonzerogen_contemp))


contemp_randomeff <- data.frame(pn = numpnvar, 
                                edge = contempnum, 
                                edge_name = contemp_name,
                                weight = theta_pns)

# Adding PBA scores to this data (to colorgrade boxplots)
contemp_randomeff <- merge(x = contemp_randomeff, y=ID_PBA, by.x = "pn", by.y ="ID")

# Figure - boxplots contemporaneous random effects
plot_contemprand <- ggplot(contemp_randomeff, mapping = aes(x = weight, y = as.factor(edge_name), color=PBA_total)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.5, width = 0.2, dodge.width=1, groupOnX=FALSE) + 
  geom_vline(xintercept = 0, color = "sky blue", linetype = 1, size = 2, alpha = .5) + 
  geom_boxplot(contemp_randomeff, mapping = aes(x = weight, y = as.factor(edge_name)), alpha = .1) + 
  geom_point(contemp_geneff, mapping = aes(x = weight,  y = as.factor(edge_name)), color = "black", shape = 15, size = 3) + 
  theme_classic() + 
  labs(x = "Weight", y = "Edge", color = "PBA Score") +
  scale_color_viridis_c(option="plasma") +
  xlim(-1, .6) +
  ggtitle("B. Random Effects from the Contemporaneous Network")

### 5.3 Plotting both together & exporting -------------------------------------------------------------

# Saving file as EMF in output/Figures file, with width/height specified
#devEMF::emf(file = here::here("output", "Figures", "Fig2_FixedvsRandom_ColoredDots.emf"), width = 10, height = 8, pointsize = 10)
cairo_ps(file = here::here("output", "Figures", "Fig2_FixedvsRandom_ColoredDots.eps"), width = 10, height = 8, pointsize = 10)
  # FYI - using cairo to generate EPS figure to keep semi-transparency of dots

#Plotting
listplots <- list(plot_temprand, plot_contemprand)
patchwork::wrap_plots(listplots, nrow=2, guides = "collect")

dev.off()
