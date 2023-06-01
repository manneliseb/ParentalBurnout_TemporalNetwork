#---------------------------------------------------------------#
# All libraries for RStudio project                             #
#---------------------------------------------------------------#

#This R script includes all packages for the entire R studio project, i.e., all
#code related to this project: https://osf.io/5sgkr/

# FYI: All packages should be directly called before any relevant functions (and
# so do not necessarily need to be loaded if they are already installed), with
# the exception of tidyverse

#################################################################
# First: check that the top-level directory of the project is correct 
here::i_am("scripts/01_libraries.R") 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R packages -----------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# For entire project
library(here) # for relative paths within project
library(tidyverse) # also loads lubridate (02)
#library(lubridate) # to deal with time data (dates, etc) - also loaded with tidyverse

# For script 2: 02_wranglingdata_demographicdescriptives
# nothing more than general ones above (including lubridate from tidyverse)

# For script 3: 03_demographicStats_questionnaireReliability
library(skimr) # summary functions for descriptive data  

# For script 4: 04_ESM_summarystats_assumptions
library(psychometric) # calculate ICCs 
library(qgraph) # for cor_auto 
library(networktools) # for goldbricker function, i.e. node redundancy 
library(moments) # used for skewness/kurtosis 
library(LambertW) #to correct non-normal data 
library(tseries) #for KPSS stationarity test 
library(stats) # for Shapiro-Wilkes test + linear models when looking at stationarity

# For script 5: 05_MainAnalyses_mlVARNetworkModel
library(qgraph) # Visualizing networks
library(mlVAR) # Computing multilevel vector autoregressive models
library(devEMF) # To save figures as EMF (if doing so)
library(patchwork) # to arrange random effects plots (with axes aligned)
library(ggbeeswarm) # For jitter in variance (fixed vs random effects) boxplot figure


# For script 6: 06_SensitivityAnalyses
# Same as script 5 

# For script 7: 07_SupplementaryMaterials
library(qgraph) # For centrality estimates etc
library(ggpubr) # to arrange plots
library(mlVAR) # Computing mlVAR models (for stability analyses re: centrality)
library(devEMF) # To save figures as EMF (if doing so)

#-------------------------------------#
####        R Session Info         ####
#-------------------------------------#
#
sessionInfo()
#
# R version 4.1.0 (2021-05-18)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# other attached packages: 
# [1] ggbeeswarm_0.6.0   ggpubr_0.4.0       patchwork_1.1.1    devEMF_4.0-2       mlVAR_0.5         
# [6] tseries_0.10-48    LambertW_0.6.6     moments_0.14       networktools_1.2.3 qgraph_1.6.9      
# [11] psychometric_2.2   multilevel_2.6     MASS_7.3-54        nlme_3.1-152       skimr_2.1.3       
# [16] forcats_0.5.1      stringr_1.4.0      dplyr_1.0.7        purrr_0.3.4        readr_2.1.3       
# [21] tidyr_1.1.3        tibble_3.1.3       ggplot2_3.3.5      tidyverse_1.3.1    here_1.0.1        
