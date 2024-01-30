############################################################################
#
# Collecting fMRI metrics
#
# This script is used to generated the metrics for the sequential analysis,
# including:
#   1. Global to SMN time delay: time delay projection map averaging SMN RIOs
#   2. Local SMN time delay: time delay averaging within SMN
#
# Liang Qunjun 2023/11/11

library(tidyverse)
library(bruceR)
library(ggstatsplot)
library(ggridges)
library(psych)
library(brainconn)
library(RColorBrewer)
library(emmeans)
library(ggeasy)
library(ggsci)
library(cowplot)
library(scales)
library(ggsignif)
library(patchwork)
library(sjPlot)
source('scripts/function_ObtainNetID.R')
source('scripts/function_ObtainBrainData_individual.R')
source('scripts/function_AddNetworkScore.R')
source("scripts/function_ObtainTDinSMN.R")

# indicate the path to the inputs and participant information
path_td <- "inputs/timeseries_Power/time_lag_estimation/"
sbj_info <- rio::import('inputs/Analysis1_participants.xlsx')
net_anna <- rio::import("inputs/Power264_Yeo7.xlsx")
outfile_name <- "inputs/Analysis2_collect_fMRI.xlsx"

######################################################
#
# Collect all fMRI metrices
#
####################################################

#############   Collect for time delay   ##############
# load the data
file_names <- list.files(path_td, pattern = 'sub-[0-9]*_projection_map_weighted', full.names = T)
file_use <- file_names[grep(paste(sbj_info$participant_id, collapse = "|"), file_names)]
dat_td_raw <- map_dfr(data.frame(file_use), ObtainBrainData_individual, .progress = T)
colnames(dat_td_raw)[2:ncol(dat_td_raw)] <- paste0("R",formatC(1:214, width = 3, flag = "0"))
## add network-level mean TDp
dat_td_net <- AddNetworkScore_Power(dat_td_raw, net_anna)

#################  Collect for intraSMN and inter-net   ####################
# load the data
file_names <- list.files(path_td, pattern = 'sub-[0-9]*_timeDelay.csv', full.names = T)
file_use <- file_names[grep(paste(sbj_info$participant_id, collapse = "|"), file_names)]

dat_intraSMN <- data.frame()
for (i in file_use) {
  dat_tmp <- ObtainTDinSMN(i, net_anna = net_anna)
  dat_intraSMN <- rbind(dat_intraSMN, dat_tmp)
}

############### merge all fMRI metrices ##################
dat_use <- sbj_info %>% left_join(dat_td_net) %>%
  left_join(dat_intraSMN)

######################################################
#
# Export to the outer
#
####################################################
rio::export(dat_use, file = outfile_name)


#################### merge the data for slow 4 and slow 5 ######################
dat_slow4 <- rio::import("inputs/Data_fMRI_slow4.xlsx")
dat_slow5 <- rio::import("inputs/Data_fMRI_slow5.xlsx")

dat_slow4_merg <- sbj_info %>% left_join(dat_slow4)
dat_slow5_merg <- sbj_info %>% left_join(dat_slow5)

# save the merged data
rio::export(dat_slow4_merg, file = "inputs/Analysis2_fMRI_slow4.xlsx")
rio::export(dat_slow5_merg, file = "inputs/Analysis2_fMRI_slow5.xlsx")
