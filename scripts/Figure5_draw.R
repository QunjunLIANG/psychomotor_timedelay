######################################################################
#
# This script is used to visulize the results in Figure 5
#
#
# Liang Qunjun 2023-12-09

library(tidyverse)
library(bruceR)
library(ggstatsplot)
library(ggridges)
library(psych)
library(RColorBrewer)
library(emmeans)
library(ggeasy)
library(ggsci)
library(patchwork)
library(cowplot)
library(scales)
library(ggsignif)
library(patchwork)
library(sjPlot)

sbj_info <- rio::import("inputs/Analysis1_subject_table.xlsx")

########################## Plot the boxplot for comparison ###################
group_ind <- sbj_info %>% select(participant_id, subtype_psycho)

## for slow-4 band ---------------------------------------------
### laod the data
dat_use <- rio::import("inputs/Analysis2_collect_fMRI_slow4.xlsx") %>% 
  left_join(group_ind) %>%
  filter(subtype_psycho != "ns-MDD")

dat_use$subtype_psycho <- factor(dat_use$subtype_psycho, 
                                 levels = c("A-MDD","HC","R-MDD"))
### visualization
p_slow4 <- dat_use %>% filter(subtype_psycho != "ns-MDD") %>%
  ggplot(aes(y = td_SMN, x = subtype_psycho, fill = subtype_psycho)) +
  geom_violin(alpha = .4, color = "grey") +
  geom_boxplot(size =1 , width = .4, alpha = .6, outlier.colour = NA) +
  geom_signif(annotations = c("***", "**"), 
              textsize = 7, vjust = .7,
              y_position = c(0.21,.23), 
              xmin = c(2,1), 
              xmax = c(3,3),
              tip_length = 0) +
  scale_fill_lancet() +
  ylab("Time delay") +
  ggtitle("Slow-4 band") +
  theme_classic() +
  easy_text_size(20) + easy_remove_x_axis(what = "title") +
  easy_remove_legend()

## for slow-5 band ---------------------------------------------
### laod the data
dat_use <- rio::import("inputs/Analysis2_collect_fMRI_slow5.xlsx") %>% 
  left_join(group_ind) %>%
  filter(subtype_psycho != "ns-MDD")

dat_use$subtype_psycho <- factor(dat_use$subtype_psycho, 
                                 levels = c("A-MDD","HC","R-MDD"))
### visualization
p_slow5 <- dat_use %>% filter(subtype_psycho != "ns-MDD") %>%
  ggplot(aes(y = td_SMN, x = subtype_psycho, fill = subtype_psycho)) +
  geom_violin(alpha = .4, color = "grey") +
  geom_boxplot(size =1 , width = .4, alpha = .6, outlier.colour = NA) +
  scale_fill_lancet() +
  ylab("Time delay") +
  ggtitle("Slow-5 band") +
  theme_classic() +
  easy_text_size(20) + easy_remove_x_axis(what = "title") +
  easy_remove_legend()

p_final <- p_slow4 + p_slow5

ggsave(plot = p_final, filename = "outputs/Figure5_combine.png", width = 12, height = 5)

########################## Plot the time delay matrix #########################

# MATRIX for A-MDD ---------------------------------------------------------
dat_amdd <- sbj_info %>% filter(subtype_psycho == "A-MDD")

# load the path 
path_td <- "inputs/timeseries_Power/time_lag_estimation/"
bands <- str_extract(path_td, pattern = "slow.")
if (is.na(bands)) {
  bands <- "Typical"
}
file_names <- list.files(path = path_td, pattern = "sub-[0-9]*_timeDelay.csv", full.names = T)
file_use <- file_names[grep(paste0(dat_amdd$participant_id, collapse = "|"), file_names)]
dat_mat <- matrix(nrow = 214, ncol = 214, data = 0)
for (i in file_use) {
  dat_tmp <- rio::import(i) %>% as.matrix()
  dat_tmp[is.na(dat_tmp)] <- 0
  dat_mat <- dat_mat + dat_tmp
}
grp_lags_mean <- dat_mat / ncol(dat_amdd)

# import the network identification
assns <- rio::import("Python_MATLAB_scripts/time_delay/Power264_net_identity.csv") %>% .$V1

# sort matric by lag
sorted_inds1 <- order(colMeans(grp_lags_mean, na.rm = T))
assns_sort <- assns[sorted_inds1]

grp_lags_temp <- grp_lags_mean[sorted_inds1, sorted_inds1]

# sort by network
sorted_inds2 <- order(assns_sort)
grp_lags_mat <- grp_lags_temp[sorted_inds2, sorted_inds2]

Cairo::Cairo(file = bruceR::Glue("outputs/TD_matrix_AMDD_{bands}.png"), width = 15, height = 12,
             units = "cm", dpi = 300)
lattice::levelplot(grp_lags_mat, scales = list(x = list(draw = F),y = list(draw = F)),
                   main = bruceR::Glue("{bands} band"), at = seq(-.06,.06, l = 100),
                   xlab = NULL, ylab = NULL, col.regions = rev(hcl.colors(palette = "Spectral", 200)))
dev.off()

# MATRIX for R-MDD ---------------------------------------------------------
dat_amdd <- sbj_info %>% filter(subtype_psycho == "R-MDD")

# load the path 
path_td <- "inputs/timeseries_Power/time_lag_estimation/"
bands <- str_extract(path_td, pattern = "slow.")
if (is.na(bands)) {
  bands <- "Typical"
}
file_names <- list.files(path = path_td, pattern = "sub-[0-9]*_timeDelay.csv", full.names = T)
file_use <- file_names[grep(paste0(dat_amdd$participant_id, collapse = "|"), file_names)]
dat_mat <- matrix(nrow = 214, ncol = 214, data = 0)
for (i in file_use) {
  dat_tmp <- rio::import(i) %>% as.matrix()
  dat_tmp[is.na(dat_tmp)] <- 0
  dat_mat <- dat_mat + dat_tmp
}
grp_lags_mean <- dat_mat / ncol(dat_amdd)

# import the network identification
assns <- rio::import("Python_MATLAB_scripts/time_delay/Power264_net_identity.csv") %>% .$V1

# sort matric by lag
sorted_inds1 <- order(colMeans(grp_lags_mean, na.rm = T))
assns_sort <- assns[sorted_inds1]

grp_lags_temp <- grp_lags_mean[sorted_inds1, sorted_inds1]

# sort by network
sorted_inds2 <- order(assns_sort)
grp_lags_mat <- grp_lags_temp[sorted_inds2, sorted_inds2]

Cairo::Cairo(file = bruceR::Glue("outputs/TD_matrix_RMDD_{bands}.png"), width = 15, height = 12,
             units = "cm", dpi = 300)
lattice::levelplot(grp_lags_mat, scales = list(x = list(draw = F),y = list(draw = F)),
                   main = bruceR::Glue("{bands} band"), at = seq(-.06,.06, l = 100),
                   xlab = NULL, ylab = NULL, col.regions = rev(hcl.colors(palette = "Spectral", 200)))
dev.off()