########################################
#
# Figure 4 plot
#
#
# Liang Qunjun 2023-12-20
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
source("scripts/function_PvalueForTable1.R")
source("scripts/function_ObtainBrainData_individual.R  ")
# load the data
dat_raw <- rio::import("inputs/Analysis2_collect_fMRI.xlsx") 

p_cor_SMN_agi <- dat_raw %>%
  ggplot(aes(x = HAMD_item9, y = td_SMN)) +
  geom_point(size = 4 , alpha = .6, position = position_jitter(.03)) +
  geom_smooth(method = "lm", color = "darkred") +
  xlab("Agitation score") + ylab("Time delay") +
  ggtitle("SMN projection map") +
  ylim(c(-0.14, 0.14)) +
  theme_classic() +
  easy_text_size(20)

p_cor_SMN_retar <- dat_raw %>%
  filter(HAMD_item9 == 0) %>%
  ggplot(aes(x = HAMD_item8, y = td_SMN)) +
  geom_point(size = 4 , alpha = .6, position = position_jitter(.03)) +
  geom_smooth(method = "lm", color = "darkred") +
  xlab("Retardation score") + ylab("Time delay") +
  ggtitle("SMN projection map") +
  ylim(c(-0.16, 0.14)) +
  theme_classic() +
  easy_text_size(20)

p_cor_intraSMN_agi <- dat_raw %>%
  ggplot(aes(x = HAMD_item9, y = td_intraSMN)) +
  geom_point(size = 4 , alpha = .6, position = position_jitter(.03)) +
  geom_smooth(method = "lm", color = "darkred") +
  xlab("Agitation score") + ylab("Time delay") +
  ggtitle("Intra-SMN index") +
  theme_classic() +
  easy_text_size(20)

p_cor_intraSMN_retar <- dat_raw %>%
  filter(HAMD_item9 == 0) %>%
  ggplot(aes(x = HAMD_item8, y = td_intraSMN)) +
  geom_point(size = 4 , alpha = .6, position = position_jitter(.03)) +
  geom_smooth(method = "lm", color = "darkred") +
  xlab("Retardation score") + ylab("Time delay") +
  ggtitle("Intra-SMN index") +
  ylim(c(-0.16, 0.14)) +
  theme_classic() +
  easy_text_size(20)


#########################################################
#
# Result visualization
#
#########################################################
dat_use <- dat_raw %>% filter(subtype_psycho != "ns-MDD")
dat_use$subtype_psycho <- factor(dat_use$subtype_psycho, 
                                 levels = c("A-MDD","HC","R-MDD"))

#########################################################
#
# Preparing the materials for visualizing time delay in
# SMN with MATLAB, BrainNetViewer
#
#########################################################

## reshape the network annotation 
net_anna <- rio::import("inputs/Power264_Yeo7.xlsx")
net_anna["ROI.Name"] <- paste0("R",formatC(1:214, digits = 2, flag = "0"))
net_anna <- net_anna %>%
  rename(x.mni = "x", y.mni="y", z.mni="z")
net_anna$x.mni <- as.integer(net_anna$x.mni)
net_anna$y.mni <- as.integer(net_anna$y.mni)
net_anna$z.mni <- as.integer(net_anna$z.mni)

net_anna_plot <- net_anna %>% mutate(net_color = ifelse(network_new == "somMot", "cyan",
                                                        ifelse(network_new == "ATN","green",
                                                               ifelse(network_new == "DMN", "red",
                                                                      ifelse(network_new == "salience", "purple",
                                                                             ifelse(network_new == "visual", "blue",
                                                                                    ifelse(network_new == "FPN", "orange", "black"))))))) %>%
  rename(network = "network_new") 

## add time delay 
path_td <- "inputs/timeseries_Power/time_lag_estimation/"
sbj_info <- rio::import('inputs/Analysis1_subject_table.xlsx')
file_names <- list.files(path_td, pattern = 'sub-[0-9]*_projection_map_weighted', full.names = T)
# for HC
sbj_hc <- sbj_info %>% filter(group == "HC") %>% .$participant_id 
file_use <- file_names[grep(paste(sbj_hc, collapse = "|"), file_names)]
dat_td_raw <- map_dfr(data.frame(file_use), ObtainBrainData_individual, .progress = T)
dat_td_SMN <- dat_td_raw[,-1] %>% colMeans()

net_anna_plot_this <- net_anna_plot %>% mutate(td = dat_td_SMN) %>%
  # filter(network == "somMot") %>%
  # mutate(cluster = 1) %>%
  select(1:3,11,7)
readr::write_delim(net_anna_plot_this, 
                   file = "outputs/SMN_Timedelay_HC.node", col_names = F, delim = "\t")

# for A-MDD
sbj_hc <- sbj_info %>% filter(subtype_psycho == "A-MDD") %>% .$participant_id 
file_use <- file_names[grep(paste(sbj_hc, collapse = "|"), file_names)]
dat_td_raw <- map_dfr(data.frame(file_use), ObtainBrainData_individual, .progress = T)
dat_td_SMN <- dat_td_raw[,-1] %>% colMeans()

net_anna_plot_this <- net_anna_plot %>% mutate(td = dat_td_SMN) %>%
  # filter(network == "somMot") %>%
  # mutate(cluster = 1) %>%
  select(1:3,11,7)
readr::write_delim(net_anna_plot_this, 
                   file = "outputs/SMN_Timedelay_AMDD.node", col_names = F, delim = "\t")

# for R-MDD
sbj_hc <- sbj_info %>% filter(subtype_psycho == "R-MDD") %>% .$participant_id 
file_use <- file_names[grep(paste(sbj_hc, collapse = "|"), file_names)]
dat_td_raw <- map_dfr(data.frame(file_use), ObtainBrainData_individual, .progress = T)
dat_td_SMN <- dat_td_raw[,-1] %>% colMeans()

net_anna_plot_this <- net_anna_plot %>% mutate(td = dat_td_SMN) %>%
  # filter(network == "somMot") %>%
  # mutate(cluster = 1) %>%
  select(1:3,11,7)
readr::write_delim(net_anna_plot_this, 
                   file = "outputs/SMN_Timedelay_RMDD.node", col_names = F, delim = "\t")

############################ visualize result from Time delay #######################
## global-local SMN 
p_global <- dat_use %>%
  ggplot(aes(y = td_SMN, x = subtype_psycho, fill = subtype_psycho)) +
  geom_violin(alpha = .4, color = "grey") +
  geom_boxplot(size =1 , width = .4, alpha = .6, outlier.colour = NA) +
  geom_signif(annotations = c("*", "**"), 
              textsize = 7, vjust = .7,
              y_position = c(0.13,.15), 
              xmin = c(1,2), 
              xmax = c(3,3),
              tip_length = 0) +
  scale_fill_lancet() +
  ylab("Time delay") +
  ggtitle("SMN projection map") +
  theme_classic() +
  easy_text_size(20) + easy_remove_x_axis(what = "title") +
  easy_remove_legend()

## intra-SMN
p_intra <- dat_use %>%
  ggplot(aes(y = td_intraSMN, x = subtype_psycho, fill = subtype_psycho)) +
  geom_violin(alpha = .4, color = "grey", adjust = 1) +
  geom_boxplot(size =1 , width = .4, alpha = .6, outlier.colour = NA) +
  scale_fill_lancet() +
  ylab("Time delay") +
  ggtitle("Intra-SMN index") +
  theme_classic() +
  easy_text_size(20) + easy_remove_x_axis(what = "title") +
  easy_remove_legend()

p_final <- p_cor_SMN_agi + p_cor_SMN_retar + p_global + p_intra

ggsave(filename = "outputs/Figure4.png", plot = p_final, width = 13, height = 10)

p_supp <- p_intra + p_cor_intraSMN_agi + p_cor_intraSMN_retar
ggsave(filename = "outputs/Figure4_supp.png", plot = p_supp, width = 13, height = 5)

## density plot 
dat_use %>% filter(subtype_psycho != "ns-MDD") %>%
  ggplot(aes(x = td_SMN, fill = subtype_psycho)) + 
  geom_density(alpha = .3, color = "white", size = .1) +
  geom_dotplot(alpha = .6, binwidth  = .008) +
  geom_rug(aes(color = subtype_psycho), alpha = .7,length = unit(0.06, "npc")) +
  xlab(label = "Time delay") +
  ylab(label = "Density") +
  ggtitle("SMN projection map") +
  scale_fill_lancet() +
  scale_color_lancet() +
  theme_ggstatsplot() +
  easy_text_size(20) + easy_add_legend_title("Groups")
ggsave("outputs/addition5_2.png", width = 12, height = 5)
