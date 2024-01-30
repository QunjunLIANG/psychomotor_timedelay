########################################
#
# Figure supp 1 plot 
#
#
# Liang Qunjun 2023-12-20
library(tidyverse)
library(bruceR)
library(ggstatsplot)
library(ggridges)
library(psych)
library(RColorBrewer)
library(ggeasy)
library(ggsci)
library(patchwork)
library(cowplot)
library(scales)
library(ggsignif)
library(patchwork)
source("scripts/function_PvalueForTable1.R")
source("scripts/function_ObtainBrainData_individual.R")
# load the data
dat_raw <- rio::import("inputs/Analysis2_collect_fMRI.xlsx") 

p_cor_intraSMN_agi <- dat_raw %>%
  ggplot(aes(x = HAMD_item9, y = td_intraSMN)) +
  geom_point(size = 4 , alpha = .6, position = position_jitter(.03)) +
  geom_smooth(method = "lm", color = "darkred") +
  xlab("Agitation score") + ylab("Time delay") +
  ggtitle("Intra-SMN TD") +
  theme_classic() +
  easy_text_size(20)

dat_raw %>%
  ggstatsplot::ggscatterstats(x = HAMD_item9, y = td_intraSMN)
# r = -0.13, p = 0.16

p_cor_intraSMN_retar <- dat_raw %>%
  filter(HAMD_item9 == 0) %>%
  ggplot(aes(x = HAMD_item8, y = td_intraSMN)) +
  geom_point(size = 4 , alpha = .6, position = position_jitter(.03)) +
  geom_smooth(method = "lm", color = "darkred") +
  xlab("Retardation score") + ylab("Time delay") +
  ggtitle("Intra-SMN TD") +
  ylim(c(-0.16, 0.14)) +
  theme_classic() +
  easy_text_size(20)

dat_raw %>%
  filter(HAMD_item9 == 0) %>%
  ggstatsplot::ggscatterstats(x = HAMD_item8, y = td_intraSMN)

dat_use <- dat_raw %>% filter(subtype_psycho != "ns-MDD")
dat_use$subtype_psycho <- factor(dat_use$subtype_psycho, 
                                 levels = c("A-MDD","HC","R-MDD"))

## intra-SMN
p_intra <- dat_use %>%
  ggplot(aes(y = td_intraSMN, x = subtype_psycho, fill = subtype_psycho)) +
  geom_violin(alpha = .4, color = "grey", adjust = 1) +
  geom_boxplot(size =1 , width = .4, alpha = .6, outlier.colour = NA) +
  scale_fill_lancet() +
  ylab("Time delay") +
  ggtitle("Intra-SMN TD") +
  theme_classic() +
  easy_text_size(20) + easy_remove_x_axis(what = "title") +
  easy_remove_legend()

p_final <- p_cor_intraSMN_agi + p_cor_intraSMN_retar + p_intra +
            plot_annotation(tag_levels = "A") 
p_final
ggsave(filename = "outputs/Figure_supp1.png", plot = p_final, width = 11, height = 4)

