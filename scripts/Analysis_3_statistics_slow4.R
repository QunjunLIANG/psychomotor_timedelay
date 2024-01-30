#############################################################################
#
# Temporal feature abnormality for Agitated MDD 
#
# For this pipeline, we tested the temporal structure among agitated MDD, HC 
# and retarded MD, focuing on slow-4 band (0.027 - 0.073 Hz) 
#
# The alternative features include:
#   1. Time delay in SMN (TD) - global
#   2. Time delay for intra-SMN - local
#   3. correlation between SMN TDp and agitation
#   4. correlation between intra-SMN and agitation
#   5. correlation between SMN TDp and retardation
#   6. correlation between intra-SMN and retardation
#
# Liang Qunjun 2024/01/03

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
# load the data
dat_use <- rio::import("inputs/Analysis2_fMRI_slow4.xlsx")

###########################################
#
# difference between A-MDD and NA-MDD 
#
###########################################

### time delay in SMN global ------------------------------------------
dat_use %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(group != "HC") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_SMN", 
                 between = "subtype_psycho", 
                 covariate = c("HAMD_A","HAMA_total")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho")
# ──────────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E. df     t     p     Cohen’s d [95% CI of d]
# ──────────────────────────────────────────────────────────────────────────────
# (R-MDD) - (A-MDD)    0.057 (0.020) 67 2.793  .007 **     0.714 [0.204, 1.225]
# ──────────────────────────────────────────────────────────────────────────────

### time delay for intra-SMN ------------------------------------------
dat_use %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(group != "HC") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_intraSMN", 
                 between = "subtype_psycho", 
                 covariate = c("HAMD_A","HAMA_total")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho")
# ───────────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E. df      t     p     Cohen’s d [95% CI of d]
# ───────────────────────────────────────────────────────────────────────────────
# (R-MDD) - (A-MDD)   -0.001 (0.047) 67 -0.029  .977      -0.007 [-0.518, 0.503]
# ───────────────────────────────────────────────────────────────────────────────

###########################################
#
# difference between A-MDD and HC
#
###########################################

### Time delay in SMN global ------------------------------------------
dat_use %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(subtype_psycho != "R-MDD") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_SMN", 
                 between = "subtype_psycho", 
                 covariate = c("age","gender")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho", p.adjust = "none")
# ───────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df      t     p     Cohen’s d [95% CI of d]
# ───────────────────────────────────────────────────────────────────────────
# HC - (A-MDD)   -0.010 (0.016) 134 -0.622  .535      -0.124 [-0.517, 0.269]
# ───────────────────────────────────────────────────────────────────────────

### Time delay for intra-SMN ------------------------------------------
dat_use %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(subtype_psycho != "R-MDD") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_intraSMN", 
                 between = "subtype_psycho", 
                 covariate = c("age","gender")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho", p.adjust = "none")
# ──────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df     t     p     Cohen’s d [95% CI of d]
# ──────────────────────────────────────────────────────────────────────────
# HC - (A-MDD)    0.014 (0.035) 134 0.385  .701       0.077 [-0.317, 0.470]
# ──────────────────────────────────────────────────────────────────────────

############################################
#
# difference between R-MDD and HC
#
###########################################

### Time delay in SMN global ------------------------------------------
dat_use %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(subtype_psycho != "A-MDD") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_SMN", 
                 between = "subtype_psycho", 
                 covariate = c("age","gender")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho", p.adjust = "none")
# ──────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df     t     p     Cohen’s d [95% CI of d]
# ──────────────────────────────────────────────────────────────────────────
# (R-MDD) - HC    0.061 (0.017) 117 3.494 <.001 ***    0.822 [0.356, 1.288]
# ──────────────────────────────────────────────────────────────────────────

### Time delay for intra-SMN ------------------------------------------
dat_use %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(subtype_psycho != "A-MDD") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_intraSMN", 
                 between = "subtype_psycho", 
                 covariate = c("age","gender")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho", p.adjust = "none")
# ───────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df      t     p     Cohen’s d [95% CI of d]
# ───────────────────────────────────────────────────────────────────────────
# (R-MDD) - HC   -0.008 (0.040) 117 -0.188  .851      -0.044 [-0.510, 0.422]
# ───────────────────────────────────────────────────────────────────────────

##############################################
#
# p adjust for group difference using FDR method
#
##############################################
# adjust for SMN TDp
p.adjust(p = c(.007, .535, .001))
# 0.014 0.535 0.003

######################################################
#
# correlation to agitation 
#
#####################################################

# agitation and time delay correlation
dat_use %>% filter(subtype_psycho != "HC") %>%
  ggscatterstats(x = HAMD_item9, y = td_SMN)
# r = -0.09, p = 0.32

dat_use %>% filter(subtype_psycho != "HC") %>%
  ggscatterstats(x = HAMD_item9, y = td_intraSMN)
# r = -0.11, p = 0.25

# retardation and time delay - filter out agitation patients
dat_use %>% filter(subtype_psycho != "HC") %>%
  filter(HAMD_item9 == 0) %>%
  ggscatterstats(x = HAMD_item8, y = td_SMN)
# r = 0.44, p < 0.001

dat_use %>% filter(subtype_psycho != "HC") %>%
  filter(HAMD_item9 == 0) %>%
  ggscatterstats(x = HAMD_item8, y = td_intraSMN)
# r = -0.37, p = 0.01

#######################################################
#
# moderation model
#
#######################################################
dat_mdd <- dat_use %>% filter(group != "HC")

bruceR::PROCESS(dat_mdd, y = "HAMA_total", 
                x = "HAMD_item9", 
                mods = "td_SMN", 
                mod.path = "all",
                covs = c("age","gender"))
# Interaction Effect on "HAMA_total" (Y)
# ──────────────────────────────────────────────────
# F df1 df2     p    
# ──────────────────────────────────────────────────
# HAMD_item9 * td_SMN  13.68   1 113 <.001 ***
#   ──────────────────────────────────────────────────
# 
# Simple Slopes: "HAMD_item9" (X) ==> "HAMA_total" (Y)
# ─────────────────────────────────────────────────────────────
# "td_SMN"      Effect    S.E.     t     p            [95% CI]
# ─────────────────────────────────────────────────────────────
# -0.100 (- SD)  4.175 (0.722) 5.778 <.001 *** [ 2.743, 5.606]
# -0.013 (Mean)  2.340 (0.491) 4.765 <.001 *** [ 1.367, 3.313]
# 0.074 (+ SD)   0.505 (0.673) 0.751  .454     [-0.827, 1.838]
# ─────────────────────────────────────────────────────────────

bruceR::PROCESS(dat_mdd, y = "HAMA_total", 
                x = "HAMD_item9", 
                mods = "td_intraSMN", 
                mod.path = "all",
                covs = c("age","gender"))
# Interaction Effect on "HAMA_total" (Y)
# ──────────────────────────────────────────────────────
# F df1 df2     p    
# ──────────────────────────────────────────────────────
# HAMD_item9 * td_intraSMN  0.98   1 113  .323    
# ──────────────────────────────────────────────────────

#######################################################
#
# visualization
#
#######################################################

## global-local SMN 
p_global <- dat_use %>% filter(subtype_psycho != "ns-MDD") %>%
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
p_global

ggsave(filename = "outputs/Figure5_slow4.png", plot = p_global, width = 6, height = 4)

## correlation plot 
dat_use %>% filter(subtype_psycho != "HC") %>%
  filter(HAMD_item9 == 0) %>%
  ggplot(aes(x = HAMD_item8, y = td_SMN)) +
  geom_point(size = 4, alpha =  .8) +
  geom_smooth(method = "lm", color = "darkred") +
  xlab("Retardation score") +
  ylab("Time delay") +
  ggtitle("SMN projection map") +
  theme_classic() + 
  easy_text_size(20)

ggsave(filename = "outputs/Analysis3_slow4_corDot.png", width = 7, height = 5)
