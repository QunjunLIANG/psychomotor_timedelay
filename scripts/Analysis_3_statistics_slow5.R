#############################################################################
#
# Temporal feature abnormality for Agitated MDD 
#
# For this pipeline, we tested the temporal structure among agitated MDD, HC 
# and retarded MD, focuing on slow-5 band (0.01 - 0.027 Hz) 
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
dat_use <- rio::import("inputs/Analysis2_fMRI_slow5.xlsx") 
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
# ───────────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E. df      t     p     Cohen’s d [95% CI of d]
# ───────────────────────────────────────────────────────────────────────────────
# (R-MDD) - (A-MDD)   -0.020 (0.066) 67 -0.306  .761      -0.078 [-0.589, 0.432]
# ───────────────────────────────────────────────────────────────────────────────

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
# (R-MDD) - (A-MDD)   -0.017 (0.045) 67 -0.372  .711      -0.095 [-0.606, 0.415]
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
# ──────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df     t     p     Cohen’s d [95% CI of d]
# ──────────────────────────────────────────────────────────────────────────
# HC - (A-MDD)    0.002 (0.052) 134 0.039  .969       0.008 [-0.385, 0.401]
# ──────────────────────────────────────────────────────────────────────────

### Time delay for intra-SMN ------------------------------------------
dat_use %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(subtype_psycho != "R-MDD") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_intraSMN", 
                 between = "subtype_psycho", 
                 covariate = c("age","gender")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho", p.adjust = "none")
# ───────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df      t     p     Cohen’s d [95% CI of d]
# ───────────────────────────────────────────────────────────────────────────
# HC - (A-MDD)   -0.096 (0.038) 134 -2.504  .013 *   -0.498 [-0.891, -0.105]
# ───────────────────────────────────────────────────────────────────────────

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
# ───────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df      t     p     Cohen’s d [95% CI of d]
# ───────────────────────────────────────────────────────────────────────────
# (R-MDD) - HC   -0.050 (0.064) 117 -0.773  .441      -0.182 [-0.648, 0.284]
# ───────────────────────────────────────────────────────────────────────────

### Time delay for intra-SMN ------------------------------------------
dat_use %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(subtype_psycho != "A-MDD") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_intraSMN", 
                 between = "subtype_psycho", 
                 covariate = c("age","gender")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho", p.adjust = "none")
# ──────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df     t     p     Cohen’s d [95% CI of d]
# ──────────────────────────────────────────────────────────────────────────
# (R-MDD) - HC    0.085 (0.042) 117 2.015  .046 *      0.474 [0.008, 0.940]
# ──────────────────────────────────────────────────────────────────────────

##############################################
#
# p adjust for group difference using FDR method
#
##############################################
# adjust for intra-SMN
p.adjust(p = c(.711, .013, .046))
# 0.711 0.039 0.092

######################################################
#
# correlation to agitation 
#
#####################################################

# agitation and time delay correlation
dat_use %>% filter(subtype_psycho != "HC") %>%
  ggscatterstats(x = HAMD_item9, y = td_SMN)
# r = 0.07, p = 0.42

dat_use %>% filter(subtype_psycho != "HC") %>%
  ggscatterstats(x = HAMD_item9, y = td_intraSMN)
# r = 0.05, p = 0.58

# retardation and time delay - filter out agitation patients
dat_use %>% filter(subtype_psycho != "HC") %>%
  filter(HAMD_item9 == 0) %>%
  ggscatterstats(x = HAMD_item8, y = td_SMN)
# r = -0.05, p = 0.75

dat_use %>% filter(subtype_psycho != "HC") %>%
  filter(HAMD_item9 == 0) %>%
  ggscatterstats(x = HAMD_item8, y = td_intraSMN)
# r = 0.07, p = 0.64

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
# ─────────────────────────────────────────────────
# F df1 df2     p    
# ─────────────────────────────────────────────────
# HAMD_item9 * td_SMN  0.39   1 113  .532    
# ─────────────────────────────────────────────────

bruceR::PROCESS(dat_mdd, y = "HAMA_total", 
                x = "HAMD_item9", 
                mods = "td_intraSMN", 
                mod.path = "all",
                covs = c("age","gender"))
# Interaction Effect on "HAMA_total" (Y)
# ──────────────────────────────────────────────────────
# F df1 df2     p    
# ──────────────────────────────────────────────────────
# HAMD_item9 * td_intraSMN  0.13   1 113  .719    
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
  scale_fill_lancet() +
  ylab("Time delay") +
  ggtitle("Slow-5 band") +
  theme_classic() +
  easy_text_size(20) + easy_remove_x_axis(what = "title") +
  easy_remove_legend()
p_global

ggsave(filename = "outputs/Figure5_slow5.png", plot = p_global, width = 6, height = 4)
