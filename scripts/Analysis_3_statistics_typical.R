#############################################################################
#
# Temporal feature abnormality for Agitated MDD 
#
# For this pipeline, we tested the temporal structure among agitated MDD, HC 
# and retarded MD, focuing on typical band (0.01 - 0.08 Hz) 
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
dat_use <- rio::import("inputs/Analysis2_collect_fMRI.xlsx")

###########################################
#
# difference between A-MDD and NA-MDD 
#
###########################################

### time delay in SMN global ------------------------------------------
dat_use  %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(group != "HC")  %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_SMN", 
                 between = "subtype_psycho", 
                 covariate = c("HAMD_A","HAMA_total")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho")
# Pairwise Comparisons of "subtype_psycho":
# ──────────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E. df     t     p     Cohen’s d [95% CI of d]
# ──────────────────────────────────────────────────────────────────────────────
# (R-MDD) - (A-MDD)    0.041 (0.013) 67 3.256  .002 **     0.833 [0.322, 1.343]
# ──────────────────────────────────────────────────────────────────────────────

### time delay for intra-SMN ------------------------------------------
dat_use  %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(group != "HC") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_intraSMN", 
                 between = "subtype_psycho", 
                 covariate = c("HAMD_A","HAMA_total")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho")
# Pairwise Comparisons of "subtype_psycho":
# ───────────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E. df      t     p     Cohen’s d [95% CI of d]
# ───────────────────────────────────────────────────────────────────────────────
# (R-MDD) - (A-MDD)   -0.011 (0.038) 67 -0.296  .768      -0.076 [-0.586, 0.435]
# ───────────────────────────────────────────────────────────────────────────────

###########################################
#
# difference between A-MDD and HC
#
###########################################

### Time delay in SMN global ------------------------------------------
dat_use  %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(subtype_psycho != "R-MDD") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_SMN", 
                 between = "subtype_psycho", 
                 covariate = c("age","gender")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho", p.adjust = "none")
# ──────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df     t     p     Cohen’s d [95% CI of d]
# ──────────────────────────────────────────────────────────────────────────
# HC - (A-MDD)    0.004 (0.011) 134 0.367  .714       0.073 [-0.320, 0.466]
# ──────────────────────────────────────────────────────────────────────────

### Time delay for intra-SMN ------------------------------------------
dat_use  %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(subtype_psycho != "R-MDD") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_intraSMN", 
                 between = "subtype_psycho", 
                 covariate = c("age","gender")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho", p.adjust = "none")
# ──────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df     t     p     Cohen’s d [95% CI of d]
# ──────────────────────────────────────────────────────────────────────────
# HC - (A-MDD)    0.028 (0.029) 134 0.966  .336       0.192 [-0.201, 0.585]
# ──────────────────────────────────────────────────────────────────────────

############################################
#
# difference between R-MDD and HC
#
###########################################

### Time delay in SMN global ------------------------------------------
dat_use  %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(subtype_psycho != "A-MDD") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_SMN", 
                 between = "subtype_psycho", 
                 covariate = c("age","gender")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho", p.adjust = "none")
# ──────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df     t     p     Cohen’s d [95% CI of d]
# ──────────────────────────────────────────────────────────────────────────
# (R-MDD) - HC    0.032 (0.013) 117 2.493  .014 *      0.586 [0.121, 1.052]
# ──────────────────────────────────────────────────────────────────────────

### Time delay for intra-SMN ------------------------------------------
dat_use  %>%
  filter(subtype_psycho != "ns-MDD") %>% filter(subtype_psycho != "A-MDD") %>%
  bruceR::MANOVA(subID = "participant_id", dv = "td_intraSMN", 
                 between = "subtype_psycho", 
                 covariate = c("age","gender")) %>%
  bruceR::EMMEANS(effect = "subtype_psycho", p.adjust = "none")
# ───────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df      t     p     Cohen’s d [95% CI of d]
# ───────────────────────────────────────────────────────────────────────────
# (R-MDD) - HC   -0.013 (0.035) 117 -0.382  .703      -0.090 [-0.556, 0.376]
# ───────────────────────────────────────────────────────────────────────────

##############################################
#
# p adjust for group difference using FDR method
#
##############################################
# adjust for SMN TDp
p.adjust(p = c(.002, .793, .02))
# 0.006 0.793 0.040

# adjust for intra-SMN TD
p.adjust(p = c(.768, .336, .703))
#  1 1 1

######################################################
#
# correlation to agitation 
#
#####################################################

## SMN TDp --------------------------------------------------
# agitation and time delay correlation
dat_use %>% filter(subtype_psycho != "HC") %>%
  ggscatterstats(x = HAMD_item9, y = td_SMN)
# r = -0.19, p = 0.04

# retardation and time delay - filter out agitation patients
dat_use %>% filter(subtype_psycho != "HC") %>%
  filter(HAMD_item9 == 0) %>%
  ggscatterstats(x = HAMD_item8, y = td_SMN)
# r = 0.32, p = 0.03

## intra-SMN TD ---------------------------------------------
dat_use %>% filter(subtype_psycho != "HC") %>%
  ggscatterstats(x = HAMD_item9, y = td_intraSMN)
# r = -0.13, p = 0.16


dat_use %>% filter(subtype_psycho != "HC") %>%
  filter(HAMD_item9 == 0) %>%
  ggscatterstats(x = HAMD_item8, y = td_intraSMN)
# r = -0.27, p = 0.07

#######################################################
#
# moderation model
#
#######################################################
dat_mdd <- dat_use %>% filter(group != "HC")

model <- bruceR::PROCESS(dat_mdd, y = "HAMA_total", 
                x = "HAMD_item9", 
                mods = "td_SMN", 
                mod.path = "all",
                covs = c("age","gender"))
# Interaction Effect on "HAMA_wave1_total" (Y)
# ─────────────────────────────────────────────────
# F df1 df2     p    
# ─────────────────────────────────────────────────
# HAMD_wave1_item9 * td_SMN  5.18   1 113  .025 *  
# ─────────────────────────────────────────────────
# 
# Simple Slopes: "HAMD_wave1_item9" (X) ==> "HAMA_wave1_total" (Y)
# ─────────────────────────────────────────────────────────────
# "td_SMN"      Effect    S.E.     t     p            [95% CI]
# ─────────────────────────────────────────────────────────────
# -0.063 (- SD)  3.540 (0.740) 4.786 <.001 *** [ 2.075, 5.005]
# -0.012 (Mean)  2.334 (0.512) 4.562 <.001 *** [ 1.321, 3.348]
# 0.040 (+ SD)   1.129 (0.733) 1.539  .127     [-0.324, 2.582]
# ─────────────────────────────────────────────────────────────

bruceR::PROCESS(dat_mdd, y = "HAMA_total", 
                x = "HAMD_item9", 
                mods = "td_intraSMN", 
                mod.path = "all",
                covs = c("age","gender"))
# Interaction Effect on "HAMA_wave1_total" (Y)
# ──────────────────────────────────────────────────────
# F df1 df2     p    
# ──────────────────────────────────────────────────────
# HAMD_wave1_item9 * td_intraSMN  1.39   1 113  .241    
# ──────────────────────────────────────────────────────
