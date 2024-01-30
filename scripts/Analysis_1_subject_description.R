##############################################################################
#
# Subject selection
#
# In this script, we selected the data of the subjects who is suitable in 
# this study.
#
# Liang Qunjun 2023/11/13

library(tidyverse)
library(NbClust)
library(ggiraphExtra)
library(ggsci)
library(ggeasy)
library(tidyLPA)
library(psych)
library(mclust)
library(ggsignif)
library(patchwork)
library(scales)
source("scripts/function_PvalueForTable1.R")

# load the data
dat_sbj <- rio::import('inputs/participants.xlsx')

# fix the education 
dat_sbj <- dat_sbj %>% mutate(educations = ifelse(education == 1, 'Illiterate',
                                                  ifelse(education == 2, 'Primary education',
                                                         ifelse(education == 3, 'Junior high school',
                                                                ifelse(education == 4, 'Senior high school',
                                                                       ifelse(education == 5, 'Undergraduate', 'Graduate'))))))

# summary the clinical scales
dat_mdd <- dat_sbj %>% filter(group == "MDD")
dat_hc <- dat_sbj %>% filter(group == "HC")

#############################################################
#
# calculate the factor and total score
#
#############################################################

# define the items
item_A <- c(1,2,7,8,10,13)
item_B <- c(4,5,6,9,11,12,14,15,17)
item_C <- c(3,16)

# calculate the factor and total score
hamd <- dat_mdd %>% select(participant_id, contains('HAMD_item')) 
hamd <- hamd %>%
  mutate(HAMD_total = rowSums(hamd[,2:ncol(hamd)])) %>%
  mutate(HAMD_A = rowSums(hamd[, 1+item_A])) %>%
  mutate(HAMD_B = rowSums(hamd[, 1+item_B])) %>%
  mutate(HAMD_C = rowSums(hamd[, 1+item_C]))

# calculate HAMA wave1 total score
hama <- dat_mdd %>% select(participant_id, contains('HAMA_item')) 
hama <- hama %>% mutate(HAMA_total = rowSums(hama[,2:ncol(hama)]) )

# merger the factor to the main data table
dat_mdd_factor <- dat_mdd %>% left_join(hamd) %>% left_join(hama)

dat_mdd_factor <- dat_mdd_factor %>% filter(HAMD_total >= 17)

##########################################################
#
# divide patients into agited and non-agited MDD
#
##########################################################

## threshold as >1 
dat_mdd_factor_psycho <- dat_mdd_factor %>% 
  mutate(psycho = ifelse(HAMD_item9 > 1, "A-MDD", 
                         ifelse(HAMD_item9 == 0 & HAMD_item8 > 1, "R-MDD","ns-MDD")))

## subgroup difference in HAMD 
bruceR::MANOVA(dat_mdd_factor_psycho,
               dv = "HAMD_total",
               between = "psycho") %>%
  bruceR::EMMEANS(effect = "psycho", p.adjust = "fdr")
# ─────────────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df      t     p     Cohen’s d [95% CI of d]
# ─────────────────────────────────────────────────────────────────────────────────
# (ns-MDD) - (A-MDD)   -5.966 (1.150) 116 -5.190 <.001 *** -1.083 [-1.590, -0.576]
# (R-MDD) - (A-MDD)    -5.026 (1.347) 116 -3.733 <.001 *** -0.913 [-1.506, -0.319]
# (R-MDD) - (ns-MDD)    0.940 (1.325) 116  0.709  .480      0.171 [-0.414,  0.755]
# ─────────────────────────────────────────────────────────────────────────────────

## subgroup difference in anxiety level 
bruceR::MANOVA(dat_mdd_factor_psycho,
               dv = "HAMA_total",
               between = "psycho") %>%
  bruceR::EMMEANS(effect = "psycho", p.adjust = "fdr")
# ─────────────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df      t     p     Cohen’s d [95% CI of d]
# ─────────────────────────────────────────────────────────────────────────────────
# (ns-MDD) - (A-MDD)   -5.735 (1.374) 116 -4.175 <.001 *** -0.871 [-1.378, -0.364]
# (R-MDD) - (A-MDD)    -4.152 (1.609) 116 -2.580  .017 *   -0.631 [-1.225, -0.037]
# (R-MDD) - (ns-MDD)    1.583 (1.583) 116  1.000  .319      0.241 [-0.344,  0.825]
# ─────────────────────────────────────────────────────────────────────────────────

## subgroup difference in anxiety level 
bruceR::MANOVA(dat_mdd_factor_psycho,
               dv = "HAMD_A",
               between = "psycho") %>%
  bruceR::EMMEANS(effect = "psycho", p.adjust = "fdr")
# ─────────────────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df      t     p     Cohen’s d [95% CI of d]
# ─────────────────────────────────────────────────────────────────────────────────
# (ns-MDD) - (A-MDD)   -2.616 (0.602) 116 -4.348 <.001 *** -0.907 [-1.414, -0.400]
# (R-MDD) - (A-MDD)    -0.544 (0.705) 116 -0.772  .442     -0.189 [-0.783,  0.405]
# (R-MDD) - (ns-MDD)    2.072 (0.693) 116  2.988  .005 **   0.719 [ 0.134,  1.303]
# ─────────────────────────────────────────────────────────────────────────────────

##############################################################
#
# Export the new subject information
#
##############################################################
# merge mdd and HC
dat_merge <- data.table::rbindlist(list(dat_mdd_factor_psycho, dat_hc), fill = T)
dat_merge <- dat_merge %>% 
  mutate(subtype_psycho=ifelse(group == 'MDD', psycho, group))

# summarise the data
dat_merge$educations <- factor(dat_merge$educations, 
                               levels = c("Illiterate","Primary education", 
                                          "Junior high school", "Senior high school",
                                          "Undergraduate", "Graduate"))


table1::table1(~ age + gender + educations +
                 HAMD_total +
                 HAMD_A +
                 HAMD_B +
                 HAMD_C + 
                 HAMA_total | subtype_psycho,
               data = dat_merge %>% filter(group == "MDD"))


table1::table1(~ age + gender + educations 
               | subtype_psycho,
               data = dat_merge)

rio::export(dat_merge, file = "inputs/Analysis1_participants.xlsx")

