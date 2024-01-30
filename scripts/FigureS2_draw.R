###########################################################
#
# This script is used to visualize the global to SMN time 
# delay in four extreme groups. 
#
#  Low Anxiety - Low agitation
#  Low Anxiety - High agitation
#  High Anxiety - Low agitation
#  High Anxiety - High Anxiety
#
# Liang Qunjun 2023-12-20

library(tidyverse)
library(ggeasy)
library(ggsci)

# load the data -----------------------------
dat_raw <- rio::import("inputs/Analysis2_collect_fMRI.xlsx") 
dat_mdd <- dat_raw %>% filter(group == "MDD")

# obtain the quarteile of anxiety level --------------------------------
quantile(dat_mdd$HAMA_total)
# 0%  25%  50%  75% 100% 
# 5.0 16.0 21.0 26.5 38.0 

# identify low anxiety (noAanx) and high anxiety (hAnx) subject 
dat_noAnx <- dat_mdd %>% filter(HAMA_total <= 16)
dat_hAnx <- dat_mdd %>% filter(HAMA_total > 26)

# identify the subgroup based on the combanition between anxiety and agitation
dat_noAnx_noAgi <- dat_noAnx %>% filter(HAMD_item9 == 0) %>%
  mutate(subtypr_anxiety = "lowAnx_lowAgi")
dat_hAnx_hAgi <- dat_hAnx %>% filter(subtype_psycho == "A-MDD") %>%
  mutate(subtypr_anxiety = "hiAnx_hiAgi")
dat_noAnx_hAgi <- dat_noAnx %>% filter(subtype_psycho == "A-MDD") %>%
  mutate(subtypr_anxiety = "lowAnx_hiAgi")
dat_hAnx_noAgi <- dat_hAnx %>% filter(HAMD_item9 == 0) %>%
  mutate(subtypr_anxiety = "hiAnx_lowAgi")

# merge the subgroup 
dat_anx_agi_all <- rbind(dat_noAnx_noAgi, dat_noAnx_hAgi, dat_hAnx_hAgi, dat_hAnx_noAgi)

# visualization with violine plot ------------------------------------------
p_vol <- ggplot(data = dat_anx_agi_all, aes(x = subtypr_anxiety, y = td_SMN)) +
  geom_violin(aes(fill = subtypr_anxiety), alpha = .6, color = "white") +
  geom_point(size = 4, alpha = .5, position = position_jitter(.02)) +
  scale_fill_lancet() +
  xlab("Subgroups") + ylab("Global to SMN time delay") +
  theme_classic() +
  easy_remove_legend() +
  easy_text_size(15) 


ggsave(plot = p_vol, width = 9, height = 7,
       filename = "outputs/FigS2_anxiety_agitation_between.png")
