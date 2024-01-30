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

############################# plot parameters ################################
size_range <- c(3,7)

############################# typical band moderation model ################################
# load the data
dat_typical <- rio::import("inputs/Analysis2_collect_fMRI.xlsx") 
dat_mdd_typical <- dat_typical %>% filter(subtype_psycho != "HC")

model_lm <- dat_mdd_typical %>%
  lm(HAMA_total ~ HAMD_item9*td_SMN + age + gender, data = .)
p_lm_emm <- plot_model(model_lm, type = "emm", terms=c("HAMD_item9","td_SMN")) +
  ylab("HAMA Predicted values") + xlab("Agitation score") +
  ggtitle("Simple slope for typical band") +
  theme_blank() + easy_add_legend_title("Typical") + easy_text_size(13)
p_lm_emm

p_dotSize <- ggplot(dat_mdd_typical, aes(x = td_SMN, y = HAMA_total)) +
  geom_point(aes(size = HAMD_item9),
             color = "black", alpha = .6) +
  scale_size_continuous(range = size_range) +
  xlim(c(-0.12, 0.12)) +
  xlab("SMN TDp") + ylab("HAMA score") + 
  ggtitle(label = "Typical band") +
  theme_classic() + easy_add_legend_title("Agitation") + easy_text_size(13)
p_dotSize

############################# slow-4 band moderation model ################################
# load the data
sbj_info <- rio::import("inputs/Analysis1_subject_table.xlsx") %>% select(participant_id, subtype_psycho)
dat_mdd_slow4 <- rio::import("inputs/Analysis2_collect_fMRI_slow4.xlsx") %>% 
  left_join(sbj_info) %>% filter(subtype_psycho != "HC")

model_lm <- dat_mdd_slow4 %>%
  lm(HAMA_total ~ HAMD_item9*td_SMN + age + gender, data = .)
p_lm_emm_slow4 <- plot_model(model_lm, type = "emm", terms=c("HAMD_item9","td_SMN")) +
  ylab("HAMA Predicted values") + xlab("Agitation score") +
  ggtitle("Simple slope for slow-4 band") +
  theme_blank() + easy_add_legend_title("Slow-4") + easy_text_size(13)
p_lm_emm_slow4

p_dotSize_slow4 <- ggplot(dat_mdd_slow4, aes(x = td_SMN, y = HAMA_total)) +
  geom_point(aes(size = HAMD_item9),
             color = "black", alpha = .6) +
  scale_size_continuous(range = size_range) +
  xlim(c(-0.2, 0.2)) +
  xlab("SMN TDp") + ylab("HAMA score") + 
  ggtitle(label = "Slow-4 band") +
  theme_classic() + easy_add_legend_title("Agitation") + easy_text_size(13)
p_dotSize_slow4

############################# slow-5 band moderation model ################################
# load the data
sbj_info <- rio::import("inputs/Analysis1_subject_table.xlsx") %>% select(participant_id, subtype_psycho)
dat_mdd_slow5 <- rio::import("inputs/Analysis2_collect_fMRI_slow5.xlsx") %>% 
  left_join(sbj_info) %>% filter(subtype_psycho != "HC")

model_lm <- dat_mdd_slow5 %>%
  lm(HAMA_total ~ HAMD_item9*td_SMN + age + gender, data = .)
p_lm_emm_slow5 <- plot_model(model_lm, type = "emm", terms=c("HAMD_item9","td_SMN")) +
  ylab("HAMA Predicted values") + xlab("Agitation score") +
  ggtitle("Simple slope for slow-5") +
  theme_blank() + easy_add_legend_title("slow-5") + easy_text_size(13)
p_lm_emm_slow5

p_dotSize_slow5 <- ggplot(dat_mdd_slow5, aes(x = td_SMN, y = HAMA_total)) +
  geom_point(aes(size = HAMD_item9),
             color = "black", alpha = .6) +
  scale_size_continuous(range = size_range) +
  xlim(c(-0.2, 0.2)) +
  xlab("SMN TDp") + ylab("HAMA score") + 
  ggtitle(label = "Slow-5 band") +
  theme_classic() + easy_add_legend_title("Agitation") + easy_text_size(13)
p_dotSize_slow5


############################# merge all plots ################################

p_mod <- p_dotSize + p_lm_emm +
  p_dotSize_slow4 + p_lm_emm_slow4 + 
  p_dotSize_slow5 + p_lm_emm_slow5 +
  plot_layout(guides = "collect", ncol  = 2) +
  plot_annotation(tag_levels = "A") &
  theme(text = element_text(size = 15), 
        axis.text =  element_text(size =15),
        title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        legend.title = element_text(size =13),
        plot.tag = element_text(size = 18, face = "bold"))
p_mod
ggsave(plot = p_mod, filename = "outputs/Fig6.png", width = 10, height = 10)
