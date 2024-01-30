
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

######################## Figure 3 ######################################
show_col(pal_lancet("lanonc")(4)) # obtain the color for B, D
color <- c("#00468BFF","#42B540FF")

p_bar_retar <- dat_use %>% filter(group == "MDD") %>%
  ggplot(aes(x = HAMD_item8)) +
  geom_bar(fill = "steelblue") +
  xlab("Severity") + ylab("Count") + 
  ggtitle("Retardation score") +
  theme_classic()

p_bar_agit <- dat_use %>% filter(group == "MDD") %>%
  ggplot(aes(x = HAMD_item9)) +
  geom_bar(fill = "darkred") +
  xlab("Severity") + ylab("Count") + 
  ggtitle("Agitation score") +
  theme_classic()

p_box_scales <- dat_use %>% filter(group == "MDD") %>%
  select(participant_id, subtype_psycho, HAMD_total, HAMA_total) %>%
  pivot_longer(cols = 3:4, names_to = "scale", values_to = "score") %>%
  mutate(scale_new = ifelse(scale == "HAMD_total", "HAMD", "HAMA")) %>%
  ggplot(aes(x =scale_new, y = score, fill = subtype_psycho)) +
  geom_boxplot(position = position_dodge(.6), width = .5, alpha = .8) +
  geom_signif(annotations = c("***", "*", "***", "**"),
              textsize = 7, vjust = .5,
              y_position = c(39,42, 41,44),
              xmin = c(.8,.8,1.8,1.8),
              xmax = c(1,1.2,2,2.2),
              tip_length = 0) +
  scale_fill_manual(values = c("#00468BFF","#0099b4FF","#42B540FF")) +
  ylab("Score") + xlab("Clinical scale") +
  theme_classic() + 
  easy_add_legend_title("Subtype")


p_dot_anxAgit <- dat_use %>% filter(group == "MDD") %>%
  ggplot(aes(x = HAMD_item9, y = HAMA_total)) +
  geom_point(size = 3, alpha = .5, position = position_jitter(.02)) +
  geom_smooth(method = "lm") + 
  xlab("Agitation score") + ylab("HAMA score") +
  theme_classic()

p_final <- p_bar_agit + p_bar_retar + p_box_scales + p_dot_anxAgit +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(text = element_text(size = 15), 
        axis.title.x = element_text(size =10, face = "bold"),
        axis.text =  element_text(size =15),
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.title = element_text(size =13),
        plot.tag = element_text(size = 18, face = "bold"))

ggsave(plot = p_final, filename = "outputs/Figure3.png", width = 8, height = 7)
