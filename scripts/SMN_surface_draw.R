library(rio)
library(tidyverse)
library(ggridges)
library(plotly)
library(RColorBrewer)
library(ggsci)
library(ggeasy)
library(patchwork)
library(bruceR)
library(ggstatsplot)
library(ggseg)
library(ggsegSchaefer)
# load the function for surface plot
source("scripts/function_DrawSurfPlot.R")

############################## SMN global ######################################

# load the network identification
net_id <- read_csv('inputs/Yeo_net_identity_7.csv', col_names = 'network_ind') 
net_parcel <- read_csv('inputs/Yeo_7net_roiAnnotation.csv', col_names = 'region')
net_ana <- cbind(net_id, net_parcel)
net_ana$network_ind <- factor(net_ana$network_ind)
net_ana['parcel'] <- paste0('parcel', 1:400)
net_ana <- net_ana %>% mutate(network = ifelse(str_detect(region, pattern = 'Vis'),'VIS',
                                               ifelse(str_detect(region, pattern = 'SomMot'),'SMN',
                                                      ifelse(str_detect(region, pattern = 'DorsAttn'),'DAN',
                                                             ifelse(str_detect(region, pattern = 'SalVentAttn'),'SAN',
                                                                    ifelse(str_detect(region, pattern = 'Limbic'),'LIM',
                                                                           ifelse(str_detect(region, pattern = 'Cont'),'FPN',
                                                                                  'DMN')))))))


## Load in atlas data provided by ggseg package
atlas      = as_tibble(schaefer7_400)

## Select atlas region names and hemisphere so that we can add the values
## we want to plot:
region     = atlas$region
hemi       = atlas$hemi
data       = distinct(na.omit(data.frame(region,hemi))) #remove NA and duplicate regions
data_Yeo7 <- data %>% left_join(net_ana)
atlas_temp = left_join(atlas,data_Yeo7)
## network color 
net_color <- c("#a153a2","#6fabd2","#2c8b4b","#b77fb4","#e7edca","#edaf5e","#e27283")

atlas_data = atlas_temp
atlas_data$network[which(is.na(atlas_data$network))] <- "Medial Wall"
atlas_data$network <- factor(atlas_data$network, levels = c("VIS","SMN","DAN","SAN","LIM","FPN","DMN",'Medial Wall'))

p_yeo7 <- ggplot() + geom_brain(
  atlas       = atlas_data,
  mapping     = aes(fill=network),
  position    = position_brain(hemi~ side),
  color       ='black',
  size        = 0.3,
  show.legend = T) +
  ggtitle('Yeo 7 network template') +
  scale_fill_manual(values = c("#edaf5e","#2c8b4b","#edaf5e","#edaf5e","#edaf5e","#edaf5e","#edaf5e",'darkgrey')) +
  easy_all_text_size(13) +
  theme_void() +# easy_legend_at('bottom') +
  easy_center_title() + easy_add_legend_title('Network')
p_yeo7
ggsave(filename = "outputs/SMN_td_example.png", width = 6, height = 4)

############################## SMN local ######################################

dat_stim <- rnorm(400)

library(ggseg)
library(ggsegSchaefer)
library(ggeasy)

## Load in atlas data provided by ggseg package
atlas      = as_tibble(schaefer7_400)

## Select atlas region names and hemisphere so that we can add the values
## we want to plot:
region     = atlas$region
hemi       = atlas$hemi
data       = distinct(na.omit(data.frame(region,hemi))) #remove NA and duplicate regions
dat_stim[-which(str_detect(data$region, pattern = "SomMot"))] <- 0
## add the value to each region
data['value'] <- dat_stim

atlas_data <- atlas %>% left_join(data) # add projection map to the atlas

p_this <- ggplot() + geom_brain(
  atlas       = atlas_data,
  mapping     = aes(fill=value),
  position    = position_brain("horizontal"),
  color       ='black',
  size        = .3,
  show.legend = T) +
  ggtitle("local SMN") +
  scale_fill_gradient2(low = "steelblue", high = "darkred", mid = "white") +
  theme_void() + easy_move_legend(to = "bottom") +
  easy_center_title() + easy_add_legend_title("time delay") +
  easy_text_size(which = c("plot.title", "legend.title", "strip.text"),
                 size = 15)
p_this
ggsave(filename = "outputs/SMN_td_local_example.png", width = 6, height = 3)
