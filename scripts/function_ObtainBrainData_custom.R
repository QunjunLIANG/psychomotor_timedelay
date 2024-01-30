##########################################################
#
# This function is used to extradt the within-network
# time delay estimation of visual and somatomotor, 
# and the visual-somatomotor time delay
#
# Liang Qunjun  2023/10/10
# 
ObtainBrainData_custom <- function(sbj_use, td_list, weight_list, net_annotation) {
  library(tidyverse)
  library(purrr)

  # match the subject data in use
  file_use <- td_list[grep(paste(sbj_use, collapse = "|"), td_list)]
  weight_use <- weight_list[grep(paste(sbj_use, collapse = "|"), weight_list)]
  
  # define the id of columns to somatomotor network
  somMot_pos <- which(net_annotation$network_new == "somMot")
  visual_pos <- which(net_annotation$network_new == "visual")

  dat_td <- data.frame()
  for (file in 1:length(file_use)) {
    sbj_tmp <- str_extract(file_use[file], pattern = 'sub-[0-9]*')
    td_raw_tmp <- readr::read_csv(file_use[file], col_names = F, na = "NAN",show_col_types = FALSE)
    weight_tmp <- readr::read_csv(weight_use[file], col_names = F, na = "NAN",show_col_types = FALSE)
    
    # weighted the delay matrix
    td_tmp <- td_raw_tmp*weight_tmp
    
    # extract within-network TD in visual network
    td_visual <- td_tmp[visual_pos, visual_pos] %>% as.matrix() %>% colMeans(na.rm = T)
    td_visual_weight <- weight_tmp[visual_pos, visual_pos] %>% as.matrix() %>% 
      colMeans(na.rm = T)
    td_visual <- mean((td_visual/td_visual_weight),na.rm = T)
    
    # extract within-network TD in somMot network
    td_somMot <- td_tmp[somMot_pos, somMot_pos] %>% as.matrix() %>% colMeans(na.rm = T)
    td_somMot_weight <- weight_tmp[somMot_pos, somMot_pos] %>% as.matrix() %>% colMeans(na.rm = T)
    td_somMot <- mean((td_somMot/td_somMot_weight), na.rm = T)
    
    # extract out-network TD in visual network
    td_visual_out <- td_tmp[visual_pos, -c(visual_pos)] %>% as.matrix() %>% colMeans(na.rm = T)
    td_visual_weight_out <- weight_tmp[visual_pos, -c(visual_pos)] %>% as.matrix() %>% 
      colMeans(na.rm = T)
    td_visual_out <- mean((td_visual_out/td_visual_weight_out),na.rm = T)
    
    # extract out-network TD in somMot network
    td_somMot_out <- td_tmp[somMot_pos, -c(somMot_pos)] %>% as.matrix() %>% colMeans(na.rm = T)
    td_somMot_weight_out <- weight_tmp[somMot_pos, -c(somMot_pos)] %>% as.matrix() %>% 
      colMeans(na.rm = T)
    td_somMot_out <- mean((td_somMot_out/td_somMot_weight_out),na.rm = T)
    # extract out-network TD in somMot network
    td_visual_out <- td_tmp[visual_pos, -c(visual_pos)] %>% as.matrix() %>% colMeans(na.rm = T)
    td_visual_weight_out <- weight_tmp[visual_pos, -c(visual_pos)] %>% as.matrix() %>% 
      colMeans(na.rm = T)
    td_visual_out <- mean((td_visual_out/td_visual_weight_out),na.rm = T)

    # extract between network TD
    td_between <- td_tmp[somMot_pos, visual_pos] %>% as.matrix() %>% colMeans(na.rm = T)
    td_between_weight <-weight_tmp[somMot_pos, visual_pos] %>% as.matrix() %>% colMeans(na.rm = T)
    td_between <- mean((td_between/td_between_weight), na.rm = T) 

    dat_tmp <- data.frame(participant_id = sbj_tmp, 
                          somMot_in = td_somMot, visual_in = td_visual,
                          somMot_out = td_somMot_out, visual_out = td_visual_out,
                          somMot_vis = td_between)
    dat_td <- rbind(dat_td, dat_tmp)
  }

  return(dat_td)
}

# ObtainBrainData_custom <- function(sbj_use, file_list, net_annotation) {
#   library(tidyverse)
#   library(purrr)
# 
#   load_data <- function(file, somMot_pos, visual_pos){
#     sbj_tmp <- str_extract(file, pattern = 'sub-[0-9]*')
#     td_tmp <- readr::read_csv(file, col_names = F, na = "NAN",show_col_types = FALSE)
#     # extract TD in visual network
#     td_visual <- td_tmp[visual_pos, visual_pos]
#     td_visual <- mean(td_visual[lower.tri(td_visual)], na.rm = T)
#     # extract TD in somatomotor network
#     td_somMot <- td_tmp[somMot_pos, somMot_pos]
#     td_somMot <- mean(td_somMot[lower.tri(td_somMot)], na.rm = T)
#     # extract between network TD
#     td_between <- td_tmp[somMot_pos, visual_pos] %>% as.matrix() %>% mean(na.rm = T)
#     dat_tmp <- data.frame(participant_id = sbj_tmp, somMot = td_somMot, visual = td_visual,
#                           somMot_vis = td_between)
#   }
# 
#   # match the subject data in use
#   file_use <- data.frame(file_list[grep(paste(sbj_use, collapse = "|"), file_list)])
#   dat_file <- data.frame(file = file_use)
# 
#   # define the id of columns to somatomotor network
#   somMot <- which(net_annotation$network_new == "somMot")
#   visual <- which(net_annotation$network_new == "visual")
#   dat_td <- map2(file_use, somMot, visual, load_data, .progress = T) %>% list_rbind()
# 
#   return(dat_td)
# }