#################################
#
# This function is used to obtain
# the TD for intra-SMN and inter-SMN-DMN
#
# the input should be the time delay matrix
#

ObtainTDinSMN <- function(dat_TD, net_anna){
  
  dat_test <- rio::import(dat_TD) %>% as.matrix()
  sbj_name <- stringr::str_extract(dat_TD, pattern = "sub-[0-9]*")
  
  net_smn <- which(net_anna$network_new == "somMot")
  net_dmn <- which(net_anna$network_new == "DMN")
  dat_smn <- dat_test[net_smn,net_smn]
  dat_smndmn <- dat_test[net_dmn,net_smn]
  td_intraSMN <- mean(dat_smn[upper.tri(dat_smn)], na.rm = T)
  td_SMNDMN <- mean(dat_smndmn, na.rm = T)
  
  return(data.frame(participant_id = sbj_name, 
                    td_intraSMN = td_intraSMN, 
                    td_interSMN = td_SMNDMN))
}