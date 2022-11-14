library(readr)

source("config.R")

#### read data #### 
individual_level <- read_csv("data/individual_level.csv")
structural_level <- read_csv("data/structural_level.csv")

individual_level_results <- read_csv("outputs/individual_level_results.csv")
structural_level_results <- read_csv("outputs/structural_level_results.csv")


#### p value correction ####
individual_level_results$fdr = p.adjust(individual_level_results$P_value, "fdr")
structural_level_results$fdr = p.adjust(structural_level_results$P_value, "fdr")

individual_cut_off = individual_level_results[individual_level_results$fdr <= 0.05, c("Feature", "Coefficients")]
structural_cut_off = structural_level_results[structural_level_results$fdr <= 0.05, c("Feature", "Coefficients")]

write.csv(file = paste0("data/individual_level_results_fdr.csv"), individual_level_results, row.names = F)
write.csv(file = paste0("data/structural_level_results_fdr.csv"), structural_level_results, row.names = F)

# individual_level_results$bonferroni = p.adjust(individual_level_results$P_value, "bonferroni")
# structural_level_results$bonferroni = p.adjust(structural_level_results$P_value, "bonferroni")


#### create exposome weights ####
#' cut_off_levels = c(.1,.05,.01)
#' 
#' #' 1. check for each variable if it below the cut off
#' individual_cut_off_variables = list(fdr = list(), bonferroni = list()) 
#' structural_cut_off_variables = list(fdr = list(), bonferroni = list()) 
#' 
#' for(p in cut_off_levels){
#'   individual_cut_off_variables$fdr[[as.character(p) ]] = individual_level_results$feature[individual_level_results$fdr <= p]
#'   individual_cut_off_variables$bonferroni[[as.character(p) ]] = individual_level_results$feature[individual_level_results$bonferroni <= p]
#'   
#'   structural_cut_off_variables$fdr[[as.character(p) ]] = structural_level_results$feature[structural_level_results$fdr <= p]
#'   structural_cut_off_variables$bonferroni[[as.character(p) ]] = structural_level_results$feature[structural_level_results$bonferroni <= p]
#' }


### calculate scores ###
individual_level$exwas_individual_sum = apply(individual_level[,individual_cut_off$Feature], 1 , function(r){
  return(sum(r*individual_cut_off$Coefficients, na.rm = T))
})

structural_level$exwas_structural_sum = apply(structural_level[,structural_cut_off$Feature], 1 , function(r){
  return(sum(r*structural_cut_off$Coefficients, na.rm = T))
})


### create dataset for exposome sum score
suicide_test <- read_csv("data/DV_suicide_test.csv")
race <- read.csv("data/demo_race.csv")
FH_suicide <- read_csv("data/family_history.csv")
genetics <- read_csv(file.path(abcd_genetics_path, "genetic.csv"))

dataset = merge(suicide_test, race, all.x = T)
dataset = merge(dataset, FH_suicide[,c("src_subject_id", "famhx_ss_momdad_scd_p")], all.x = T)
dataset = merge(dataset, genetics[,c("src_subject_id", "suicide_PRSice_Pt0_05", "genetic_afr")], all.x = T)
dataset = merge(dataset, individual_level[,c("src_subject_id", "eventname", "interview_age", "sex", "exwas_individual_sum")] )
dataset = merge(dataset, structural_level[,c("src_subject_id", "eventname", "interview_age", "sex", "exwas_structural_sum")] )

write.csv(file = paste0("data/dataset_ESS.csv"), dataset, row.names = F)




