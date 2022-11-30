library(lme4)
library(readr)


#### define functions #### 
get_results <- function(mod){
  
  fix_coef = summary(mod)$coefficients
  
  variable = rownames(fix_coef)[2]
  
  coeffic = fix_coef[variable, "Estimate"]
  or = round(exp( fix_coef[variable, "Estimate"]), digits = 3)
  p_value = fix_coef[variable, "Pr(>|z|)"]
  
  coeffic_ci = round(confint(mod,method="Wald")[variable,], digits = 3)
  coeffic_ci = paste0(coeffic_ci[1],"-",coeffic_ci[2])
   
  ci = round(exp(confint(mod,method="Wald"))[variable,], digits = 3)
  lowerci = ci[1]
  upperci = ci[2]
  
  observationsN = summary(mod)[["devcomp"]]$dims[["N"]]
  participants = length(levels(mod@frame[,c("src_subject_id")]))
  
  
  return(c(variable ,or, lowerci, upperci, p_value, coeffic, coeffic_ci, observationsN, participants))
  
  # tab_model(mod,show.intercept = F)  
}


run_mm <- function(variable, dataset ){
  formula_str = as.formula(paste0("SA_y ~ ", variable, " + interview_age + (interview_age)^2 + (interview_age)^3 + sex + (1 | site_id_l_br/rel_family_id/src_subject_id)"))
  glmer(formula_str, family = binomial, data = dataset, nAGQ = 0)
}
 

run_models <- function(dataset_IV, dataset_DV){ #, file_name
  
  variables = grep("src|^sex|interview|event", colnames(dataset_IV), invert = T, value = T)
  dataset = merge(dataset_DV, dataset_IV)
  
  models_list = lapply(variables, run_mm, dataset )
  results_to_print = as.data.frame(t(sapply(models_list, get_results)))
  colnames(results_to_print) = c("Feature" ,"OR", "Lowerci_or", "Upperci_or", "P_value","Coefficients","Coefficients_ci", "observationsN", "participants")
  # tab_model(models_list[[1]],show.intercept = F)
  
  # in case the variable was dropped by the mix model
  # results_to_print = results_to_print[results_to_print$Feature != "interview_age", ]
  # print(paste0("vari not in mm: ", paste(setdiff(variables, results_to_print$Feature), collapse = " | ")))
  
  # write.csv(file = paste0("outputs/", file_name, "_results.csv"), results_to_print, row.names = F)
  
  return(list(models = models_list, 
              results = as.data.frame(results_to_print)))
}


#### read data #### 
individual_level <- read_csv("data/individual_level_train.csv")
structural_level <- read_csv("data/structural_level_train.csv")

suicide_train <- read_csv("data/DV_suicide_train.csv")



#### run Exwas #### 
individual_level_models = run_models(individual_level, suicide_train) #, "individual_level")
structural_level_models = run_models(structural_level, suicide_train)# , "structural_level")


#### add FDR correction ###
individual_level_models$results$fdr = p.adjust(individual_level_models$results$P_value, "fdr")
structural_level_models$results$fdr = p.adjust(structural_level_models$results$P_value, "fdr")


temp = individual_level_models$results[order(as.numeric(individual_level_models$results$P_value) , decreasing = F),]
rownames(temp) = NULL
temp$order = as.numeric(rownames(temp))
temp$fdr_br = as.numeric(temp$P_value)*nrow(temp)/temp$order

individual_level_models$results$fdr = temp$fdr_br


temp = structural_level_models$results[order(as.numeric(structural_level_models$results$P_value) , decreasing = F),]
rownames(temp) = NULL
temp$order = as.numeric(rownames(temp))
temp$fdr_br = as.numeric(temp$P_value)*nrow(temp)/temp$order

structural_level_models$results$fdr = temp$fdr_br



write.csv(file = paste0("outputs/individual_level_results.csv"), individual_level_models$results, row.names = F)
write.csv(file = paste0("outputs/structural_level_results.csv"), structural_level_models$results, row.names = F)




individual_level_models$results$bonferroni = p.adjust(individual_level_models$results$P_value, "bonferroni")
structural_level_models$results$bonferroni = p.adjust(structural_level_models$results$P_value, "bonferroni")


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


