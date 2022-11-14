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
  ci = paste0(ci[1],"-",ci[2])
  
  observationsN = summary(mod)[["devcomp"]]$dims[["N"]]
  participants = length(levels(mod@frame[,c("src_subject_id")]))
  
  
  return(c(variable ,or, ci, p_value, coeffic, coeffic_ci, observationsN, participants))
  
  # tab_model(mod,show.intercept = F)  
}


run_mm <- function(variable, dataset ){
  formula_str = as.formula(paste0("SA_y ~ ", variable, " + interview_age + (interview_age)^2 + (interview_age)^3 + sex + (1 | site_id_l_br/rel_family_id/src_subject_id)"))
  glmer(formula_str, family = binomial, data = dataset, nAGQ = 0)
}
 

run_models <- function(dataset_IV, dataset_DV, file_name){
  
  variables = grep("src|^sex|interview|event", colnames(dataset_IV), invert = T, value = T)
  dataset = merge(dataset_DV, dataset_IV)
  
  models_list = lapply(variables, run_mm, dataset )
  results_to_print = as.data.frame(t(sapply(models_list, get_results)))
  colnames(results_to_print) = c("Feature" ,"OR", "CI_OR", "P_value","Coefficients","Coefficients_ci", "observationsN", "participants")
  # tab_model(models_list[[1]],show.intercept = F)
  
  # in case the variable was dropped by the mix model
  # results_to_print = results_to_print[results_to_print$Feature != "interview_age", ]
  # print(paste0("vari not in mm: ", paste(setdiff(variables, results_to_print$Feature), collapse = " | ")))
  
  write.csv(file = paste0("outputs/", file_name, "_results.csv"), results_to_print, row.names = F)
  
  return(models_list)
}


#### read data #### 
individual_level <- read_csv("data/individual_level_training.csv")
structural_level <- read_csv("data/structural_level_training.csv")

suicide_train <- read_csv("data/DV_suicide_train.csv")



#### run Exwas #### 
individual_level_models = run_models(individual_level, suicide_train, "individual_level")
structural_level_models = run_models(structural_level, suicide_train, "structural_level")

