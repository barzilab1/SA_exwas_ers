library(lme4)
library(readr)


#### define functions #### 
get_results <- function(mod, variable){
  
  result  = list()
  
  result$variable = variable
  
  if(is.null(mod)){
    return(list("variable" = variable, "coeffic" = NA,  "or"= NA, "lowerci"= NA, "upperci"= NA, 
                "p_value"= NA, "coeffic_ci"= NA, "observationsN"= NA, "participants"= NA))
  }
  
  fix_coef = summary(mod)$coefficients
  
  result$coefficients = fix_coef[variable, "Estimate"]
  
  coeffic_ci = round(confint(mod, method="Wald")[variable,], digits = 3)
  result$coeffic_ci = paste0(coeffic_ci[1],"-",coeffic_ci[2])
   
  result$or = round(exp( fix_coef[variable, "Estimate"]), digits = 3)
  ci = round(exp(confint(mod,method="Wald"))[variable,], digits = 3)
  result$lowerci = ci[[1]]
  result$upperci = ci[[2]]
  
  result$p_value = fix_coef[variable, "Pr(>|z|)"]
  
  result$observationsN = summary(mod)[["devcomp"]]$dims[["N"]]
  result$participants = length(levels(mod@frame[,c("src_subject_id")]))
  
  return(result)
  
  
  # library(sjPlot)
  # tab_model(mod,show.intercept = F)  
}


run_mm <- function(variable, df ){
  formula_str = as.formula(paste0("SA_y ~ ", variable, " + interview_age + (interview_age)^2 + (interview_age)^3 + sex + (1 | site_id_l_br/rel_family_id/src_subject_id)"))
  glmer(formula_str, family = binomial, data = df, nAGQ = 0)
}
 

run_models <- function(dataset_IV, dataset_DV){
  
  variables = grep("src|^sex|interview|event", colnames(dataset_IV), invert = T, value = T)
  df = merge(dataset_DV, dataset_IV)
  
  models_list = sapply(variables, run_mm, df)
  results_to_print = as.data.frame(t(sapply(names(models_list), \(variable) unlist(get_results(models_list[[variable]], variable)))))
  # library(sjPlot);tab_model(models_list, show.intercept = F)
  
  return(list(models = models_list, 
              results = results_to_print))
}


#### read data #### 
exposome_df <- read_csv("data/exposome_df_train.csv")
suicide_train_df <- read_csv("data/DV_suicide_train.csv")

#### run Exwas #### 
exposome_models = run_models(exposome_df, suicide_train_df) 

#### add FDR correction ####
exposome_models$results$fdr = p.adjust(exposome_models$results$p_value, "fdr")

write.csv(file = paste0("outputs/exwas_results.csv"), exposome_models$results, row.names = F)









#### run one model that contains all selected features ####
###!!!!!!!! we can't run all in one models. we have variables available in different timepoints so all rows have NA -> no data
# individual_cut_off = exposome_models$results$variable[exposome_models$results$fdr <= 0.05]
# # structural_cut_off = structural_level_models5$results[structural_level_models5$results$fdr <= 0.05, "Feature"]
# 
# dataset_in = merge(exposome_df, suicide_train_df)
# # dataset_st = merge(structural_level_5, suicide_train)
# 
# formula_str = as.formula(paste0("SA_y ~ ", paste(individual_cut_off,collapse = " + "), " + interview_age + (interview_age)^2 + (interview_age)^3 + sex + (1 | site_id_l_br/rel_family_id/src_subject_id)"))
# individula_mod_all_2 = glmer(formula_str, family = binomial, data = dataset_in, nAGQ = 0)
# 
# library(sjPlot)
# tab_in2 = tab_model(individula_mod_all_2,show.intercept = F) 
# tab_in2






