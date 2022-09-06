library(lme4)
library(readr)


############ define functions ############
get_results <- function(mod, variable){
  
  fix_coef = summary(mod)$coefficients
  
  or = round(exp( fix_coef[variable, "Estimate"]), digits = 3)
  p_value = fix_coef[variable, "Pr(>|z|)"]
  
  ci = round(exp(confint(mod,method="Wald"))[variable,], digits = 3)
  ci = paste0(ci[1],"-",ci[2])
  
  observationsN = ifelse(is.null(summary(mod)[["devcomp"]]),
                     length(summary(mod)[["deviance.resid"]]), 
                     summary(mod)[["devcomp"]]$dims[["N"]])
  
  
  return(c(or, ci, p_value, observationsN))
  
  # tab_model(mod,show.intercept = F)  
}



run_mm <- function(variable, dataset ){
  formula_str = as.formula(paste0("SA_y_ever", " ~", variable," + (1 | site_id_l_br/rel_family_id)"))
  glmer(formula_str, family = binomial, data = dataset, nAGQ = 0)
}



run_glm <- function(variable, dataset ){
  formula_str = as.formula(paste0("SA_y_ever", " ~", variable))
  glm(formula_str, family = binomial, data = dataset)
}



run_models <- function(dataset_IV, dataset_DV, file_name, model_to_run = run_mm ){
  
  variables = grep("src|sex", colnames(dataset_IV), invert = T, value = T)
  dataset = merge(dataset_DV, dataset_IV)
  
  col_with_sd = sapply(dataset[,!grepl("src|sex", colnames(dataset)), drop = F], function(x){sd(x, na.rm = T)> 0})
  col_with_sd = names(which(col_with_sd))
  print(paste0("cols removed: ", paste(setdiff(colnames(dataset), col_with_sd ), collapse = " | " ) ))
  dataset = dataset[, c("src_subject_id", "sex", col_with_sd) ]
  variables = intersect(variables, col_with_sd)
  
  models_list = list()
  results_to_print = matrix(NA,nrow = length(variables), ncol = 4, dimnames = list(variables, c("OR", "CI", "P value", "observationsN")))
  
  for (variable in variables) {
   
    mod = model_to_run(variable, dataset)
    
    fix_coef = summary(mod)$coefficients
    if(fix_coef[variable,"Pr(>|z|)"] <= 0.05){
      results_to_print[variable,] =  get_results(mod,variable)
      models_list[[variable]] = mod
    }
    
  }
  
  results_to_print = results_to_print[rowSums(is.na(results_to_print)) != 4,]
  # print(results_to_print)
  write.csv(file = paste0("outputs/", file_name, "_results.csv"), results_to_print)
  
  return(models_list)
}




############ run analysis ############
demographics <- read_csv("data/demographics_exposome_wide.csv")
exposome_set_item <- read_csv("data/exposome_set_item.csv")
exposome_set_sum <- read_csv("data/exposome_sum_set.csv")
substance_set <- read_csv("data/substance.csv")
family_history <- read_csv("data/family_history.csv")
geo_data <- read_csv("data/geo_data.csv")


suicide <- read_csv("data/suicide_wide.csv")


demo_models = run_models(demographics, suicide, "demographics")
family_models = run_models(family_history, suicide, "family_history")
exposome_sum_models = run_models(exposome_set_sum, suicide, "exposome_sum")
exposome_items_models = run_models(exposome_set_item, suicide, "exposome_items")
geo_data_models = run_models(geo_data, suicide, "geo_data")
substance_models = run_models(substance_set, suicide, "substance_data")


# demo_models = run_models(demographics, suicide, "demographics_glm", run_glm)
# family_models = run_models(family_history, suicide, "family_history_glm", run_glm)
# exposome_sum_models = run_models(exposome_set_sum, suicide, "exposome_sum_glm", run_glm)
# exposome_items_models = run_models(exposome_set_item, suicide, "exposome_items_glm", run_glm)
# geo_data_models = run_models(geo_data, suicide, "geo_data_glm", run_glm)
# substance_models = run_models(substance_set, suicide, "substance_data_glm", run_glm)


library(qgraph)

all_datasets = merge(demographics,family_history)
all_datasets = merge(all_datasets,exposome_set_sum)
all_datasets = merge(all_datasets,exposome_set_item)
all_datasets = merge(all_datasets,substance_set)
# all_datasets = merge(all_datasets,geo_data)
corrs = cor_auto(all_datasets[,grep("src|sex", colnames(all_datasets), invert = T)])



