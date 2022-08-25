library(lme4)


############ define functions ############
get_results <- function(mod, variable){
  
  fix_coef = summary(mod)$coefficients
  
  or = round(exp( fix_coef[variable, "Estimate"]), digits = 3)
  p_value = fix_coef[variable, "Pr(>|z|)"]
  
  ci = round(exp(confint(mod,method="Wald"))[variable,], digits = 3)
  ci = paste0(ci[1],"-",ci[2])
  
  observationsN = summary(mod)[["devcomp"]]$dims[["N"]]
  
  
  return(c(or, ci, p_value, observationsN))
  
  # tab_model(mod,show.intercept = F)  
}


run_models <- function(dataset_IV, dataset_DV, file_name){
  
  variables = grep("src|sex", colnames(dataset_IV), invert = T, value = T)
  dataset = merge(dataset_DV, dataset_IV)
  models_list = list()
  results_to_print = matrix(NA,nrow = length(variables), ncol = 4, dimnames = list(variables, c("OR", "CI", "P value", "observationsN")))
  
  for (variable in variables) {
   
    formula_str = as.formula(paste0("SA_y_ever", " ~", variable," + (1 | site_id_l_br/rel_family_id)"))
    
    mod = glmer(formula_str, family = binomial, data = dataset, nAGQ = 0)
    models_list[[variable]] = mod
    
    fix_coef = summary(mod)$coefficients
    if(fix_coef[variable,"Pr(>|z|)"] <= 0.05){
      results_to_print[variable,] =  get_results(mod,variable)
    }
    
  }
  
  results_to_print = results_to_print[rowSums(is.na(results_to_print)) != 4,]
  print(results_to_print)
  write.csv(file = paste0("outputs/", file_name, "_results.csv"), results_to_print)
  
  return(models_list)
}




############ run analysis ############
demographics <- read_csv("data/demographics_exposome_wide.csv")
exposome_set_item <- read_csv("data/exposome_set_item.csv")
exposome_set_sum <- read_csv("data/exposome_sum_set.csv")
family_history <- read_csv("data/family_history.csv")
geo_data <- read_csv("data/geo_data.csv")


suicide <- read_csv("data/suicide_wide.csv")


demo_models = run_models(demographics, suicide, "demographics")
family_models = run_models(family_history, suicide, "family_history")
exposome_sum_models = run_models(exposome_set_sum, suicide, "exposome_sum")
exposome_items_models = run_models(exposome_set_item, suicide, "exposome_items")
geo_data_models = run_models(geo_data, suicide, "geo_data")







