library(data.table)
library(readr)
library(sjPlot)
library(lme4)

setwd("~/sa_exwas")

#### help functions ####

get_results <- function(mod, variable){
  
  result  = list()
  
  result$variable = variable
  
  if(is.null(mod)){
    return(list("variable" = variable, "coeffic" = NA,  "or"= NA, "lowerci"= NA, "upperci"= NA, 
                "p_value"= NA, "coeffic_ci"= NA, "observationsN"= NA, "participants"= NA))
  }
  
  fix_coef = summary(mod)$coefficients
  
  result$coefficients = fix_coef[variable, "Estimate"]
  result$or = round(exp( fix_coef[variable, "Estimate"]), digits = 3)
  result$p_value = fix_coef[variable, "Pr(>|z|)"]
  
  coeffic_ci = round(confint(mod,method="Wald")[variable,], digits = 3)
  result$coeffic_ci = paste0(coeffic_ci[1],"-",coeffic_ci[2])
  
  ci = round(exp(confint(mod,method="Wald"))[variable,], digits = 3)
  result$lowerci = ci[[1]]
  result$upperci = ci[[2]]
  
  result$observationsN = nrow(mod[["model"]])
  
  return(result)
  
  # tab_model(mod,show.intercept = F)  
}


run_glm <- function(variable, df){
  formula_str = as.formula(paste0("bhssu04 ~ ", variable, " +",paste(covariates,collapse = " + ") ))
  glm(formula_str,family = binomial, data = df)
}


run_models = function(variables, df){
  
  models_list = sapply(variables, run_glm, df, simplify = F)
  results_to_print = as.data.frame(t(sapply(names(models_list), \(variable) unlist(get_results(models_list[[variable]], variable)))))
  # tab_model(models_list[[1]],show.intercept = F)
  
  return(list(models = models_list, 
              results = results_to_print))
  
}

###################    
#### run exwas ####
###################    

train_df = read_csv("data/train_df.csv")

covariates = c("age_at_screen", "sex" )
IVs = grep("bhs(?!su04)|medi", colnames(train_df), ignore.case = T, perl = T, value = T)
outcome = "bhssu04"


results = run_models(IVs, train_df)
tab_model(results$models, show.intercept = F, file = "outputs/exwas_results_models.xls" )

results$results$fdr = p.adjust(results$results$p_value, "fdr")
write.csv(results$results, "outputs/exwas_results.csv", row.names = F)




