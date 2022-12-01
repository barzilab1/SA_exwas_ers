# library(performance)
library(data.table)
library(sjPlot)
library(lme4)


#### help functions ####

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
  participants = length(levels(mod@frame[,c("pat_id")]))
  
  
  return(c(variable ,or, lowerci, upperci, p_value, coeffic, coeffic_ci, observationsN, participants))
  
  # tab_model(mod,show.intercept = F)  
}

run_mm <- function(variable, dataset ){
  formula_str = as.formula(paste0("bhssu04 ~ ", variable, " + age_at_screen + (age_at_screen)^2 + (age_at_screen)^3 + sex + (1 | pat_id)"))
  glmer(formula_str, family = binomial, data = dataset, nAGQ = 0)
}

run_glm <- function(variable, dataset){
  formula_str = as.formula(paste0("bhssu04 ~ ", variable, " + age_at_screen  + sex "))
  glm(formula_str,family = binomial, data = dataset)
}


run_models = function(variables, data_){
  
  models_list = lapply(variables, run_mm, data_ )
  results_to_print = as.data.frame(t(sapply(models_list, get_results)))
  colnames(results_to_print) = c("Feature" ,"OR", "Lowerci_or", "Upperci_or", "P_value","Coefficients","Coefficients_ci", "observationsN", "participants")
  # tab_model(models_list[[1]],show.intercept = F)
  
  return(list(models = models_list, 
              results = as.data.frame(results_to_print)))
}


#### run exwas ####
individual_matched = readRDS("exwas/data/matched_groups.rds")
dataset_individual = readRDS("exwas/data/dataset_individual_full.rds")

dataset_individual = merge(dataset_individual, individual_matched)

covariates = c("age_at_screen", "sex" )
IVs = grep("bhs(t|b|s[^u])", colnames(dataset_individual), value = T)
outcome = "bhssu04"


train = dataset_individual[group == 1,]
results = run_models(IVs, train)
tab_results = tab_model(results$models, show.intercept = F)

# results_glm = run_models(IVs)
# tab_results_glm = tab_model(models_list, show.intercept = F)

results$results$fdr = p.adjust(results$results$P_value, "fdr")


write.csv(results$results, "exwas/outputs/exwas_individual_results.csv", row.names = F)


