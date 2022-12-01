library(readr)
library(data.table)


individual_matched = readRDS("exwas/data/matched_groups.rds")
dataset_individual = readRDS("exwas/data/dataset_individual_full.rds")
exwas_results = read.csv("exwas/outputs/exwas_individual_results.csv")
setDT(exwas_results)

# testing dataset
testing_dataset = merge(dataset_individual, individual_matched)
testing_dataset = testing_dataset[group == 2,]

# get exwas features
individual_cut_off = exwas_results[fdr <= 0.05, c("Feature", "Coefficients")]


#### calculate scores ####
testing_dataset[,exwas_individual_sum := apply(.SD, 1 , function(r){
  return(sum(r*individual_cut_off$Coefficients, na.rm = T))
}), .SDcols = individual_cut_off$Feature]


testing_dataset[ ,exwas_individual_sum_z := scale(exwas_individual_sum)[,1]]

#### sum score analysis ####
covariates = c("age_at_screen", "sex", "race_black" ,"race_white", "ethnicity_new" )
outcome = "bhssu04"


mod1 = glmer(paste0("bhssu04 ~ ", paste(covariates, collapse = " + ") , " + (1 | pat_id)"), family = binomial, data = testing_dataset, nAGQ = 0)
mod2 = glmer(paste0("bhssu04 ~ exwas_individual_sum_z + ", paste(covariates, collapse = " + ") , " + (1 | pat_id)"), family = binomial, data = testing_dataset, nAGQ = 0)

t1 = tab_model(mod1, mod2, show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T) 
t1

anova(mod1, mod2, test="Chisq")





