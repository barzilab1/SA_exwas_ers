library(lme4)
library(readr)
library(sjPlot)
library(pROC)


get_marginal_AUC <- function(mod){
  
  model_data = model.frame(mod)
  fixed_effects = fixef(mod)[-1]
  model_data$y_predicted <- rowSums(sapply(names(fixed_effects), \(fixed_IV) model_data[[fixed_IV]] * fixed_effects[fixed_IV]))
  roc_object <- roc( model_data$SA_y, model_data$y_predicted)
  print(round(auc(roc_object), digits = 4))
  return(roc_object)
  
}

########################
#### 1. main models ####
########################
dataset <- read_csv("data/dataset_ERS.csv") 
dataset_fh_complete = dataset[complete.cases(dataset$famhx_ss_momdad_scd_p),]

basic_covs = c("interview_age", "(interview_age)^2","(interview_age)^3","sex_br")
covs = c( basic_covs, "race_black", "race_white" , "ethnicity_hisp" )
cov_fh = c(covs, "famhx_ss_momdad_scd_p")
random_exp = "(1 | site_id_l_br/rel_family_id/src_subject_id)"


# Table 2
mod1 = glmer(paste0("SA_y ~ ", paste(covs,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2 = glmer(paste0("SA_y ~ ers_z + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod1_com_fh = glmer(paste0("SA_y ~ ", paste(covs,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)
mod2_com_fh = glmer(paste0("SA_y ~ ers_z + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)
mod3 = glmer(paste0("SA_y ~ ", paste(cov_fh,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)
mod4 = glmer(paste0("SA_y ~ ers_z + ", paste(cov_fh, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)

tab_model(mod1, mod2, mod3, mod4, 
          show.intercept = F, show.ngroups = T,  show.r2 = T,  file = "outputs/abcd_ers_main_results.xls")
tab_model(mod1, mod2, mod1_com_fh, mod2_com_fh,  mod3, mod4,
          show.intercept = F, show.ngroups = T,  show.r2 = T)

anova(mod1, mod2, test="Chisq")$'Pr(>Chisq)'[2]
anova(mod3, mod4, test="Chisq")$'Pr(>Chisq)'[2]
anova(mod1_com_fh, mod3, test="Chisq") 
anova(mod2_com_fh, mod4, test="Chisq") 

p.adjust(c(1.347805e-42,1.650847e-38, 3.178e-07 ,0.0001684), "fdr")

roc1 = get_marginal_AUC(mod1)
roc2 = get_marginal_AUC(mod2)
roc.test(roc1, roc2)


###############################
#### 2. supplements models ####
###############################

# 1. supp table 5 - single risk exposure
# only 2 year FU
Top5_2_year = c("ers_z", "peq_threat_vic_b", "cybb_phenx_harm", "peq_chase_vic_b")
random_exp_2 = "(1 | site_id_l_br/rel_family_id)"
models_list_2y = list()
for(exposure in Top5_2_year){
  models_list_2y = append(models_list_2y, glmer(paste0("SA_y ~ ", exposure, " + ", paste(covs, collapse = " + ") , " + " , random_exp_2), family = binomial, data = subset(dataset, eventname == "2_year_follow_up_y_arm_1"), nAGQ = 0))
}

tab_model(models_list_2y,
          show.intercept = F, digits.p = 4,  show.r2 = T, file = "outputs/top5_risk_protective_2year.xls")


# only 1 and 2 year FU
Top5_1_2_year = c("ers_z", "ple_victim_y", "dim_yesno_q3")
models_list_1_2y = list()
for(exposure in Top5_1_2_year){
  models_list_1_2y = append(models_list_1_2y, glmer(paste0("SA_y ~ ", exposure, " + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = subset(dataset, eventname != "baseline_year_1_arm_1"), nAGQ = 0))
}

tab_model(models_list_1_2y,
          show.intercept = F, digits.p = 4,  show.r2 = T, file = "outputs/top5_risk_protective_1_2year.xls")


# 2. supp table 8 - add interaction with minority  
mod1_int = glmer(paste0("SA_y ~ ers_z * race_black + ", paste(covs,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_int = glmer(paste0("SA_y ~ ers_z * ethnicity_hisp + ", paste(covs,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)

mod3_int = glmer(paste0("SA_y ~ ers_z + LGBT_inclusive + ", paste(covs,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod4_int = glmer(paste0("SA_y ~ ers_z * LGBT_inclusive + ", paste(covs,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)

tab_model(mod1_int, mod2_int, mod3_int, mod4_int, 
          show.intercept = F, show.ngroups = T,  show.r2 = T,  file = paste0("outputs/abcd_interaction.xls"))


# 3. supp table 9 - Stratified analyses attempt across subpopulations
for (lgbt in c(0,1)) {
  mod1_lgbt = glmer(paste0("SA_y ~ ", paste(covs,collapse = " + ") , " + " , random_exp), family = binomial, data = subset(dataset, LGBT_inclusive == lgbt), nAGQ = 0)
  mod2_lgbt = glmer(paste0("SA_y ~ ers_z + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = subset(dataset, LGBT_inclusive == lgbt), nAGQ = 0)
  
  print(tab_model(mod1_lgbt, mod2_lgbt, 
                  show.intercept = F, show.ngroups = T,  show.r2 = T,  file = paste0("outputs/abcd_ers_lgbt",lgbt,".xls")))
}


races = unique(na.omit(dataset$race_eth))
for (race in races) {
  mod1r = glmer(paste0("SA_y ~ ", paste(basic_covs,collapse = " + ") , " + " , random_exp), family = binomial, data = subset(dataset, race_eth == race ), nAGQ = 0)
  mod2r = glmer(paste0("SA_y ~ ers_z + ", paste(basic_covs, collapse = " + ") , " + " , random_exp), family = binomial, data = subset(dataset, race_eth == race ), nAGQ = 0)
  
  print(tab_model(mod1r, mod2r, 
                  show.intercept = F, show.ngroups = T,  show.r2 = T,  file = paste0("outputs/abcd_ers_",race,".xls")))
}



# supp table 12 - add cbcl
mod1_cbc = glmer(paste0("SA_y ~ cbcl_scr_syn_totprob_t + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_cbc = glmer(paste0("SA_y ~ ers_z + cbcl_scr_syn_totprob_t + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod1_cbc_com_fh = glmer(paste0("SA_y ~ cbcl_scr_syn_totprob_t + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)
mod2_cbc_com_fh = glmer(paste0("SA_y ~ ers_z + cbcl_scr_syn_totprob_t + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)
mod3_cbc = glmer(paste0("SA_y ~ cbcl_scr_syn_totprob_t + ", paste(cov_fh, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)
mod4_cbc = glmer(paste0("SA_y ~ ers_z + cbcl_scr_syn_totprob_t + ", paste(cov_fh, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)

tab_model( mod1_cbc, mod2_cbc, mod3_cbc, mod4_cbc, 
          show.intercept = F, digits.p = 4, show.r2 = T,  file = "outputs/abcd_ers_supp_6.xls")


anova(mod1_cbc, mod2_cbc, test="Chisq")$`Pr(>Chisq)`[2]
anova(mod3_cbc, mod4_cbc, test="Chisq")$`Pr(>Chisq)`[2]
anova(mod1_cbc_com_fh, mod3_cbc, test="Chisq") 
anova(mod2_cbc_com_fh, mod4_cbc, test="Chisq")

p.adjust(c(1.401908e-25,6.25682e-24, 0.001537 ,0.009891), "fdr")


# supp table 13 - use ers_sensitivity
mod1_sens = glmer(paste0("SA_y ~ ", paste(covs,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_sens = glmer(paste0("SA_y ~ ers_sensitivity_z + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod1_sens_com_fh = glmer(paste0("SA_y ~ ", paste(covs,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)
mod2_sens_com_fh = glmer(paste0("SA_y ~ ers_sensitivity_z + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)
mod3_sens = glmer(paste0("SA_y ~ ", paste(cov_fh,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)
mod4_sens = glmer(paste0("SA_y ~ ers_sensitivity_z + ", paste(cov_fh, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset_fh_complete, nAGQ = 0)

tab_model(mod1_sens, mod2_sens, mod3_sens, mod4_sens,
          show.intercept = F, digits.p = 4,  show.r2 = T,  file = "outputs/abcd_ers_supp_7.xls")
tab_model(mod1_sens, mod2_sens, mod1_sens_com_fh, mod2_sens_com_fh,  mod3_sens, mod4_sens,
          show.intercept = F, digits.p = 4,  show.r2 = T)

anova(mod1_sens, mod2_sens, test="Chisq")$'Pr(>Chisq)'[2]
anova(mod3_sens, mod4_sens, test="Chisq")$'Pr(>Chisq)'[2]
anova(mod1_sens_com_fh, mod3_sens, test="Chisq") 
anova(mod2_sens_com_fh, mod4_sens, test="Chisq") 

p.adjust(c(1.499856e-42,1.101567e-38, 3.178e-07 ,0.0008145), "fdr")





