library(lme4)
library(readr)
library(sjPlot)
library(AICcmodavg)
library(ROCR)


dataset <- read.csv("data/dataset_ESS.csv") #"non_hispanic_black", "non_hispanic_white",
covs = c("interview_age", "race_black", "race_white" ,  "ethnicity_hisp" ) #,"race_black", "race_white" ,  "ethnicity_hisp"
cov_fh = c(covs, "famhx_ss_momdad_scd_p")

# mod_i = lm(exwas_individual_sum ~ race_black + race_white + ethnicity_hisp, data=dataset,na.action=na.exclude)
# mod_s = lm(exwas_structural_sum ~ race_black + race_white + ethnicity_hisp, data=dataset,na.action=na.exclude)
# dataset$exwas_individual_sum_reg = scale(residuals(mod_i,na.action=na.exclude))[,1]
# dataset$exwas_structural_sum_reg = scale(residuals(mod_s,na.action=na.exclude))[,1]

# dataset = dataset[complete.cases(dataset$famhx_ss_momdad_scd_p), ]
# dataset = dataset[dataset$non_hispanic_black == 1, ]
# dataset = dataset[dataset$non_hispanic_white == 1, ]
# dataset = dataset[dataset$sex == "M", ]
dataset = dataset[dataset$sex == "F", ]

mod1 = glmer(paste0("SA_y ~ ", paste(covs,collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2 = glmer(paste0("SA_y ~ exwas_individual_sum_z + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod3 = glmer(paste0("SA_y ~ exwas_structural_sum_z + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod4 = glmer(paste0("SA_y ~ exwas_individual_sum_z + exwas_structural_sum_z + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod5 = glmer(paste0("SA_y ~ exwas_individual_sum_z * exwas_structural_sum_z + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)

mod1_FH = glmer(paste0("SA_y ~ ", paste(cov_fh,collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2_FH = glmer(paste0("SA_y ~ exwas_individual_sum_z + ", paste(cov_fh, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod3_FH = glmer(paste0("SA_y ~ exwas_structural_sum_z + ", paste(cov_fh, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod4_FH = glmer(paste0("SA_y ~ exwas_individual_sum_z + exwas_structural_sum_z + ", paste(cov_fh, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod5_FH = glmer(paste0("SA_y ~ exwas_individual_sum_z * exwas_structural_sum_z + ", paste(cov_fh, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)


t1 = tab_model(mod1, mod2, mod3, mod4, mod5,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T) 

t2 = tab_model(mod1_FH, mod2_FH, mod3_FH, mod4_FH, mod5_FH,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T) 

t1
t2

get_auc <- function(mod){
  
  y_predicted <- predict(mod, type ="response")
  y_test = dataset$SA_y[complete.cases(dataset[,covs])]
  pred <- prediction(y_predicted, y_test)
  round(performance(pred, measure = "auc")@y.values[[1]], digits = 4)
  
}


print(paste0("mod 1 AUC: ", get_auc(mod1)))
print(paste0("mod 2 AUC: ", get_auc(mod2)))
print(paste0("mod 3 AUC: ", get_auc(mod3)))
print(paste0("mod 4 AUC: ", get_auc(mod4)))
print(paste0("mod 5 AUC: ", get_auc(mod5)))


anova(mod1, mod2, test="Chisq")
anova(mod1, mod3, test="Chisq")
anova(mod1, mod4, test="Chisq")
anova(mod2, mod4, test="Chisq")
anova(mod3, mod4, test="Chisq")
anova(mod1, mod5, test="Chisq")
anova(mod2, mod5, test="Chisq")
anova(mod3, mod5, test="Chisq")
anova(mod4, mod5, test="Chisq")

anova(mod1_FH, mod2_FH, test="Chisq")
anova(mod1_FH, mod3_FH, test="Chisq")
anova(mod1_FH, mod4_FH, test="Chisq")
anova(mod2_FH, mod4_FH, test="Chisq")
anova(mod3_FH, mod4_FH, test="Chisq")
anova(mod1_FH, mod5_FH, test="Chisq")
anova(mod2_FH, mod5_FH, test="Chisq")
anova(mod3_FH, mod5_FH, test="Chisq")
anova(mod4_FH, mod5_FH, test="Chisq")

anova(mod1, mod1_FH, test="Chisq") 
anova(mod2, mod2_FH, test="Chisq")
anova(mod3, mod3_FH, test="Chisq")
anova(mod4, mod4_FH, test="Chisq")
anova(mod5, mod5_FH, test="Chisq")


models <- list(mod1, mod2, mod3, mod4, mod5)
aictab(cand.set = models)



