library(lme4)
library(readr)
library(sjPlot)
library(AICcmodavg)


dataset <- read.csv("data/dataset_ESS.csv")
cov = c("interview_age", "sex", "race_black", "race_white", "ethnicity_hisp" ) #"non_hispanic_black", "non_hispanic_white",
cov = c(cov, "suicide_PRSice_Pt0_05")

mod_i = lm(exwas_individual_sum ~ race_black + race_white + ethnicity_hisp, data=dataset,na.action=na.exclude)
mod_s = lm(exwas_structural_sum ~ race_black + race_white + ethnicity_hisp, data=dataset,na.action=na.exclude)
dataset$exwas_individual_sum_reg = scale(residuals(mod_i,na.action=na.exclude))[,1]
dataset$exwas_structural_sum_reg = scale(residuals(mod_s,na.action=na.exclude))[,1]

mod1 = glmer(paste0("SA_y ~ ", paste(cov,collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2 = glmer(paste0("SA_y ~ exwas_individual_sum_reg + ", paste(cov,collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod3 = glmer(paste0("SA_y ~ exwas_structural_sum_reg + ", paste(cov,collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod4 = glmer(paste0("SA_y ~ exwas_individual_sum_reg + exwas_structural_sum_reg + ", paste(cov,collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)


tab_model(mod1, mod2, mod3, mod4, show.intercept = F, 
          show.ngroups = T, show.aic = T, show.aicc = T, show.r2 = T) 

anova(mod1, mod2, test="Chisq")
anova(mod1, mod3, test="Chisq")
anova(mod2, mod4, test="Chisq")


models <- list(mod1, mod2, mod3, mod4)
aictab(cand.set = models)



