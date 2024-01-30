library(lme4)
library(readr)
library(sjPlot)
# library(AICcmodavg)
# library(ROCR)



########################
#### 1. main models ####
########################
dataset <- read_csv("data/dataset_ERS.csv") 
dataset_fh_complete = dataset[complete.cases(dataset$famhx_ss_momdad_scd_p),]

basic_covs = c("interview_age", "(interview_age)^2","(interview_age)^3","sex")
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



# supp table 6
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


# supp table 7
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


# https://easystats.github.io/performance/articles/r2.html
# performance::r2(mod2)
# performance::r2_nakagawa(mod2) # this one is being used in the sjPlot
# performance::r2_nakagawa(mod1)
# effectsize::cohens_f_squared(mod1, model2 = mod2) # not supported 

# https://rdrr.io/cran/MuMIn/man/r.squaredGLMM.html
# MuMIn::r.squaredGLMM(mod2)
# MuMIn::r.squaredGLMM(mod2, mod1)

# library(epicalc)/ lmtest
# lrtest (model0, model1)

# models <- list(mod1, mod2, mod3, mod4, mod5)
# aictab(cand.set = models)


