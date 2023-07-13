library(lme4)
library(readr)
library(sjPlot)
library(AICcmodavg)
library(meta)
library(performance)
library(purrr)



#####################################
# help functions
#####################################
create_meta_df <- function (feature, model1, model2) {
  mod1_coef = summary(model1)$coef
  mod2_coef = summary(model2)$coef
  
  df = data.frame(
    TE = c(mod1_coef[, "Estimate"][feature], mod2_coef[, "Estimate"][feature]),
    seTE = c(mod1_coef[, "Std. Error"][feature], mod2_coef[, "Std. Error"][feature])
  )
}


run_meta_analysis <- function(IV, modEUR, modAFR){
  
  df = create_meta_df(IV, modEUR, modAFR)
  met = metagen(TE, seTE, studlab = c("EUR", "AFR"),  data = df, sm = "OR")
  cat(paste0("\n\n ------ meta analysis for ",IV," ------\n\n"))
  print(met)
}

run_models <- function( covars = NULL) {
  
  formula_str = paste0("SA_y_ever ~ ", paste(covars,collapse = " + "), "+ (1 | site_id_l_br/rel_family_id)")
  f = as.formula(formula_str)
  
  dataset_ = dataset
  
  modEUR = glmer(f, data = dataset_[dataset_$genetic_afr == 0,], family = binomial, nAGQ = 0)
  modAFR = glmer(f, data = dataset_[dataset_$genetic_afr == 1,], family = binomial, nAGQ = 0)
  
  map(covars, ~run_meta_analysis(.x, modEUR, modAFR))
  
  return(list(modEUR =modEUR, modAFR = modAFR))
  
}



#####################################
# organize data
#####################################

dataset <- read.csv("data/dataset_ESS_wide.csv") 


covs = c("age", "sex_br" )
cov_g = c(covs, "suicide_PRSice_Pt0_05")
cov_fh = c(covs, "famhx_ss_momdad_scd_p")
cov_fh_g = c(cov_fh, "suicide_PRSice_Pt0_05")



#####################################
# run models
#####################################
mod_meta_1 = run_models(covs)
mod_meta_2 = run_models(cov_g)
mod_meta_3 = run_models(c(cov_g, "exwas_individual_sum_z"))
mod_meta_4 = run_models(c(cov_g, "exwas_structural_sum_z"))
mod_meta_5 = run_models(c(cov_g, "exwas_individual_sum_z", "exwas_structural_sum_z"))
mod_meta_6 = run_models(c(cov_g, "exwas_individual_sum_z", "exwas_structural_sum_z",
                          "exwas_individual_sum_z:exwas_structural_sum_z",
                          "suicide_PRSice_Pt0_05:exwas_individual_sum_z",
                          "suicide_PRSice_Pt0_05:exwas_structural_sum_z"))

teur = tab_model(mod_meta_1$modEUR, mod_meta_2$modEUR, mod_meta_3$modEUR, 
                 mod_meta_4$modEUR, mod_meta_5$modEUR, mod_meta_6$modEUR, 
                 show.intercept = F )

tafr = tab_model(mod_meta_1$modAFR, mod_meta_2$modAFR, mod_meta_3$modAFR, 
                 mod_meta_4$modAFR, mod_meta_5$modAFR, mod_meta_6$modAFR, 
                 show.intercept = F )


mod_meta_1_fh = run_models(cov_fh)
mod_meta_2_fh = run_models(cov_fh_g)
mod_meta_3_fh = run_models(c(cov_fh_g, "exwas_individual_sum_z"))
mod_meta_4_fh = run_models(c(cov_fh_g, "exwas_structural_sum_z"))
mod_meta_5_fh = run_models(c(cov_fh_g, "exwas_individual_sum_z", "exwas_structural_sum_z"))
mod_meta_6_fh = run_models(c(cov_fh_g, "exwas_individual_sum_z", "exwas_structural_sum_z",
                          "exwas_individual_sum_z:exwas_structural_sum_z",
                          "suicide_PRSice_Pt0_05:exwas_individual_sum_z",
                          "suicide_PRSice_Pt0_05:exwas_structural_sum_z"))

teur = tab_model(mod_meta_1_fh$modEUR, mod_meta_2_fh$modEUR, mod_meta_3_fh$modEUR, 
                 mod_meta_4_fh$modEUR, mod_meta_5_fh$modEUR, mod_meta_6_fh$modEUR, 
                 show.intercept = F )

tafr = tab_model(mod_meta_1_fh$modAFR, mod_meta_2_fh$modAFR, mod_meta_3_fh$modAFR, 
                 mod_meta_4_fh$modAFR, mod_meta_5_fh$modAFR, mod_meta_6_fh$modAFR, 
                 show.intercept = F )
 

#####################################
# compare models
#####################################
#EUR
anova(mod_meta_1$modEUR, mod_meta_2$modEUR,test="Chisq")
anova(mod_meta_2$modEUR, mod_meta_3$modEUR,test="Chisq")
anova(mod_meta_2$modEUR, mod_meta_4$modEUR,test="Chisq")
anova(mod_meta_2$modEUR, mod_meta_5$modEUR,test="Chisq")
anova(mod_meta_3$modEUR, mod_meta_5$modEUR,test="Chisq")
anova(mod_meta_4$modEUR, mod_meta_5$modEUR,test="Chisq")
anova(mod_meta_2$modEUR, mod_meta_6$modEUR,test="Chisq")
anova(mod_meta_3$modEUR, mod_meta_6$modEUR,test="Chisq")
anova(mod_meta_4$modEUR, mod_meta_6$modEUR,test="Chisq")
anova(mod_meta_5$modEUR, mod_meta_6$modEUR,test="Chisq")


#AFR
anova(mod_meta_1$modAFR, mod_meta_2$modAFR,test="Chisq")
anova(mod_meta_2$modAFR, mod_meta_3$modAFR,test="Chisq")
anova(mod_meta_2$modAFR, mod_meta_4$modAFR,test="Chisq")
anova(mod_meta_2$modAFR, mod_meta_5$modAFR,test="Chisq")
anova(mod_meta_3$modAFR, mod_meta_5$modAFR,test="Chisq")
anova(mod_meta_4$modAFR, mod_meta_5$modAFR,test="Chisq")
anova(mod_meta_2$modAFR, mod_meta_6$modAFR,test="Chisq")
anova(mod_meta_3$modAFR, mod_meta_6$modAFR,test="Chisq")
anova(mod_meta_4$modAFR, mod_meta_6$modAFR,test="Chisq")
anova(mod_meta_5$modAFR, mod_meta_6$modAFR,test="Chisq")



#EUR
anova(mod_meta_1_fh$modEUR, mod_meta_2_fh$modEUR,test="Chisq")
anova(mod_meta_2_fh$modEUR, mod_meta_3_fh$modEUR,test="Chisq")
anova(mod_meta_2_fh$modEUR, mod_meta_4_fh$modEUR,test="Chisq")
anova(mod_meta_2_fh$modEUR, mod_meta_5_fh$modEUR,test="Chisq")
anova(mod_meta_3_fh$modEUR, mod_meta_5_fh$modEUR,test="Chisq")
anova(mod_meta_4_fh$modEUR, mod_meta_5_fh$modEUR,test="Chisq")
anova(mod_meta_2_fh$modEUR, mod_meta_6_fh$modEUR,test="Chisq")
anova(mod_meta_3_fh$modEUR, mod_meta_6_fh$modEUR,test="Chisq")
anova(mod_meta_4_fh$modEUR, mod_meta_6_fh$modEUR,test="Chisq")
anova(mod_meta_5_fh$modEUR, mod_meta_6_fh$modEUR,test="Chisq")


#AFR
anova(mod_meta_1_fh$modAFR, mod_meta_2_fh$modAFR,test="Chisq")
anova(mod_meta_2_fh$modAFR, mod_meta_3_fh$modAFR,test="Chisq")
anova(mod_meta_2_fh$modAFR, mod_meta_4_fh$modAFR,test="Chisq")
anova(mod_meta_2_fh$modAFR, mod_meta_5_fh$modAFR,test="Chisq")
anova(mod_meta_3_fh$modAFR, mod_meta_5_fh$modAFR,test="Chisq")
anova(mod_meta_4_fh$modAFR, mod_meta_5_fh$modAFR,test="Chisq")
anova(mod_meta_2_fh$modAFR, mod_meta_6_fh$modAFR,test="Chisq")
anova(mod_meta_3_fh$modAFR, mod_meta_6_fh$modAFR,test="Chisq")
anova(mod_meta_4_fh$modAFR, mod_meta_6_fh$modAFR,test="Chisq")
anova(mod_meta_5_fh$modAFR, mod_meta_6_fh$modAFR,test="Chisq")


models <- list(mod1, mod2, mod3, mod4, mod5)
aictab(cand.set = models)



