library(lme4)
library(readr)
library(sjPlot)
# library(AICcmodavg)
# library(ROCR)


dataset <- read.csv("data/dataset_ESS.csv") #"non_hispanic_black", "non_hispanic_white",
covs = c("interview_age", "(interview_age)^2","(interview_age)^3","sex", "race_black", "race_white" ,  "ethnicity_hisp" ) #,"race_black", "race_white" ,  "ethnicity_hisp" 
cov_fh = c(covs, "famhx_ss_momdad_scd_p")

# mod_i = lm(exwas_individual_sum ~ race_black + race_white + ethnicity_hisp, data=dataset,na.action=na.exclude)
# mod_s = lm(exwas_structural_sum ~ race_black + race_white + ethnicity_hisp, data=dataset,na.action=na.exclude)
# dataset$exwas_individual_sum_reg = scale(residuals(mod_i,na.action=na.exclude))[,1]
# dataset$exwas_structural_sum_reg = scale(residuals(mod_s,na.action=na.exclude))[,1]

# dataset = dataset[complete.cases(dataset$famhx_ss_momdad_scd_p), ]
# dataset = dataset[dataset$non_hispanic_black == 1, ]
# dataset = dataset[dataset$non_hispanic_white == 1, ]
# dataset = dataset[dataset$sex == "M", ]
# dataset = dataset[dataset$sex == "F", ]
# dataset = dataset[dataset$genetic_afr == 1, ]

mod1 = glmer(paste0("SA_y ~ ", paste(covs,collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2 = glmer(paste0("SA_y ~ exwas_individual_sum_z + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)

mod1_FH = glmer(paste0("SA_y ~ ", paste(cov_fh,collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2_FH = glmer(paste0("SA_y ~ exwas_individual_sum_z + ", paste(cov_fh, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)


t1 = tab_model(mod1, mod2, mod1_FH, mod2_FH,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T) 

t1




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



# library(epicalc)/ lmtest
# lrtest (model0, model1)

models <- list(mod1, mod2, mod3, mod4, mod5)
aictab(cand.set = models)





#### plots ####
library(ggplot2)
x_main = "Individual sum score"
y_main = "Probability of suicide attempt"

df = dataset[complete.cases(dataset$ethnicity_hisp),]
df$Pedicted_SA_probability =  predict(mod2,type = "response")

### sex
df$category = factor(df$sex, labels = c("Female", "Male"))
p=ggplot(data=df, aes(x=exwas_individual_sum_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = y_main) +
  theme(legend.title=element_blank())
p

save_plot(filename = "plots/sex.tif",  p,  dpi=700)


### non hisp groups
df$category = df$ethnicity_hisp
df$category = ifelse(df$non_hispanic_black == 1 & df$non_hispanic_white == 0 , 2, df$category)
df$category = ifelse(df$non_hispanic_black == 0 & df$non_hispanic_white == 1 , 3, df$category)
df$category[df$category==0] =NA
df$category = factor(df$category, levels = c(2,3,1), labels = c("Non Hispanic Black", "Non Hispanic White", "Hispanic"))
p=ggplot(data=df[complete.cases(df$category),], aes(x=exwas_individual_sum_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = y_main) +
  theme(legend.title=element_blank())
p

save_plot(filename = "plots/hisp_3_categories.tif",  p,  dpi=700)


### PRS
mean_prs = mean(na.exclude(df$suicide_PRSice_Pt0_05))
sd_prs = sd(df$suicide_PRSice_Pt0_05,na.rm = T)
df$category = NA
df$category = ifelse(df$suicide_PRSice_Pt0_05 > (mean_prs+3*sd_prs), "High", NA)
df$category = ifelse(df$suicide_PRSice_Pt0_05 < (mean_prs-3*sd_prs), "Low", df$category)
# df$category = ifelse( is.na(df$category) &  !is.na(df$suicide_PRSice_Pt0_05) , "Center", df$category)
df$category = factor(df$category)
p=ggplot(data=df[complete.cases(df$category),], aes(x=exwas_individual_sum_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = y_main) +
  theme(legend.title=element_blank())
p

save_plot(filename = "plots/PRS_3sd.png",  p,  dpi=200)

p=ggplot(data=df[complete.cases(df$suicide_PRSice_Pt0_05),], aes(x=suicide_PRSice_Pt0_05, y=exwas_individual_sum_z)) +
  geom_point(alpha = 0.25)+
  geom_smooth(method=lm,  level=0.95)+
  labs(x= "PRS", y = x_main ) +
  theme(legend.title=element_blank())
p
save_plot(filename = "plots/PRS_vs_sum_score.png",  p,  dpi=200)

