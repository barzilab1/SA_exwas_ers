library(lme4)
library(readr)
library(sjPlot)
# library(AICcmodavg)
# library(ROCR)


dataset <- read.csv("data/dataset_ESS.csv") #"non_hispanic_black", "non_hispanic_white",
covs = c("interview_age", "(interview_age)^2","(interview_age)^3","sex", "race_black", "race_white" ,  "ethnicity_hisp" ) #,"race_black", "race_white" ,  "ethnicity_hisp" 
cov_fh = c(covs, "famhx_ss_momdad_scd_p")

mod1 = glmer(paste0("SA_y ~ ", paste(covs,collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2 = glmer(paste0("SA_y ~ exwas_individual_sum_z + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)

mod1_FH = glmer(paste0("SA_y ~ ", paste(cov_fh,collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2_FH = glmer(paste0("SA_y ~ exwas_individual_sum_z + ", paste(cov_fh, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)


tab_model(mod1, mod2, mod1_FH, mod2_FH,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T,  file = "outputs/abcd_ers_results.doc")



# https://easystats.github.io/performance/articles/r2.html
performance::r2(mod2)
performance::r2_nakagawa(mod2) # this is being used in the sjPlot
performance::r2_nakagawa(mod1)
effectsize::cohens_f_squared(mod1, model2 = mod2) # not supported 

# https://rdrr.io/cran/MuMIn/man/r.squaredGLMM.html
MuMIn::r.squaredGLMM(mod2)
MuMIn::r.squaredGLMM(mod2, mod1)




anova(mod1, mod2, test="Chisq")

anova(mod1_FH, mod2_FH, test="Chisq")

anova(mod1, mod1_FH, test="Chisq") 
anova(mod2, mod2_FH, test="Chisq")






## check quntailes 
quantiles_ers_control =  quantile(dataset$exwas_individual_sum_z[dataset$SA_y == 0], seq(0,1,0.2))
dataset$q_group = findInterval(dataset$exwas_individual_sum_z, quantiles_ers_control, rightmost.closed = T)
dataset$q_group[dataset$q_group ==6] = 5
dataset$q_group = factor(dataset$q_group, levels = c(3,1,2,4,5))

modq1 = glmer(paste0("SA_y ~ q_group + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)


tab_model(modq1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T)

dataset_plot = data.frame(
  Quintiles = seq(1:5),
  OR = c(0.06,0.63, 1, 1.24, 4.76),
  ci_lower = c(0.01,0.30,NA,0.67,2.82),
  ci_upper = c(0.46,1.33,NA,2.27,8.05)
)

library(ggplot2)
ggplot(dataset_plot, aes(x=Quintiles, y=OR)) +
  geom_point(shape=95, size = 7)+
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.025) +
  scale_y_continuous( name = "Odds Ratio",
                      limits = c(0, NA),
                      breaks = seq(0,8.25,1),
                      labels = c(0,"",2,"",4,"",6,"",8),
                      sec.axis = sec_axis(trans = ~.*1,
                                          breaks = seq(0,8.25,1),
                                          labels = c(0,"",2,"",4,"",6,"",8)),
                     expand = expansion(mult = c(0, 0.1))
                     )+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



# library(epicalc)/ lmtest
# lrtest (model0, model1)

models <- list(mod1, mod2, mod3, mod4, mod5)
aictab(cand.set = models)




dataset$race_eth = factor(dataset$race_eth, levels = c("NH-White", "Hispanic","NH-Black"))

# interaction by subgroups
mod2_sex =    glmer(paste0("SA_y ~ exwas_individual_sum_z*sex + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2_race =   glmer(paste0("SA_y ~ exwas_individual_sum_z*race_black + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2_race1 =  glmer(paste0("SA_y ~ exwas_individual_sum_z*ethnicity_hisp + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2_race2 =  glmer(paste0("SA_y ~ exwas_individual_sum_z*race_black + exwas_individual_sum_z*ethnicity_hisp +", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2_race_sex = glmer(paste0("SA_y ~  exwas_individual_sum_z*sex + exwas_individual_sum_z*race_black + exwas_individual_sum_z*ethnicity_hisp + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2_lgbt =   glmer(paste0("SA_y ~ exwas_individual_sum_z+LGBT_inclusive + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2_lgbt1 =  glmer(paste0("SA_y ~ exwas_individual_sum_z*LGBT_inclusive + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)

tab_model(mod2, mod2_sex, mod2_race, mod2_race1, mod2_race2, mod2_race_sex, mod2_lgbt, mod2_lgbt1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T , digits.rsq = 4,
          file = "outputs/abcd_ers_subgroups_results.doc")


# stratify by lgbt 
mod2_lgbt_0 =   glmer(paste0("SA_y ~ exwas_individual_sum_z + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset[dataset$LGBT_inclusive==0,], nAGQ = 0)
mod2_lgbt_1 =   glmer(paste0("SA_y ~ exwas_individual_sum_z + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset[dataset$LGBT_inclusive==1,], nAGQ = 0)

tab_model(mod2, mod2_lgbt_0, mod2_lgbt_1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T , digits.rsq = 4,
          file = "outputs/abcd_ers_lgbt_results.doc")


#### plots ####
library(ggplot2)
x_main = "ERS"
y_main = "Probability of Suicide Attempt"

df = dataset[complete.cases(dataset$ethnicity_hisp),]
df$Pedicted_SA_probability =  predict(mod2,type = "response")

### lgbt
df = df[complete.cases(df$LGBT_inclusive),]
df$category = factor(df$LGBT_inclusive, labels = c("NON LGBT", "LGBT"))

p=ggplot(data=df, aes(x=exwas_individual_sum_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = y_main) +
  theme_minimal() +
  theme(legend.title=element_blank(), legend.position = c(.9, .1))
  
p

ggsave(filename ="plots/lgbt.png", p, width = 20, height = 15, dpi=700)

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

