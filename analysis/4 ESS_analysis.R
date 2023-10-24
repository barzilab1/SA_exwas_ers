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



##### grant
dataset <- read.csv("data/dataset_ESS_grant.csv") #"non_hispanic_black", "non_hispanic_white",

dataset$resi_cbcl = residuals(lm(cbcl_scr_syn_external_t~exwas_individual_sum_z, dataset, na.action=na.exclude))

mod1 = glmer(paste0("SA_y ~ ", paste(covs,collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod2 = glmer(paste0("SA_y ~ exwas_individual_sum_z + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod3 = glmer(paste0("SA_y ~ cbcl_scr_syn_external_t + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod4 = glmer(paste0("SA_y ~ exwas_individual_sum_z + cbcl_scr_syn_external_t + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod5 = glmer(paste0("SA_y ~ exwas_individual_sum_z*cbcl_scr_syn_external_t + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)

mod3r = glmer(paste0("SA_y ~ resi_cbcl + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod4r = glmer(paste0("SA_y ~ exwas_individual_sum_z + resi_cbcl + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)
mod5r = glmer(paste0("SA_y ~ exwas_individual_sum_z*resi_cbcl + ", paste(covs, collapse = " + ") , " + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = dataset, nAGQ = 0)


tab_model(mod1, mod2, mod3, mod4, mod5, mod3r, mod4r, mod5r,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T,  file = "outputs/abcd_ers_cbcl_results.doc")

##### end grant

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




dataset$race_eth = as.factor(dataset$race_eth)

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
x_main = element_blank()
y_main = "Probability of Suicide Attempt"

df = dataset[complete.cases(dataset$ethnicity_hisp),]
df$Pedicted_SA_probability =  predict(mod2,type = "response")


FONT_SIZE = 26

theme_set(theme_minimal() + #scale_color_manual(values = c("#158EA7", "#844D88","#8DC34A"))+ 
            theme(
              plot.title = element_text(size = FONT_SIZE, face = "bold"),
              plot.subtitle = element_blank(),
              text = element_text(size = FONT_SIZE, face="bold"),
              axis.text = element_text(color='black',size = FONT_SIZE-5, face="bold"),
              legend.text = element_text(size = FONT_SIZE, face="bold")))



### lgbt
df1 = df[complete.cases(df$LGBT_inclusive),]
df1$category = factor(df1$LGBT_inclusive, labels = c("NON LGBT", "LGBT"))

p_lgbt=ggplot(data=df1, aes(x=exwas_individual_sum_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = element_blank()) + 
  scale_colour_manual(values =  c("#158EA7", "#844D88"))+ 
  scale_fill_manual(values =  c( "#158EA7", "#844D88"))+ 
  theme(legend.title=element_blank(), legend.position = c(.75, .12))+
  scale_x_continuous( breaks = seq(-2, 10, 3))
  
p_lgbt

# ggsave(filename ="plots/lgbt.png", p, width = 20, height = 15, dpi=700)

### sex
df$category = factor(df$sex, labels = c("Female", "Male"))
p_sex=ggplot(data=df, aes(x=exwas_individual_sum_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = y_main) +
  scale_colour_manual(values =  c("#158EA7", "#844D88"))+ 
  scale_fill_manual(values =  c( "#158EA7", "#844D88"))+ 
  theme(legend.title=element_blank(), legend.position = c(.8, .12))+
  scale_y_continuous( breaks = c(seq(0, .2, .05), .24))+
  scale_x_continuous( breaks = seq(-2, 10, 3))
p_sex

# save_plot(filename = "plots/sex.tif",  p,  dpi=700)


### non hisp groups

df$category = factor(df$race_eth)

p_race=ggplot(data=df[complete.cases(df$category),], aes(x=exwas_individual_sum_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = element_blank()) +
  scale_colour_manual(values =  c("#158EA7", "#844D88", "#8DC34A"))+ 
  scale_fill_manual(values =  c( "#158EA7", "#844D88","#8DC34A"))+ 
  theme(legend.title=element_blank(), legend.position = c(.75, .14))+
  scale_x_continuous( breaks = seq(-2, 10, 3))
p_race

# save_plot(filename = "plots/hisp_3_categories.tif",  p,  dpi=700)


p = ggarrange(p_sex,p_lgbt,p_race, nrow = 1, widths = c(.4,.4,.4) ) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "black"))
p


ggsave(filename ="plots/differential effects.tiff", p, width = 18, height = 6, dpi=700)




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

