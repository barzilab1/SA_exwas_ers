library(lme4)
library(readr)
library(sjPlot)
library(ggplot2)
library(ggpubr)
# library(AICcmodavg)
# library(ROCR)



########################
#### 1. main models ####
########################
dataset <- read.csv("data/dataset_ERS.csv") 

covs = c("interview_age", "(interview_age)^2","(interview_age)^3","sex", "race_black", "race_white" , "ethnicity_hisp" )
cov_fh = c(covs, "famhx_ss_momdad_scd_p")
random_exp = "(1 | site_id_l_br/rel_family_id/src_subject_id)"

dataset$resi_cbcl = residuals(lm(cbcl_scr_syn_external_t~ers_z, dataset, na.action=na.exclude))

mod1 = glmer(paste0("SA_y ~ ", paste(covs,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2 = glmer(paste0("SA_y ~ ers_z + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)

mod1_FH = glmer(paste0("SA_y ~ ", paste(cov_fh,collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_FH = glmer(paste0("SA_y ~ ers_z + ", paste(cov_fh, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)

mod1_cbc_ext = glmer(paste0("SA_y ~ cbcl_scr_syn_external_t + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_cbc_ext = glmer(paste0("SA_y ~ ers_z + cbcl_scr_syn_external_t + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod3_cbc_ext = glmer(paste0("SA_y ~ ers_z*cbcl_scr_syn_external_t + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)

mod1_cbc_ext_r = glmer(paste0("SA_y ~ resi_cbcl + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_cbc_ext_r = glmer(paste0("SA_y ~ ers_z + resi_cbcl + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod3_cbc_ext_r = glmer(paste0("SA_y ~ ers_z*resi_cbcl + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)

mod1_all = glmer(paste0("SA_y ~ ers_z + cbcl_scr_syn_external_t + ", paste(cov_fh, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod1_all_r = glmer(paste0("SA_y ~ ers_z + resi_cbcl + ", paste(cov_fh, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)

tab_model(mod1, mod2, mod1_FH, mod2_FH, mod1_cbc_ext, mod2_cbc_ext, mod3_cbc_ext, mod1_cbc_ext_r, mod2_cbc_ext_r, mod3_cbc_ext_r,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T,  file = "outputs/abcd_ers_results.doc")


tab_model( mod1_all, mod1_all_r,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T,  file = "outputs/abcd_ers_results_1.doc")




# https://easystats.github.io/performance/articles/r2.html
# performance::r2(mod2)
# performance::r2_nakagawa(mod2) # this one is being used in the sjPlot
# performance::r2_nakagawa(mod1)
# effectsize::cohens_f_squared(mod1, model2 = mod2) # not supported 

# https://rdrr.io/cran/MuMIn/man/r.squaredGLMM.html
# MuMIn::r.squaredGLMM(mod2)
# MuMIn::r.squaredGLMM(mod2, mod1)


anova(mod1, mod2, test="Chisq")
anova(mod1_FH, mod2_FH, test="Chisq")
anova(mod1, mod1_FH, test="Chisq") 
anova(mod2, mod2_FH, test="Chisq")


# library(epicalc)/ lmtest
# lrtest (model0, model1)

# models <- list(mod1, mod2, mod3, mod4, mod5)
# aictab(cand.set = models)



######################
#### 2. subgroups ####
######################

dataset$race_eth = as.factor(dataset$race_eth)

# interaction by subgroups
mod2_sex =    glmer(paste0("SA_y ~ ers_z*sex + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_race =   glmer(paste0("SA_y ~ ers_z*race_black + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_race1 =  glmer(paste0("SA_y ~ ers_z*ethnicity_hisp + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_race2 =  glmer(paste0("SA_y ~ ers_z*race_black + ers_z*ethnicity_hisp +", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_race_sex = glmer(paste0("SA_y ~  ers_z*sex + ers_z*race_black + ers_z*ethnicity_hisp + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_lgbt =   glmer(paste0("SA_y ~ ers_z+LGBT_inclusive + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)
mod2_lgbt1 =  glmer(paste0("SA_y ~ ers_z*LGBT_inclusive + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset, nAGQ = 0)

tab_model(mod2, mod2_sex, mod2_race, mod2_race1, mod2_race2, mod2_race_sex, mod2_lgbt, mod2_lgbt1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T , digits.rsq = 4,
          file = "outputs/abcd_ers_subgroups_results.doc")


# stratify by lgbt 
mod2_lgbt_0 =   glmer(paste0("SA_y ~ ers_z + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset[dataset$LGBT_inclusive==0,], nAGQ = 0)
mod2_lgbt_1 =   glmer(paste0("SA_y ~ ers_z + ", paste(covs, collapse = " + ") , " + " , random_exp), family = binomial, data = dataset[dataset$LGBT_inclusive==1,], nAGQ = 0)

tab_model(mod2, mod2_lgbt_0, mod2_lgbt_1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T , digits.rsq = 4,
          file = "outputs/abcd_ers_lgbt_results.doc")


#### plots ####

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

p_lgbt=ggplot(data=df1, aes(x=ers_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = element_blank()) + 
  scale_colour_manual(values =  c("#158EA7", "#844D88"))+ 
  scale_fill_manual(values =  c( "#158EA7", "#844D88"))+ 
  theme(legend.title=element_blank(), legend.position = c(.75, .12))+
  scale_x_continuous( breaks = seq(-2, 10, 3))
  
p_lgbt


### sex
df$category = factor(df$sex, labels = c("Female", "Male"))
p_sex=ggplot(data=df, aes(x=ers_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = y_main) +
  scale_colour_manual(values =  c("#158EA7", "#844D88"))+ 
  scale_fill_manual(values =  c( "#158EA7", "#844D88"))+ 
  theme(legend.title=element_blank(), legend.position = c(.8, .12))+
  scale_y_continuous( breaks = c(seq(0, .2, .05), .24))+
  scale_x_continuous( breaks = seq(-2, 10, 3))
p_sex



### non hisp groups
p_race=ggplot(data=df[complete.cases(df$category),], aes(x=ers_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = element_blank()) +
  scale_colour_manual(values =  c("#158EA7", "#844D88", "#8DC34A"))+ 
  scale_fill_manual(values =  c( "#158EA7", "#844D88","#8DC34A"))+ 
  theme(legend.title=element_blank(), legend.position = c(.75, .14))+
  scale_x_continuous( breaks = seq(-2, 10, 3))
p_race



p = ggarrange(p_sex,p_lgbt,p_race, nrow = 1, widths = c(.4,.4,.4) ) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "black"))
p


ggsave(filename ="plots/differential effects.tiff", p, width = 18, height = 6, dpi=700)




