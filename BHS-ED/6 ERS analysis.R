library(data.table)
library(performance)
library(sjPlot)
library(readr)
library(ggpubr)

setwd("~/sa_exwas")

testing_df = read_csv("data/df_testing_imputed.csv")
setDT(testing_df)


#########################
#### 1. ERS Analysis ####
#########################
covariates = c("age_at_screen", "sex", "race_black" ,"race_white", "ethnicity_new" )
outcome = "bhssu04"


mod1 = glm(paste0(outcome, " ~ ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod2 = glm(paste0(outcome, " ~ ers_z + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod3 = glm(paste0(outcome, " ~ depression_score +", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod4 = glm(paste0(outcome, " ~ ers_z + depression_score +", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)

t1 = tab_model(mod1, mod2, mod3, mod4, show.intercept = F, show.ngroups = T, show.aic = T) 
t1

anova(mod1, mod2, test="Chisq")
anova(mod3, mod4, test="Chisq")
anova(mod2, mod4, test="Chisq")

r2_nagelkerke(mod1) # 0.02488547 
r2_nagelkerke(mod2) # 0.1859619
r2_nagelkerke(mod3) # 0.2718739
r2_nagelkerke(mod4) # 0.3149205



######################################
#### 2. ERS Analysis by subgroups ####
######################################

#gender
# df1[, sum(TRANS, na.rm = T) ]
# df1[, mean(TRANS, na.rm = T) *100 ]
# df1[, mean(SA_y) * 100, by = TRANS]
# df1[, chisq.test(TRANS, SA_y , correct = F)]
# 
# df1[, t.test( exwas_individual_sum_z ~ TRANS )]
# df1[, wilcox.test( exwas_individual_sum_z ~ TRANS )]


# interaction by subgroups
mod2_sex =   glm(paste0(outcome, " ~ ers_z*sex + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod2_race =   glm(paste0(outcome, " ~ ers_z*race_black + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod2_race1 =  glm(paste0(outcome, " ~ ers_z*ethnicity_new + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod2_race2 =  glm(paste0(outcome, " ~ ers_z*race_black + ers_z*ethnicity_new + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod2_race_sex = glm(paste0(outcome, " ~ ers_z*sex + ers_z*race_black + ers_z*ethnicity_new + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod2_lgbt =   glm(paste0(outcome, " ~ ers_z+TRANS + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod2_lgbt1 =  glm(paste0(outcome, " ~ ers_z*TRANS + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)

tab_model(mod2, mod2_sex, mod2_race, mod2_race1, mod2_race2, mod2_race_sex, mod2_lgbt, mod2_lgbt1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T , digits.rsq = 4)#,
          # file = "outputs/abcd_ers_subgroups_results.doc")


# stratify by lgbt 
mod2_lgbt_0 = glm(paste0(outcome, " ~ ers_z + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df[TRANS == 0,])
mod2_lgbt_1 = glm(paste0(outcome, " ~ ers_z + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df[TRANS == 1,])
tab_model(mod2, mod2_lgbt, mod2_lgbt1,mod2_lgbt_0, mod2_lgbt_1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T , digits.rsq = 4)




#### plots ####
library(ggplot2)
x_main = "ERS (z-score)" #element_blank()
y_main = "Probability of suicide attempt"

df = df_testing_imputed[complete.cases(ethnicity_new, race_white, sex),]
df$Pedicted_SA_probability =  predict(mod2,type = "response")


FONT_SIZE = 26

theme_set(theme_minimal() +  
            theme(
              plot.title = element_text(size = FONT_SIZE, face = "bold"),
              text = element_text(size = FONT_SIZE, face="bold"),
              axis.text = element_text(color='black',size = FONT_SIZE-5, face="bold"),
              legend.text = element_text(size = FONT_SIZE, face="bold")))


#### sex ####
df$category = factor(df$sex_abbr, labels = c( "Female","Male"))

p_sex=ggplot(data=df, aes(x=ers_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = y_main) +
  theme(legend.title=element_blank(),
        legend.position = c(0.75, 0.15)) +
  scale_colour_manual(values =  c("#158EA7", "#844D88"))+ 
  scale_fill_manual(values =  c( "#158EA7", "#844D88"))
p_sex

### non hisp groups ####
df$category = factor(df$race_eth)

p_race=ggplot(data=df[complete.cases(category),], aes(x=ers_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = element_blank()) +
  theme(legend.title=element_blank(),
        legend.position = c(0.7, 0.15)) +
  scale_colour_manual(values =  c("#158EA7", "#844D88", "#8DC34A"))+ 
  scale_fill_manual(values =  c( "#158EA7", "#844D88", "#8DC34A"))
p_race




### lgbt ####
df[, TRANS_ch := {
  fcase(
    TRANS == 1, "Transgender",
    TRANS == 0, "Not Transgender"
  )
}]
df$category = factor(df$TRANS_ch)


p_trans=ggplot(data=df[complete.cases(df$TRANS),], aes(x=ers_z, y=Pedicted_SA_probability, group = category, color=category)) +
  geom_smooth(method=lm,  level=0.95, aes(fill = category))+
  labs(x= x_main, y = element_blank()) +
  theme(legend.title=element_blank(),
        legend.position = c(0.57, 0.15)) +
  scale_colour_manual(values =  c("#158EA7", "#844D88"))+ 
  scale_fill_manual(values =  c( "#158EA7", "#844D88"))

p_trans




p = ggarrange(p_sex,p_trans,p_race, nrow = 1, widths = c(.4,.4,.4) ) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "black"))
p


ggsave(filename ="plots/differential effects.png", p, width = 21, height = 7, dpi=700)


