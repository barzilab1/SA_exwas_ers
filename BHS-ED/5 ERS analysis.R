library(data.table)
library(doParallel)
library(missForest)
library(performance)
library(sjPlot)
library(readr)
library(ggpubr)

setwd("~/sa_exwas")

############################
#### 1. Testing dataset ####
############################

matched_groups = read_csv("data/matched_groups.csv")
exposome_full_clean = read_csv("data/exposome_full_clean.csv")
setDT(exposome_full_clean)

# testing dataset
testing_df = merge(exposome_full_clean, matched_groups)
testing_df = testing_df[group == 2,]


#######################
#### 2. Imputation ####
#######################

# convert  to factor
binary_features = testing_df[, names(which(sapply(.SD,\(x) is.numeric(x) && (max(x, na.rm = T) - min(x, na.rm = T)) == 1)))]
factor_features = c(binary_features, "race", "gender", "race_eth")
testing_df[, (factor_features) := lapply(.SD, as.factor), .SDcols = factor_features]

# impute
NPROCS = detectCores() 
cl <- makeCluster(NPROCS)
registerDoParallel(cl)

set.seed(101)
df_imputed = missForest(testing_df[,.SD, .SDcols = grep("_id|date|_abbr|visit|group|bhssu04", colnames(testing_df), invert = T)], 
                        parallelize = 'forests')
stopCluster(cl)

df_testing_imputed = cbind(testing_df[,.SD, .SDcols = grep("_id|date|sex|bhssu04|race|eth|gender|TRANS", colnames(testing_df))],
                           df_imputed$ximp[,.SD, .SDcols = grep("race|eth|sex|TRANS|gender", colnames(df_imputed$ximp), invert = T)])

df_testing_imputed[, bhssa01a_z := scale(bhssa01a)]
df_testing_imputed[, bhssa03a_z := scale(bhssa03a)]
df_testing_imputed[, (binary_features) := lapply(.SD, \(x) as.numeric(as.character(x))), .SDcols = binary_features]

##########################
#### 3. Calculate ERS ####
##########################
exwas_results = read_csv("outputs/exwas_results.csv")
setDT(exwas_results)
# get exwas features
exposome_cut_off = exwas_results[fdr <= 0.05, c("variable", "coefficients")]


df_testing_imputed[, ers := apply(.SD, 1 , \(r) {
                      return(sum(r*exposome_cut_off$coefficients))
                    }), 
                   .SDcols = exposome_cut_off$variable]


df_testing_imputed[ ,ers_z := scale(ers)]

write.csv(df_testing_imputed, "data/df_testing_imputed.csv", row.names = F)


##### grant data ####
testing_df[,hist(exwas_individual_sum_z, main = "CHOP ED exwas sum score", xlab = "")]
risk_kids = testing_df[,.SD[exwas_individual_sum_z > quantile(exwas_individual_sum_z, 0.9)]]
risk_kids = testing_df[,.SD[exwas_individual_sum_z > 0]]
risk_kids[,table(bhssu04)*100/.N]


p=ggplot(data=testing_df, aes(x=bhssu04, y=exwas_individual_sum_z)) +
  geom_point(alpha = 0.25)+
  geom_smooth(method=lm,  level=0.95)+
  labs(x= "SA", y = "exwas" ) 
  # theme(legend.title=element_blank())
p


testing_df[,mean(exwas_individual_sum_z), by=bhssu04]
##### end grant data



#########################
#### 4. ERS Analysis ####
#########################
covariates = c("age_at_screen", "sex", "race_black" ,"race_white", "ethnicity_new" )
outcome = "bhssu04"


mod1 = glm(paste0(outcome, " ~ ", paste(covariates, collapse = " + ") ), family = binomial, data = df_testing_imputed)
mod2 = glm(paste0(outcome, " ~ ers_z + ", paste(covariates, collapse = " + ") ), family = binomial, data = df_testing_imputed)

t1 = tab_model(mod1, mod2, show.intercept = F, show.ngroups = T, show.aic = T) 
t1

anova(mod1, mod2, test="Chisq")

r2_nagelkerke(mod1)
r2_nagelkerke(mod2)



######################################
#### 5. ERS Analysis by subgroups ####
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
mod2_sex =   glm(paste0(outcome, " ~ ers_z*sex + ", paste(covariates, collapse = " + ") ), family = binomial, data = df_testing_imputed)
mod2_race =   glm(paste0(outcome, " ~ ers_z*race_black + ", paste(covariates, collapse = " + ") ), family = binomial, data = df_testing_imputed)
mod2_race1 =  glm(paste0(outcome, " ~ ers_z*ethnicity_new + ", paste(covariates, collapse = " + ") ), family = binomial, data = df_testing_imputed)
mod2_race2 =  glm(paste0(outcome, " ~ ers_z*race_black + ers_z*ethnicity_new + ", paste(covariates, collapse = " + ") ), family = binomial, data = df_testing_imputed)
mod2_race_sex = glm(paste0(outcome, " ~ ers_z*sex + ers_z*race_black + ers_z*ethnicity_new + ", paste(covariates, collapse = " + ") ), family = binomial, data = df_testing_imputed)
mod2_lgbt =   glm(paste0(outcome, " ~ ers_z+TRANS + ", paste(covariates, collapse = " + ") ), family = binomial, data = df_testing_imputed)
mod2_lgbt1 =  glm(paste0(outcome, " ~ ers_z*TRANS + ", paste(covariates, collapse = " + ") ), family = binomial, data = df_testing_imputed)

tab_model(mod2, mod2_sex, mod2_race, mod2_race1, mod2_race2, mod2_race_sex, mod2_lgbt, mod2_lgbt1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T , digits.rsq = 4)#,
          # file = "outputs/abcd_ers_subgroups_results.doc")


# stratify by lgbt 
mod2_lgbt_0 = glm(paste0(outcome, " ~ ers_z + ", paste(covariates, collapse = " + ") ), family = binomial, data = df_testing_imputed[TRANS == 0,])
mod2_lgbt_1 = glm(paste0(outcome, " ~ ers_z + ", paste(covariates, collapse = " + ") ), family = binomial, data = df_testing_imputed[TRANS == 1,])
tab_model(mod2, mod2_lgbt, mod2_lgbt1,mod2_lgbt_0, mod2_lgbt_1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T , digits.rsq = 4)




#### plots ####
library(ggplot2)
x_main = element_blank()
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




###### abstract
df = testing_df[visit_number == 1, ]
df[, SA_y := bhssu04]

#race
df[race_white_non_hisp == 1, new_race := "non_hispanic_white"]
df[race_black_non_hisp == 1, new_race := "non_hispanic_black"]
df[race_black_non_hisp == 1 & race_white_non_hisp == 1, new_race := NA]
df[ethnicity_new == 1, new_race := "hisp"]

df[, table(new_race, race_white_non_hisp, useNA = "always")]
df[, table(new_race, race_black_non_hisp, useNA = "always")]
df[, table(new_race, ethnicity_new, useNA = "always")]

df[, table( new_race, SA_y, useNA = "always")]

df[, hist(exwas_individual_sum_z)]
df[new_race == "non_hispanic_white" , hist(exwas_individual_sum_z)]
df[new_race == "non_hispanic_black" , hist(exwas_individual_sum_z)]
df[new_race == "hisp", hist(exwas_individual_sum_z)]

df[, mean(SA_y) * 100, by = new_race]

df[, table( new_race, SA_y, useNA = "always")]
df[, chisq.test(new_race,SA_y , correct = F)]


# t test between every 2 groups
df[new_race %in% c("non_hispanic_white" , "non_hispanic_black") , t.test( ers_z ~ new_race )]
df[new_race %in% c("non_hispanic_white" , "non_hispanic_black") , wilcox.test( ers_z ~ new_race )]

df[new_race %in% c("non_hispanic_white" , "hisp") , t.test( exwas_individual_sum_z ~ new_race )]
df[new_race %in% c("non_hispanic_white" , "hisp") , wilcox.test( exwas_individual_sum_z ~ new_race )]

df[new_race %in% c("non_hispanic_black" , "hisp") , t.test( exwas_individual_sum_z ~ new_race )]
df[new_race %in% c("non_hispanic_black" , "hisp") , wilcox.test( exwas_individual_sum_z ~ new_race )]


df[, summary(aov(exwas_individual_sum_z ~ new_race ))]
df[, TukeyHSD(aov(exwas_individual_sum_z ~ new_race ))]
df[, pairwise.t.test(exwas_individual_sum_z , new_race, p.adjust.method = "fdr")]

df[, kruskal.test(exwas_individual_sum_z ~ new_race )]
df[, pairwise.wilcox.test(exwas_individual_sum_z , new_race,  p.adjust.method = "fdr")]


# sex
df[, mean(SA_y) * 100, by = sex_abbr]
df[, chisq.test(sex_abbr, SA_y , correct = F)]

df[, t.test( exwas_individual_sum_z ~ sex_abbr )]
df[, wilcox.test( exwas_individual_sum_z ~ sex_abbr )]


#gender
df1 = merge(df, unique(bhs_main[,c("pat_id","gender")]))
df1 = df1[!is.na(gender),]

ids = df1$pat_id[duplicated(df1$pat_id)]

df1[, gender_e := substr(gender, 1, 1) ]
df1[, gender_e_n := fifelse(gender_e == "T", 1, 0) ]
df1[, TRANS := sum(gender_e_n), by = pat_id]
df1[, TRANS := fifelse(TRANS > 0 ,1, 0)]
View(df1[, .(pat_id, sex_abbr, gender, TRANS)])

df1 = merge(df, unique(df1[,.(pat_id, TRANS)]) , all.x = T)
df1 = df1[!is.na(TRANS),]

df1[, sum(TRANS, na.rm = T) ]
df1[, mean(TRANS, na.rm = T) *100 ]
df1[, mean(SA_y) * 100, by = TRANS]
df1[, chisq.test(TRANS, SA_y , correct = F)]

df1[, t.test( exwas_individual_sum_z ~ TRANS )]
df1[, wilcox.test( exwas_individual_sum_z ~ TRANS )]

mod1 = glmer(paste0("SA_y ~ age_at_screen + exwas_individual_sum_z + new_race1 + (1 | pat_id)"), family = binomial, data = df1, nAGQ = 0)
mod2 = glmer(paste0("SA_y ~ age_at_screen +exwas_individual_sum_z + sex + (1 | pat_id)"), family = binomial, data = df, nAGQ = 0)
mod3 = glmer(paste0("SA_y ~ age_at_screen +exwas_individual_sum_z + TRANS + (1 | pat_id)"), family = binomial, data = df1, nAGQ = 0)
mod4 = glmer(paste0("SA_y ~ age_at_screen +exwas_individual_sum_z * new_race1 +  (1 | pat_id)"), family = binomial, data = df1, nAGQ = 0)
mod5 = glmer(paste0("SA_y ~ age_at_screen +exwas_individual_sum_z * sex  + (1 | pat_id)"), family = binomial, data = df, nAGQ = 0)
mod6 = glmer(paste0("SA_y ~ age_at_screen +exwas_individual_sum_z * TRANS + (1 | pat_id)"), family = binomial, data = df1, nAGQ = 0)

tab_model(mod1, mod2, mod3, mod4, mod5, mod6, show.intercept = F, show.ngroups = T, show.r2 = T) 


mod11 = glm(paste0("SA_y ~ age_at_screen + sex + race_black + ethnicity_new +TRANS"), family = binomial, data = df1)
mod12 = glm(paste0("SA_y ~ age_at_screen + sex + race_black + ethnicity_new +TRANS+ exwas_individual_sum_z"), family = binomial, data = df1)
mod13 = glm(paste0("SA_y ~ age_at_screen + sex*exwas_individual_sum_z + race_black + ethnicity_new +TRANS"), family = binomial, data = df1)
mod14 = glm(paste0("SA_y ~ age_at_screen + sex + race_black*exwas_individual_sum_z + ethnicity_new +TRANS"), family = binomial, data = df1)
mod15 = glm(paste0("SA_y ~ age_at_screen + sex + race_black + ethnicity_new*exwas_individual_sum_z +TRANS"), family = binomial, data = df1)
mod16 = glm(paste0("SA_y ~ age_at_screen + sex + race_black + ethnicity_new +TRANS*exwas_individual_sum_z"), family = binomial, data = df1)
mod17 = glm(paste0("SA_y ~ age_at_screen + (sex + race_black + ethnicity_new +TRANS)*exwas_individual_sum_z"), family = binomial, data = df1)
tab_model(mod11,mod12,mod13,mod14,mod15,mod16,mod17, show.intercept = F, show.ngroups = T, show.r2 = T) 



# temp_bhs = readRDS("data/dataset_individual_full.rds")
temp_bhs = unique(bhs_main[,c("pat_id","gender")])

temp_bhs = temp_bhs[ visit_number == 1 ,]
temp_bhs = temp_bhs[!is.na(gender),]

ids = df1$pat_id[duplicated(df1$pat_id)]

temp_bhs[, gender_e := substr(gender, 1, 1) ]
temp_bhs[, gender_e_n := fifelse(gender_e == "T", 1, 0) ]
temp_bhs[, TRANS := sum(gender_e_n), by = pat_id]
temp_bhs[, TRANS := fifelse(TRANS > 0 ,1, 0)]
View(temp_bhs[, .(pat_id, sex_abbr, gender, TRANS)])

df1 = merge(df, unique(df1[,.(pat_id, TRANS)]) , all.x = T)
df1 = df1[!is.na(TRANS),]

df1[, sum(TRANS, na.rm = T) ]
df1[, mean(TRANS, na.rm = T) *100 ]
