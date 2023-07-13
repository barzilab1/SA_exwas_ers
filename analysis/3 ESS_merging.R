library(readr)

source("config.R")

##########################
#### 1. calculate ERS ####
##########################

individual_level_test_df <- read_csv("data/individual_level_test.csv")
individual_level_exwas_results <- read_csv("outputs/individual_level_results.csv")

individual_cut_off = individual_level_exwas_results[individual_level_exwas_results$fdr <= 0.05, c("variable", "coefficients")]

### TODO - should we impute????

# calculate scores
individual_level_test_df$exwas_individual_sum = apply(individual_level_test_df[,individual_cut_off$variable], 1 , function(r){
  return(sum(r*individual_cut_off$Coefficients, na.rm = T))
})

individual_level_test_df$exwas_individual_sum_z = scale(individual_level_test_df$exwas_individual_sum)[,1]



###############################################
#### 2. add FH and PRS to the testing data ####
###############################################
suicide_test <- read_csv("data/DV_suicide_test.csv")
race <- read.csv("data/demo_race.csv")
FH_suicide <- read_csv("data/family_history.csv")
genetics <- read_csv(file.path(abcd_genetics_path, "genetic.csv"))

dataset = merge(suicide_test, race, all.x = T)
dataset = merge(dataset, FH_suicide[,c("src_subject_id", "famhx_ss_momdad_scd_p")], all.x = T)
dataset = merge(dataset, genetics[,c("src_subject_id", "suicide_PRSice_Pt0_05", "genetic_afr")], all.x = T)
dataset = merge(dataset, individual_level_test_df[,c("src_subject_id", "eventname", "interview_age", "sex", "exwas_individual_sum", "exwas_individual_sum_z")] )

write.csv(file = paste0("data/dataset_ESS.csv"), dataset, row.names = F)





###########################################
### Abstract for AFSP
setDT(dataset)
df = dataset

### race
df[non_hispanic_white == 1, new_race := "non_hispanic_white"]
df[non_hispanic_black == 1, new_race := "non_hispanic_black"]
df[non_hispanic_black == 1 & non_hispanic_white == 1, new_race := NA]
df[ethnicity_hisp == 1, new_race := "hisp"]

df[, table(new_race, non_hispanic_white, useNA = "always")]
df[, table(new_race, non_hispanic_black, useNA = "always")]
df[, table(new_race, ethnicity_hisp, useNA = "always")]

df[, table( new_race, SA_y, useNA = "always")]


df[, hist(exwas_individual_sum_z)]
df[new_race == "non_hispanic_white" , hist(exwas_individual_sum_z)]
df[new_race == "non_hispanic_black" , hist(exwas_individual_sum_z)]
df[new_race == "hisp", hist(exwas_individual_sum_z)]

# t test between every 2 groups
df[new_race %in% c("non_hispanic_white" , "non_hispanic_black") , t.test( exwas_individual_sum_z ~ new_race )]
df[new_race %in% c("non_hispanic_white" , "non_hispanic_black") , wilcox.test( exwas_individual_sum_z ~ new_race )]

df[new_race %in% c("non_hispanic_white" , "hisp") , t.test( exwas_individual_sum_z ~ new_race )]
df[new_race %in% c("non_hispanic_white" , "hisp") , wilcox.test( exwas_individual_sum_z ~ new_race )]

df[new_race %in% c("non_hispanic_black" , "hisp") , t.test( exwas_individual_sum_z ~ new_race )]
df[new_race %in% c("non_hispanic_black" , "hisp") , wilcox.test( exwas_individual_sum_z ~ new_race )]


df[, summary(aov(exwas_individual_sum_z ~ new_race ))]
df[, TukeyHSD(aov(exwas_individual_sum_z ~ new_race ))]
df[, pairwise.t.test(exwas_individual_sum_z , new_race, p.adjust.method = "fdr")]

df[, kruskal.test(exwas_individual_sum_z ~ new_race )]
df[, pairwise.wilcox.test(exwas_individual_sum_z , new_race,  p.adjust.method = "fdr")]



### sex
df[, t.test( exwas_individual_sum_z ~ sex )]
df[, wilcox.test( exwas_individual_sum_z ~ sex )]



### LGBT
df1 = merge(df, yksad01)
df1[, t.test( exwas_individual_sum_z ~ TRANS )]
df1[, wilcox.test( exwas_individual_sum_z ~ TRANS )]



df1[, new_race := as.factor(new_race)]
levels(df1$new_race)
df1$new_race1 = relevel(df1$new_race, "non_hispanic_white")

mod1 = glmer(paste0("SA_y ~ interview_age + exwas_individual_sum_z + new_race1 + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df1, nAGQ = 0)
mod2 = glmer(paste0("SA_y ~ interview_age + exwas_individual_sum_z + sex_br + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df, nAGQ = 0)
mod3 = glmer(paste0("SA_y ~ interview_age +exwas_individual_sum_z + LGBT_inclusive + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df1, nAGQ = 0)
mod4 = glmer(paste0("SA_y ~ interview_age +exwas_individual_sum_z * new_race1 +  (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df1, nAGQ = 0)
mod5 = glmer(paste0("SA_y ~ interview_age +exwas_individual_sum_z * sex_br  + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df, nAGQ = 0)
mod6 = glmer(paste0("SA_y ~ interview_age +exwas_individual_sum_z * LGBT_inclusive + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df1, nAGQ = 0)

tab_model(mod1, mod2, mod3, mod4, mod5, mod6, show.intercept = F, show.ngroups = T, show.r2 = T) 


mod11 = glmer(paste0("SA_y ~ interview_age + sex_br + race_black + ethnicity_hisp + TRANS + exwas_individual_sum_z + (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df1, nAGQ = 0)
mod12 = glmer(paste0("SA_y ~ interview_age + sex_br*exwas_individual_sum_z + race_black + ethnicity_hisp + TRANS +  (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df1, nAGQ = 0)
mod13 = glmer(paste0("SA_y ~ interview_age + sex_br + race_black*exwas_individual_sum_z + ethnicity_hisp + TRANS +  (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df1, nAGQ = 0)
mod14 = glmer(paste0("SA_y ~ interview_age + sex_br + race_black + ethnicity_hisp*exwas_individual_sum_z + TRANS +  (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df1, nAGQ = 0)
mod15 = glmer(paste0("SA_y ~ interview_age + sex_br + race_black + ethnicity_hisp + TRANS*exwas_individual_sum_z +  (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df1, nAGQ = 0)
mod16 = glmer(paste0("SA_y ~ interview_age + sex_br + (race_black + ethnicity_hisp + TRANS)*exwas_individual_sum_z +  (1 | site_id_l_br/rel_family_id/src_subject_id)"), family = binomial, data = df1, nAGQ = 0)

tab_model(mod11, mod12, mod13, mod14, mod15, mod16, show.intercept = F, show.ngroups = T, show.r2 = T, file = "abcd.doc") 

