library(readr)
library(plyr)
library(doParallel)
library(janitor)
library(data.table)
library(missForest)

source("config.R")


################################
#### 1. impute testing data ####
################################
individual_level_test_df <- read_csv("data/individual_level_test.csv")
race <- read.csv("data/demo_race.csv")

# convert  to factor
race_b_features = grep("src|^sex|event|inter", colnames(race), invert = T, value = T)
race[,race_b_features] = as.data.frame(lapply(race[,race_b_features], as.factor))

ranges = sapply(individual_level_test_df[,grep("src|^sex|event|inter", colnames(individual_level_test_df), invert = T)], range, na.rm = T)
binary_features = names(which(ranges[2,]-ranges[1,] == 1 )) 
# ordinal_features = grep("screen1[3-4]_y|screentime_sq([3-9]|1[0-1])|physical_activity(1|2|5)_y", colnames(individual_level_test_df), value = T)

individual_level_test_df[,binary_features] = as.data.frame(lapply(individual_level_test_df[,binary_features], as.factor))
# individual_level_test_df[,ordinal_features] = as.data.frame(lapply(individual_level_test_df[,ordinal_features], factor, order = TRUE))


dataset = merge(individual_level_test_df, race)


# impute
NPROCS = detectCores() 
cl <- makeCluster(NPROCS-2)
registerDoParallel(cl)

set.seed(101)
test_baseline = dataset[dataset$eventname == "baseline_year_1_arm_1",]
test_baseline = remove_empty(test_baseline, which = "cols")
test_baseline_imputed = missForest(test_baseline[,grep("src|event|date$|sex$", colnames(test_baseline), invert = T)], 
                                   parallelize = 'forests')
set.seed(101)
test_1_year = dataset[dataset$eventname == "1_year_follow_up_y_arm_1",]
test_1_year = remove_empty(test_1_year, which = "cols")
test_1_year_imputed = missForest(test_1_year[,grep("src|event|date$|sex$", colnames(test_1_year), invert = T)], 
                                 parallelize = 'variables')
set.seed(101)
test_2_year = dataset[dataset$eventname == "2_year_follow_up_y_arm_1",]
test_2_year = remove_empty(test_2_year, which = "cols")
test_2_year_imputed = missForest(test_2_year[,grep("src|event|date$|sex$", colnames(test_2_year), invert = T)], 
                                 parallelize = 'variables')
stopCluster(cl)

test_baseline_imputed = cbind(test_baseline[,grep("src|event|date$|sex$", colnames(test_baseline))], test_baseline_imputed$ximp)
test_1_year_imputed = cbind(test_1_year[,grep("src|event|date$|sex$", colnames(test_1_year))], test_1_year_imputed$ximp)
test_2_year_imputed = cbind(test_2_year[,grep("src|event|date$|sex$", colnames(test_2_year))], test_2_year_imputed$ximp)

dataset_test_imputed = rbind.fill(test_baseline_imputed, test_1_year_imputed, test_2_year_imputed)

# View(as.data.frame(describe(dataset)))
# View(as.data.frame(describe(dataset_test_imputed)))

dataset_test_imputed[,binary_features] = as.data.frame(lapply(dataset_test_imputed[,binary_features], \(x) as.numeric(as.character(x))))
dataset_test_imputed[,race_b_features] = as.data.frame(lapply(dataset_test_imputed[,race_b_features], \(x) as.numeric(as.character(x))))
dataset_test_imputed = scale_features(dataset_test_imputed)


##########################
#### 2. calculate ERS ####
##########################
individual_level_exwas_results <- read_csv("outputs/individual_level_results.csv")
individual_cut_off = individual_level_exwas_results[individual_level_exwas_results$fdr <= 0.05, c("variable", "coefficients")]


# calculate scores
dataset_test_imputed$exwas_individual_sum = apply(dataset_test_imputed[,individual_cut_off$variable], 1 , function(r){
  return(sum(r*individual_cut_off$coefficients, na.rm = T))
})

dataset_test_imputed$exwas_individual_sum_z = scale(dataset_test_imputed$exwas_individual_sum)[,1]



###############################################
#### 3. add FH and PRS to the testing data ####
###############################################
FH_suicide <- read_csv("data/family_history.csv")
suicide_test <- read_csv("data/DV_suicide_test.csv")
genetics <- read_csv(file.path(abcd_genetics_path, "genetic.csv"))
lgbt <- read_csv("data/lgbtqia.csv")

dataset = merge(suicide_test, race, all.x = T)
dataset = merge(dataset, FH_suicide[,c("src_subject_id", "famhx_ss_momdad_scd_p")], all.x = T)
dataset = merge(dataset, lgbt[,c("src_subject_id", "eventname", "LGBT", "LGBT_inclusive")])
dataset = merge(dataset, genetics[,c("src_subject_id", "suicide_PRSice_Pt0_05", "genetic_afr")], all.x = T)
dataset = merge(dataset, dataset_test_imputed[,c("src_subject_id", "eventname", "interview_age", "sex",
                                                     "exwas_individual_sum", "exwas_individual_sum_z")] )
setDT(dataset)
dataset[, race_eth := {
  fcase(
    ethnicity_hisp == 0 & race_black == 1 , "NH-Black",
    ethnicity_hisp == 0 & race_white == 1 , "NH-White",
    ethnicity_hisp == 1 , "Hispanic"
  )
}]
dataset[,table(race_eth, ethnicity_hisp, useNA = "ifany")]

dataset[, time := {
  fcase(
    eventname == "baseline_year_1_arm_1", "baseline",
    eventname == "1_year_follow_up_y_arm_1", "1 year follow up",
    eventname == "2_year_follow_up_y_arm_1", "2 year follow up"
  )
}]


write.csv(file = paste0("data/dataset_ESS.csv"), dataset, row.names = F)




###########################################
#### presentation for NIH
individual_level_train_df <- read_csv("data/individual_level_train.csv")
suicide_train = read_csv("data/DV_suicide_train.csv")

# calculate scores
individual_level_train_df$exwas_individual_sum = apply(individual_level_train_df[,individual_cut_off$variable], 1 , function(r){
  return(sum(r*individual_cut_off$coefficients, na.rm = T))
})

individual_level_train_df$exwas_individual_sum_z = scale(individual_level_train_df$exwas_individual_sum)[,1]
library(plyr)
individual_level = rbind.fill(individual_level_train_df, individual_level_test_df)
suicide = rbind.fill(suicide_train, suicide_test)

dataset_all = merge(suicide, race, all.x = T)
dataset_all = merge(dataset_all, FH_suicide[,c("src_subject_id", "famhx_ss_momdad_scd_p")], all.x = T)
dataset_all = merge(dataset_all, genetics[,c("src_subject_id", "suicide_PRSice_Pt0_05", "genetic_afr")], all.x = T)
dataset_all = merge(dataset_all, individual_level[,c("src_subject_id", "eventname", "interview_age", "sex", 
                                                     "exwas_individual_sum", "exwas_individual_sum_z")] )

library(qgraph)
cor_auto(dataset_all[dataset_all$genetic_afr == 1,c("exwas_individual_sum_z", "suicide_PRSice_Pt0_05")])
cor_auto(dataset[dataset$genetic_afr == 1,c("exwas_individual_sum_z", "suicide_PRSice_Pt0_05")])

cor_auto(dataset_all[dataset_all$genetic_afr == 0,c("exwas_individual_sum_z", "suicide_PRSice_Pt0_05")])
cor_auto(dataset[dataset$genetic_afr == 0,c("exwas_individual_sum_z", "suicide_PRSice_Pt0_05")])


cor.test(dataset_all$exwas_individual_sum_z[dataset_all$genetic_afr == 1], dataset_all$suicide_PRSice_Pt0_05[dataset_all$genetic_afr == 1])
cor.test(dataset$exwas_individual_sum_z[dataset$genetic_afr == 1],dataset$suicide_PRSice_Pt0_05[dataset$genetic_afr == 1])

cor.test(dataset_all$exwas_individual_sum_z[dataset_all$genetic_afr == 0], dataset_all$suicide_PRSice_Pt0_05[dataset_all$genetic_afr == 0])
cor.test(dataset$exwas_individual_sum_z[dataset$genetic_afr == 0],dataset$suicide_PRSice_Pt0_05[dataset$genetic_afr == 0])




# ever
dataset_all$timepoint = sub("_year.*", "", dataset_all$eventname)
dataset_all[, c("interview_age", "interview_date", "eventname")] = NULL

dataset_all_wide = reshape(dataset_all[,c("src_subject_id", "timepoint", "SA_y", "exwas_individual_sum")], direction = "wide", 
                           idvar = c("src_subject_id"), 
                           timevar = "timepoint", sep = "__")

dataset_all_wide$SA_y_ever = apply(dataset_all_wide[,grep("SA", colnames(dataset_all_wide))], 1,
                               function(r){  any( r == 1)*1 })
dataset_all_wide$exwas_ever = apply(dataset_all_wide[,grep("exwas", colnames(dataset_all_wide))], 1,
                                   function(r){  mean( na.exclude(r)) })

dataset_all_wide$exwas_ever_z = scale(dataset_all_wide$exwas_ever)[,1]

### select only kids that have value in SA ever
dataset_all_wide = dataset_all_wide[!is.na(dataset_all_wide$SA_y_ever), c("src_subject_id", "SA_y_ever", "exwas_ever", "exwas_ever_z")]

dataset_all_wide = merge(dataset_all_wide, race )
dataset_all_wide = merge(dataset_all_wide, FH_suicide[,c("src_subject_id", "famhx_ss_momdad_scd_p")], all.x = T)
dataset_all_wide = merge(dataset_all_wide, genetics[,c("src_subject_id", "suicide_PRSice_Pt0_05", "genetic_afr")], all.x = T)


library(lavaan)
run_mediation_models = function(origin){
  set.seed(173)

  
  med_for = paste0(
    "exwas_ever_z ~ a*",origin," \n",
    "SA_y_ever ~ b*exwas_ever_z + c*", origin," \n",
    "indirecteffect := a*b
    totaleffect := c + a*b"
  )
  
  med_mod <- sem(med_for, data = dataset_all_wide[dataset_all_wide$genetic_afr == 0,], se = "bootstrap", bootstrap = 500)
  
  solution = standardizedSolution(med_mod, type = "std.lv")
  summary_res = summary(med_mod)
  
  return(list(solution = solution,  summary = summary_res))
}


race_black_res = run_mediation_models("race_black")
race_white_res =run_mediation_models("race_white")
genetic_afr_res =run_mediation_models("genetic_afr")
sex_res =run_mediation_models("sex_br")

prs_res_afr =run_mediation_models("suicide_PRSice_Pt0_05")
prs_res_eur =run_mediation_models("suicide_PRSice_Pt0_05")


###########################################




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

