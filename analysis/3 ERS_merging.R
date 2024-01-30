library(readr)
library(plyr)
library(doParallel)
library(janitor)
library(data.table)
library(missForest)

source("config.R")
source("utility_fun.R")


################################
#### 1. impute testing data ####
################################
exposome_df_test <- read_csv("data/exposome_df_test.csv")
race <- read.csv("data/demo_race.csv")


# convert to factor
race_b_features = grep("src|^sex|event|inter", colnames(race), invert = T, value = T)
race[,race_b_features] = as.data.frame(lapply(race[,race_b_features], as.factor))

binary_features = names(which(sapply(exposome_df_test[,grep("src|^sex|event|inter", colnames(exposome_df_test), invert = T)], 
                function(col){
                  unique_values <- unique(na.exclude(col));
                  return(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))
                })))

exposome_df_test[,binary_features] = as.data.frame(lapply(exposome_df_test[,binary_features], as.factor))
dataset = merge(exposome_df_test, race)


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
                                 parallelize = 'forests')
set.seed(101)
test_2_year = dataset[dataset$eventname == "2_year_follow_up_y_arm_1",]
test_2_year = remove_empty(test_2_year, which = "cols")
test_2_year_imputed = missForest(test_2_year[,grep("src|event|date$|sex$", colnames(test_2_year), invert = T)], 
                                 parallelize = 'forests')
stopCluster(cl)

test_baseline_imputed_merged = cbind(test_baseline[,grep("src|event|date$|sex$", colnames(test_baseline))], test_baseline_imputed$ximp)
test_1_year_imputed_merged = cbind(test_1_year[,grep("src|event|date$|sex$", colnames(test_1_year))], test_1_year_imputed$ximp)
test_2_year_imputed_merged = cbind(test_2_year[,grep("src|event|date$|sex$", colnames(test_2_year))], test_2_year_imputed$ximp)

dataset_test_imputed = rbind.fill(test_baseline_imputed_merged, test_1_year_imputed_merged, test_2_year_imputed_merged)

# View(as.data.frame(describe(dataset)))
# View(as.data.frame(describe(dataset_test_imputed)))

dataset_test_imputed[,binary_features] = as.data.frame(lapply(dataset_test_imputed[,binary_features], \(x) as.numeric(as.character(x))))
dataset_test_imputed[,race_b_features] = as.data.frame(lapply(dataset_test_imputed[,race_b_features], \(x) as.numeric(as.character(x))))
dataset_test_imputed = scale_features(dataset_test_imputed)

saveRDS(dataset_test_imputed, "outputs/dataset_test_imputed.RDS" )


##########################
#### 2. calculate ERS ####
##########################
exwas_results <- read_csv("outputs/exwas_results.csv")
cut_off = exwas_results[exwas_results$fdr <= 0.05, c("variable", "coefficients")] # 99

# calculate scores
dataset_test_imputed$ers = apply(dataset_test_imputed[,cut_off$variable], 1 , function(r){
  return(sum(r*cut_off$coefficients, na.rm = T))
})
dataset_test_imputed$ers_z = scale(dataset_test_imputed$ers)[,1]


cut_off_sensitivity = exwas_results[(exwas_results$or <= 1/1.2 & exwas_results$or > 0 )| exwas_results$or >= 1.2 , c("variable", "coefficients")] #300

dataset_test_imputed$ers_sensitivity = apply(dataset_test_imputed[,cut_off_sensitivity$variable], 1 , function(r){
  return(sum(r*cut_off_sensitivity$coefficients, na.rm = T))
})
dataset_test_imputed$ers_sensitivity_z = scale(dataset_test_imputed$ers_sensitivity)[,1]



###############################################
#### 3. add FH and PRS to the testing data ####
###############################################
FH_suicide <- read_csv("data/family_history.csv")
suicide_test <- read_csv("data/DV_suicide_test.csv")
lgbt <- read_csv("data/lgbtqia.csv")
cbcl = read_csv("data/cbcl.csv")
  
  
dataset = merge(suicide_test, race, all.x = T)
dataset = merge(dataset, FH_suicide[,c("src_subject_id", "famhx_ss_momdad_scd_p")], all.x = T)
dataset = merge(dataset, lgbt[,c("src_subject_id", "eventname", "LGBT", "LGBT_inclusive")])
# dataset = merge(dataset, genetics[,c("src_subject_id", "suicide_PRSice_Pt0_05", "genetic_afr")], all.x = T)
dataset = merge(dataset, dataset_test_imputed[,c("src_subject_id", "eventname", "interview_age", "sex",
                                                     "ers", "ers_z", "ers_sensitivity", "ers_sensitivity_z")] )
dataset = merge(dataset, cbcl[,c("src_subject_id", "eventname", "interview_age", "sex", 
                                  "cbcl_scr_syn_totprob_t", "cbcl_scr_syn_external_t")] )

setDT(dataset)
dataset[, race_eth := {
  fcase(
    ethnicity_hisp == 0 & race_black == 1 , "NH-Black",
    ethnicity_hisp == 0 & race_white == 1 , "NH-White",
    ethnicity_hisp == 1 , "Hispanic",
    default = NA
  )
}]
dataset[,table(race_eth, ethnicity_hisp, useNA = "ifany")]


write.csv(file = "data/dataset_ERS.csv", dataset, row.names = F, na = "")


