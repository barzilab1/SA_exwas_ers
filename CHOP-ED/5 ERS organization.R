library(data.table)
library(doParallel)
library(missForest)
library(performance)
library(sjPlot)
library(readr)
library(ggpubr)

setwd("~/sa_exwas")

testing_df = read_csv("data/testing_df.csv")
setDT(testing_df)

#######################
#### 1. Imputation ####
#######################

# convert  to factor
binary_features = testing_df[, names(which(sapply(.SD, function(col){
  unique_values <- unique(na.exclude(col));
  return(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))
})))]

factor_features = c(binary_features, "race", "gender_new", "race_eth")
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
#### 2. Calculate ERS ####
##########################
exwas_results = read_csv("outputs/exwas_results.csv") #18 features 
setDT(exwas_results)
# get exwas features
exposome_cut_off = exwas_results[fdr <= 0.05, c("variable", "coefficients")] # 15 features 


df_testing_imputed[, ers := apply(.SD, 1 , \(r) {
                      return(sum(r*exposome_cut_off$coefficients))
                    }), 
                   .SDcols = exposome_cut_off$variable]


df_testing_imputed[ ,ers_z := scale(ers)]

write.csv(df_testing_imputed, "data/df_testing_imputed.csv", row.names = F, na = "")

