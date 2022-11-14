library(readr)
library(janitor)
library(qgraph)
library(caret)

source("config.R")
source("utility_fun.R")


### load and merge suicide ####
family_relationship <- read_csv("data/family_relationship.csv")
site <- read_csv("data/site.csv")
suicide <- read_csv("data/suicide_long.csv")

suicide_site = merge(suicide, site)
suicide_site = suicide_site[complete.cases(suicide_site$SA_y) , grep("interview", colnames(suicide_site), invert = T)]
suicide_site = merge(suicide_site, family_relationship[,c("src_subject_id","sex","rel_family_id")])

participants = read.table(file = file.path(abcd_partition_path, "participants.tsv"), sep = '\t', header = TRUE)
participants$src_subject_id = sub("sub-NDAR", "NDAR_", participants$participant_id)
participants$site_id_l_br = as.numeric(sub("site", "", participants$site))
participants = unique(participants[, c("src_subject_id", "matched_group")]) #site_id_l_br


### merge participants ####
dataset_long = merge(suicide_site, participants)

DV_suicide_train = dataset_long[dataset_long$matched_group == 1,]
DV_suicide_test = dataset_long[dataset_long$matched_group == 2,]

write.csv(file = "data/DV_suicide_train.csv", x = DV_suicide_train, row.names=F, na = "")
write.csv(file = "data/DV_suicide_test.csv", x = DV_suicide_test, row.names=F, na = "")



### load and merge exposome data ###
exposome_item <- read_csv("data/exposome_set_item.csv")
exposome_sum <- read_csv("data/exposome_sum_set.csv")
demographics <- read_csv("data/demographics_all.csv")

structural_level <- read_csv("data/geo_data.csv") 

individual_level = merge(demographics, exposome_sum)
individual_level = merge(individual_level, exposome_item)

individual_level = merge(individual_level, suicide_site[,c("src_subject_id", "eventname")])
structural_level = merge(structural_level, suicide_site[,c("src_subject_id", "eventname")])


### remove features with more than 10% missing data ####
remove_cols_with_na = function(df){
  
  removed_variables = vector(length = ncol(df), mode = "integer")
  names(removed_variables) = colnames(df)
  
  for (timepoint in names(table(df$eventname))) {
    sub_dataset = df[df$eventname == timepoint,]
    vari_delete = colnames(sub_dataset)[which( colSums(is.na(sub_dataset)) >= 0.10*nrow(sub_dataset))]
    removed_variables[vari_delete] = removed_variables[vari_delete] + 1
    df[df$eventname == timepoint, vari_delete] = NA
  }

  df = remove_empty(df, which = "cols")

  return(list(df = df,
              removed_variables = removed_variables))
}

individual_level = remove_cols_with_na(individual_level)$df
structural_level = remove_cols_with_na(structural_level)$df


### scale and check corr  ###

#train
individual_level_train = merge(individual_level, DV_suicide_train[,c("src_subject_id", "eventname")])
corr_data = individual_level_train[,grep("src|sex|interview|event", colnames(individual_level_train), invert = T)]
corrs = cor_auto(corr_data)
corr_featuers = findCorrelation(corrs, cutoff = .9, exact = T, names = T, verbose = T) 
individual_level_train[,corr_featuers] = NULL


structural_level_train = merge(structural_level, DV_suicide_train[,c("src_subject_id", "eventname")])
structural_range = sapply(structural_level_train[,grep("reshist", colnames(structural_level_train))], range, na.rm = T)
cols_to_scale = names(which(structural_range[2,]-structural_range[1,] >= 9e03)) #1e04
structural_level_train[,cols_to_scale] = scale(structural_level_train[,cols_to_scale])

corr_data = structural_level_train[,grep("src|sex|interview|event", colnames(structural_level_train), invert = T)]
corrs_s = cor_auto(corr_data)
corr_featuers = findCorrelation(corrs_s, cutoff = .9, exact = T, names = T, verbose = T) 
structural_level_train[,corr_featuers] = NULL


write.csv(file = "data/individual_level_train.csv", x = individual_level_train, row.names=F, na = "")
write.csv(file = "data/structural_level_train.csv", x = structural_level_train, row.names=F, na = "")

#test
individual_level_test = merge(individual_level, DV_suicide_test[,c("src_subject_id", "eventname")])
structural_level_test = merge(structural_level, DV_suicide_test[,c("src_subject_id", "eventname")])
structural_range = sapply(structural_level_test[,grep("reshist", colnames(structural_level_test))], range, na.rm = T)
cols_to_scale = names(which(structural_range[2,]-structural_range[1,] >= 9e03)) #1e04
structural_level_test[,cols_to_scale] = scale(structural_level_test[,cols_to_scale])


write.csv(file = "data/individual_level_test.csv", x = individual_level_test, row.names=F, na = "")
write.csv(file = "data/structural_level_test.csv", x = structural_level_test, row.names=F, na = "")












