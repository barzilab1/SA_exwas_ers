library(janitor)
library(qgraph)
library(psych)
library(caret)
library(missForest)

source("config.R")
source("utility_fun.R")

################################### 
#### 1. load and merge suicide ####
################################### 

family_relationship <- read.csv("data/family_relationship.csv")
site <- read.csv("data/site.csv")
suicide <- read.csv("data/suicide_long.csv")

suicide$interview_date = NULL

suicide_site = merge(suicide, site)
suicide_site = suicide_site[complete.cases(suicide_site$SA_y) ,]
suicide_site = merge(suicide_site, family_relationship[,c("src_subject_id","sex","rel_family_id")])


participants = read.table(file = file.path(abcd_partition_path, "participants.tsv"), sep = '\t', header = TRUE)
participants$src_subject_id = sub("sub-NDAR", "NDAR_", participants$participant_id)
participants = unique(participants[, c("src_subject_id", "matched_group")]) 


# merge participants
suicide_site = merge(suicide_site, participants)

DV_suicide_train = suicide_site[suicide_site$matched_group == 1,]
DV_suicide_test = suicide_site[suicide_site$matched_group == 2,]

write.csv(file = "data/DV_suicide_train.csv", x = DV_suicide_train, row.names=F, na = "")
write.csv(file = "data/DV_suicide_test.csv", x = DV_suicide_test, row.names=F, na = "")


######################################### 
#### 2. load and merge exposome data ####
######################################### 
##TODO add lgbt to data
demographics <- read_csv("data/demographics_all.csv")
exposome_sum <- read_csv("data/exposome_sum_set.csv")
exposome_item <- read_csv("data/exposome_set_item.csv")
lifestyle <- read_csv("data/lifestyle_item.csv")
lgbt <- read_csv("data/lgbtqia.csv")

geo_data <- read.csv("data/geo_data.csv")

# merge all exposome features
individual_level = merge(demographics, exposome_sum)
individual_level = merge(individual_level, exposome_item)
individual_level = merge(individual_level, lifestyle)
individual_level = merge(individual_level, lgbt)

# remove observations with no DV
individual_level = merge(individual_level, suicide_site[,c("src_subject_id", "eventname")])

# remove features with more than 10% missing data 
remove_cols_with_na = function(df){
  
  for (timepoint in unique(df$eventname)) {
    sub_dataset = df[df$eventname == timepoint,]
    vari_delete = colnames(sub_dataset)[which( colSums(is.na(sub_dataset)) >= 0.10*nrow(sub_dataset))]
    df[df$eventname == timepoint, vari_delete] = NA
  }

  df = remove_empty(df, which = "cols")

  return(df)
}

individual_level = remove_cols_with_na(individual_level)

# remove columns with sd = 0
# zero_sd_cols = sapply(individual_level, \(x) is.numeric(x) && sd(x, na.rm = T) == 0)
# individual_level = individual_level[, !zero_sd_cols]


individual_level = remove_low_signal_cols(individual_level)



# create training subsample
individual_level_train = merge(individual_level, DV_suicide_train[,c("src_subject_id", "eventname")])

# clean outliers 
cols_range = sapply(individual_level_train[,grep("src|^sex|event|inter", colnames(individual_level_train), invert = T)], range, na.rm = T)
cols_to_check_outliers = names(which(cols_range[2,]-cols_range[1,] >= 6)) 




individual_level_train = remove_outliers(cols_to_check_outliers, individual_level_train)
individual_level_train = remove_low_signal_cols(individual_level_train)



# check correlation and remove above 0.9
corr_data = individual_level_train[,grep("src|^sex|interview|event", colnames(individual_level_train), invert = T)]
corrs = cor_auto(corr_data, ordinalLevelMax=8)
saveRDS(corrs, file = "outputs/corrs_data_all.rds")
corr_featuers = findCorrelation(corrs, cutoff = .9, exact = T, names = T, verbose = T) 
individual_level_train[,corr_featuers] = NULL


View(as.data.frame(describe(individual_level_train)))

individual_level_train = scale_features(individual_level_train)

write.csv(file = "data/individual_level_train.csv", x = individual_level_train, row.names=F, na = "")





# create testing subsample
individual_level_test = merge(individual_level, DV_suicide_test[,c("src_subject_id", "eventname")])

# clean outliers 
cols_range = sapply(individual_level_test[,grep("src|^sex|event|inter", colnames(individual_level_test), invert = T)], range, na.rm = T)
cols_to_check_outliers_test = names(which(cols_range[2,]-cols_range[1,] >= 6)) 

individual_level_test = remove_outliers(cols_to_check_outliers_test, individual_level_test)
# individual_level_test = remove_low_signal_cols(individual_level_test)


write.csv(file = "data/individual_level_test.csv", x = individual_level_test, row.names=F, na = "")







