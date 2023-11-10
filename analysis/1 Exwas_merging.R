library(janitor)
library(qgraph)
library(psych)
library(caret)
# library(missForest)

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
demographics <- read_csv("data/demographics_all.csv")
exposome_sum <- read_csv("data/exposome_sum_set.csv")
exposome_item <- read.csv("data/exposome_set_item.csv")
lifestyle <- read_csv("data/lifestyle_item.csv")


# merge all exposome features
exposome_df = merge(demographics, exposome_sum)
exposome_df = merge(exposome_df, exposome_item)
exposome_df = merge(exposome_df, lifestyle) 
# total of 1156-6 = 1150 features in abcd

# remove observations with no DV
exposome_df = merge(exposome_df, suicide_site[,c("src_subject_id", "eventname")])

exposome_df = remove_cols_with_na(exposome_df) #657


############################### 
#### 3. create sub cohorts ####
############################### 

# create training cohort
exposome_df_train = merge(exposome_df, DV_suicide_train[,c("src_subject_id", "eventname")])

# clean outliers 
exposome_df_train = remove_outliers(exposome_df_train)
exposome_df_train = remove_low_signal_cols(exposome_df_train)


# check correlation and remove above 0.9
corr_data = exposome_df_train[,grep("src|^sex|interview|event", colnames(exposome_df_train), invert = T)]
corrs = cor_auto(corr_data, ordinalLevelMax=8)
saveRDS(corrs, file = "outputs/corrs_data_all.rds")
corr_featuers = findCorrelation(corrs, cutoff = .9, exact = T, names = T, verbose = T) 
exposome_df_train[,corr_featuers] = NULL #452-6 =446

View(as.data.frame(describe(exposome_df_train)))

# scale features
exposome_df_train = scale_features(exposome_df_train)

write.csv(file = "data/exposome_df_train.csv", x = exposome_df_train, row.names=F, na = "")


### create testing cohort
exposome_df_test = merge(exposome_df, DV_suicide_test[,c("src_subject_id", "eventname")])

# clean outliers 
exposome_df_test = remove_outliers(exposome_df_test)
# don't remove low signal in testing. remove only no variance as in any case they don't change the ers but have impact on the imputation 
zero_sd_cols = sapply(exposome_df_test, \(x) is.numeric(x) && sd(x, na.rm = T) == 0)
exposome_df_test = exposome_df_test[,!zero_sd_cols]

write.csv(file = "data/exposome_df_test.csv", x = exposome_df_test, row.names=F, na = "")







