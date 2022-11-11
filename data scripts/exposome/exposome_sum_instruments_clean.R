library(psych)
library(plyr)

source("config.R")
source("utility_fun.R")

########### Sum Scores Culture & Environment Youth ########### 
# sscey01 = load_instrument("abcd_sscey01",abcd_files_path)
# 
# #remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
# sscey01 = sscey01[,!grepl("_(nm|nt|na|answered|pr)$",colnames(sscey01))]
# # remove not exposome measurements: 
# sscey01 = sscey01[,!grepl("^(psb|wps|macv)_",colnames(sscey01))]
# 
# describe(sscey01)


########### Sum Scores Culture & Environment Parent ########### 
sscep = load_instrument("abcd_sscep01",abcd_files_path)

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscep = sscep[,!grepl("^psb|_(nm|nt|na|answered|pr)$",colnames(sscep))]
sscep = sscep[,grep("src|sex|event|interview|macv|meim", colnames(sscep))]
describe(sscep)


########### Summary Scores Developmental History ########### 
# devhxss = load_instrument("abcd_devhxss01",abcd_files_path)
# 
# devhxss[devhxss == 999] = NA
# describe(devhxss)
# devhxss_wide = get_wide_data(devhxss)


########### Sum Scores Traumatic Brain Injury ########### 
tbi01 = load_instrument("abcd_tbi01",abcd_files_path)
tbi01 = tbi01[, !grepl("_nm$", colnames(tbi01))]

########### Longitudinal Sum Scores Traumatic Brain Injury ########### 
lsstbi01 = load_instrument("abcd_lsstbi01",abcd_files_path)
lsstbi01 = lsstbi01[, !grepl("_nm_l$", colnames(lsstbi01))]

### combine the 2 instruments 
colnames(lsstbi01) = sub("_l", "", colnames(lsstbi01))
tbi = rbind.fill(tbi01, lsstbi01)
tbi01$tbi_ss_worst1[is.na(tbi01$tbi_ss_worst1)] = 0
describe(tbi)


################### Sum Scores Mental Health Youth ################### 
# mhy = load_instrument("abcd_mhy02", abcd_files_path)
# 
# mhy = mhy[,grepl("^(src|interview|event|sex|ple|peq)",colnames(mhy))]
# mhy = mhy[,!grepl("_(nm|nt)$",colnames(mhy))]
# 
# mhy$bully_vic = rowSums(mhy[ ,c("peq_ss_relational_victim", "peq_ss_reputation_victim", "peq_ss_overt_victim")])
# mhy$bully_aggs = rowSums(mhy[ ,c("peq_ss_relational_aggs", "peq_ss_reputation_aggs", "peq_ss_overt_aggression")])
# 
# mhy[mhy$eventname == "baseline_year_1_arm_1", grep("ple", colnames(mhy))] = NA
# 
# describe(mhy)


################### Sum Scores Mental Health Parent ################### 
# mhp02 = load_instrument("abcd_mhp02", abcd_files_path)
# mhp02 = mhp02[,grepl("^src|interview|event|sex|ple",colnames(mhp02))]
# mhp02 = mhp02[,!grepl("_(nm|nt)$",colnames(mhp02))]
# mhp02[mhp02$eventname == "baseline_year_1_arm_1", grep("ple", colnames(mhp02))] = NA



########### merge all tables ###########
exposome_set = merge(sscep, tbi)

# remove 3 year follow up and empty columns
exposome_set = exposome_set[exposome_set$eventname != "3_year_follow_up_y_arm_1", ]

describe(exposome_set)

write.csv(file = "data/exposome_sum_set.csv",x = exposome_set, row.names = F, na = "")

