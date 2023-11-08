library(psych)
library(plyr)

source("config.R")
source("utility_fun.R")



########### Sum Scores Culture & Environment Parent ########### 
sscep = load_instrument("abcd_sscep01",abcd_files_path)

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscep = sscep[,!grepl("^psb|_(nm|nt|na|answered|pr)$",colnames(sscep))]
sscep = sscep[,grep("src|sex|event|interview|macv|meim", colnames(sscep))]
describe(sscep)



########### Sum Scores Traumatic Brain Injury ########### 
tbi01 = load_instrument("abcd_tbi01",abcd_files_path)
tbi01 = tbi01[, !grepl("_nm$", colnames(tbi01))]

########### Longitudinal Sum Scores Traumatic Brain Injury ########### 
lsstbi01 = load_instrument("abcd_lsstbi01",abcd_files_path)
lsstbi01 = lsstbi01[, !grepl("_nm_l$", colnames(lsstbi01))]

### combine the 2 instruments 
colnames(lsstbi01) = sub("_l", "", colnames(lsstbi01))
tbi = rbind.fill(tbi01, lsstbi01)
describe(tbi)

tbi$tbi_ss_nmrpi_b = ifelse(tbi$tbi_ss_nmrpi > 0 , 1, 0)
tbi$tbi_ss_worst_overall_b = ifelse(tbi$tbi_ss_worst_overall  > 1 , 1, 0)
tbi[,c("tbi_ss_nmrpi", "tbi_ss_worst_overall")] = NULL



########### merge all tables ###########
exposome_set = merge(sscep, tbi)

# remove 3 year follow up and empty columns
exposome_set = exposome_set[exposome_set$eventname != "3_year_follow_up_y_arm_1", ]

describe(exposome_set)

write.csv(file = "data/exposome_sum_set.csv",x = exposome_set, row.names = F, na = "")

