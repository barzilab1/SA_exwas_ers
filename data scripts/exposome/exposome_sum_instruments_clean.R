library(psych)
library(plyr)

source("config.R")
source("utility_fun.R")

########### Sum Scores Culture & Environment Youth ########### 
sscey01 = load_instrument("abcd_sscey01",abcd_files_path)

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscey01 = sscey01[,!grepl("_(nm|nt|na|answered|pr)$",colnames(sscey01))]
# remove not exposome measurements: 
sscey01 = sscey01[,!grepl("^(psb|wps|macv)_",colnames(sscey01))]

describe(sscey01)
sscey01_wide = get_wide_data(sscey01)


########### Sum Scores Culture & Environment Parent ########### 
sscep = load_instrument("abcd_sscep01",abcd_files_path)

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscep = sscep[,!grepl("^psb|_(nm|nt|na|answered|pr)$",colnames(sscep))]

describe(sscep)
sscep_wide = get_wide_data(sscep)
sscep_wide = sscep_wide[,grep("src|sex|nsc|macv|meim", colnames(sscep_wide))]


########### Sum Scores Mobil Tech Youth ########### 
ssmty = load_instrument("abcd_ssmty01",abcd_files_path)
ssmty = ssmty[, !grepl("_(nm|nt)$", colnames(ssmty))]
summary(ssmty)
ssmty_wide = get_wide_data(ssmty)


########### Summary Scores Developmental History ########### 
devhxss = load_instrument("abcd_devhxss01",abcd_files_path)

devhxss[devhxss == 999] = NA
describe(devhxss)
devhxss_wide = get_wide_data(devhxss)


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
tbi_wide = get_wide_data(tbi, "tbi_ss_worst_overall")


########### Summary Scores Sports Activity ########### 
spacss= load_instrument("abcd_spacss01",abcd_files_path)
spacss[spacss == 999] = NA
spacss = spacss[,grep("src|sex|event|interview|_nmonth_", colnames(spacss), value = T)]


########### Longitudinal Summary Scores Sports Activity ########### 
lsssa = load_instrument("abcd_lsssa01",abcd_files_path)
lsssa[lsssa == 999] = NA
lsssa = lsssa[,grep("src|sex|event|interview|_nmonth_", colnames(lsssa), value = T)]


### combine the 2 instruments 
colnames(lsssa) = sub("_l", "", colnames(lsssa))
sssa = rbind.fill(spacss, lsssa)
describe(sssa)
sssa_wide = get_wide_data(sssa)


########## ABCD Sum Scores Physical Health Parent ###########
ssphp01 = load_instrument("abcd_ssphp01", abcd_files_path)

#select variables
ssphp01 = ssphp01[,grepl("src|event|interview|sex|cna.*_sum$",colnames(ssphp01))]
ssphp01_wide = get_wide_data(ssphp01)


################### Sum Scores Mental Health Youth ################### 
mhy = load_instrument("abcd_mhy02", abcd_files_path)

mhy = mhy[,grepl("^(src|interview|event|sex|ple|peq)",colnames(mhy))]
mhy = mhy[,!grepl("_(nm|nt)$",colnames(mhy))]

mhy$bully_vic = rowSums(mhy[ ,c("peq_ss_relational_victim", "peq_ss_reputation_victim", "peq_ss_overt_victim")])
mhy$bully_aggs = rowSums(mhy[ ,c("peq_ss_relational_aggs", "peq_ss_reputation_aggs", "peq_ss_overt_aggression")])

mhy[mhy$eventname == "baseline_year_1_arm_1", grep("ple", colnames(mhy))] = NA

describe(mhy)
mhy_wide = get_wide_data(mhy)



################### Sum Scores Mental Health Parent ################### 
mhp02 = load_instrument("abcd_mhp02", abcd_files_path)
mhp02 = mhp02[,grepl("^src|interview|event|sex|ple",colnames(mhp02))]
mhp02 = mhp02[,!grepl("_(nm|nt)$",colnames(mhp02))]
mhp02[mhp02$eventname == "baseline_year_1_arm_1", grep("ple", colnames(mhp02))] = NA
mhp02_wide = get_wide_data(mhp02)


################### Summary Scores Substance Use ################### 
suss = load_instrument("abcd_suss01", abcd_files_path)
suss = suss[,!grepl("_(nt|nm)((_l)?)$",colnames(suss))]

describe(suss)
suss_wide = get_wide_data(suss)





########### merge all tables ###########
exposome_set = merge(sscep_wide, ssmty_wide ,all = T)
exposome_set = merge(exposome_set, tbi_wide ,all = T)
exposome_set = merge(exposome_set, ssphp01_wide ,all = T)



write.csv(file = "data/exposome_sum_set.csv",x = exposome_set, row.names = F, na = "")

