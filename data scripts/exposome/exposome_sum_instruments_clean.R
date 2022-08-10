library(psych)

source("config.R")
source("utility_fun.R")

########### Sum Scores Culture & Environment Youth ########### 
sscey01 = load_instrument("abcd_sscey01",abcd_files_path)

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscey01 = sscey01[,!grepl("_(nm|nt|na|answered)$",colnames(sscey01))]
# remove not exposome measurements: 
sscey01 = sscey01[,!grepl("^(psb|wps)_",colnames(sscey01))]

describe(sscey01)


########### Sum Scores Culture & Environment Parent ########### 
sscep = load_instrument("abcd_sscep01",abcd_files_path)

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscep = sscep[,!grepl("_(nm|nt|na|answered)$",colnames(sscep))]
# remove not exposome measurements: 
sscep = sscep[,!grepl("^psb_",colnames(sscep))]

describe(sscep)


########### Sum Scores Mobil Tech Youth ########### 
ssmty = load_instrument("abcd_ssmty01",abcd_files_path)
ssmty = ssmty[, !grepl("_(nm|nt)$", colnames(ssmty))]
summary(ssmty)


########### Parent Adult Self Report Scores Aseba (ASR) ########### 
asrs = load_instrument("abcd_asrs01",abcd_files_path)
asrs = asrs[, grepl("src|event|interview|sex|_t$", colnames(asrs))]


########### Summary Scores Developmental History ########### 
devhxss = load_instrument("abcd_devhxss01",abcd_files_path)

devhxss[devhxss == 999] = NA
describe(devhxss)


########### Sum Scores Traumatic Brain Injury ########### 
tbi01 = load_instrument("abcd_tbi01",abcd_files_path)

tbi01 = tbi01[, !grepl("_nm$", colnames(tbi01))]
tbi01 = tbi01[tbi01$eventname == "baseline_year_1_arm_1",]
# tbi01 = tbi01[, colSums(is.na(tbi01)) < nrow(tbi01)/5]


########### Longitudinal Sum Scores Traumatic Brain Injury ########### 
lsstbi01 = load_instrument("abcd_lsstbi01",abcd_files_path)
lsstbi01 = lsstbi01[, !grepl("_nm_l$", colnames(lsstbi01))]


########### Summary Scores Brief Problem Monitor-Teacher Form for Ages 6-18 ########### 
# a lot of missing data
ssbpmtf = load_instrument("abcd_ssbpmtf01",abcd_files_path)
ssbpmtf = ssbpmtf[, grepl("src|sex|event|interview|_t$", colnames(ssbpmtf))]


########### Summary Scores Sports Activity ########### 
spacss= load_instrument("abcd_spacss01",abcd_files_path)
spacss[spacss == 999] = NA
spacss = spacss[,grep("src|sex|event|interview|_nmonth_", colnames(spacss), value = T)]


########### Longitudinal Summary Scores Sports Activity ########### 
lsssa = load_instrument("abcd_lsssa01",abcd_files_path)
lsssa[lsssa == 999] = NA
lsssa = lsssa[,grep("src|sex|event|interview|_nmonth_", colnames(lsssa), value = T)]


########## ABCD Sum Scores Physical Health Parent ###########
ssphp01 = load_instrument("abcd_ssphp01", abcd_files_path)

#select variables
ssphp01 = ssphp01[,grepl("src|event|interview|sex|cna.*_sum$",colnames(ssphp01))]


########### merge all tables ###########
exposome_set = merge(sscey01, sscep, all = T)
exposome_set = merge(exposome_set, ssmty, all = T)
exposome_set = merge(exposome_set, asrs, all = T)
exposome_set = merge(exposome_set, devhxss, all = T)
exposome_set = merge(exposome_set, tbi01, all = T)
exposome_set = merge(exposome_set, lsstbi01, all = T)
exposome_set = merge(exposome_set, spacss, all = T)
exposome_set = merge(exposome_set, lsssa, all = T)
exposome_set = merge(exposome_set, ssphp01, all = T)

exposome_set = exposome_set[grep("^(1|2|baseline)",exposome_set$eventname),]
exposome_set = exposome_set[, colSums(is.na(exposome_set)) != nrow(exposome_set)]

write.csv(file = "outputs/exposome_sum_set.csv",x = exposome_set, row.names = F, na = "")


#ssbpmtf
