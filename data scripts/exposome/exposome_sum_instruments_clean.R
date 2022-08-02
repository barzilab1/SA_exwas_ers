
source("config.R")
source("utility_fun.R")

########### Sum Scores Culture & Environment Youth ########### 
sscey01 = load_instrument("abcd_sscey01",abcd_files_path)

# sscey01 = sscey01[, grepl("^(src|interview|event|sex|pmq|fes|crpbi|srpf|dim)", colnames(sscey01))]

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscey01 = sscey01[,!grepl("_(nm|nt|na|answered)$",colnames(sscey01))] #pr


# sscey01$school_protective_factors = as.numeric(as.character(sscey01$srpf_y_ss_ses)) + as.numeric(as.character(sscey01$srpf_y_ss_iiss))

summary(droplevels(sscey01))


########### Sum Scores Culture & Environment Parent ########### 
sscep = load_instrument("abcd_sscep01",abcd_files_path)

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscep = sscep[,!grepl("_(nm|nt|na|answered)$",colnames(sscep))]


########### Sum Scores Mobil Tech Youth ########### 
ssmty = load_instrument("abcd_ssmty01",abcd_files_path)

ssmty = ssmty[, grepl("(src|interview|event|sex)|_(weekend|weekday)$", colnames(ssmty))]

summary(ssmty)


########### Parent Adult Self Report Scores Aseba (ASR) ########### 
asrs = load_instrument("abcd_asrs01",abcd_files_path)
summary(droplevels(asrs[asrs$eventname == "baseline_year_1_arm_1",]))


########### Summary Scores Developmental History ########### 
devhxss = load_instrument("abcd_devhxss01",abcd_files_path)

devhxss[devhxss == 999] = NA
summary(droplevels(devhxss))


########### family history ########### 
fhx.1 = load_instrument("fhxp102",abcd_files_path)
fhx.2 = load_instrument("fhxp201",abcd_files_path)

fh_dataset = merge(fhx.1, fhx.2)
fh_dataset = fh_dataset[, grep("^(src|interview|event|sex)|famhx_1$|famhx_4_p$|^fam_history_([5-9]|1[0-3])_yes_no", colnames(fh_dataset))]

fh_dataset[fh_dataset == 7|fh_dataset == 999] = NA
summary(fh_dataset)

###rename variables to meaningful names
colnames(fh_dataset)[7] = "FH_ALCOHOL"
colnames(fh_dataset)[8] = "FH_DRUGS"
colnames(fh_dataset)[9] = "FH_DEPRESSION"
colnames(fh_dataset)[10] = "FH_BIPOLAR"
colnames(fh_dataset)[11] = "FH_PSYCHOSIS"
colnames(fh_dataset)[12] = "FH_EXTERNALIZING"
colnames(fh_dataset)[13] = "FH_NERVES"
colnames(fh_dataset)[14] = "FH_MENTAL_HEALTH_PROFFESSIONAL"
colnames(fh_dataset)[15] = "FH_PSYCHIATRIC_HOSPITALIZATION"
colnames(fh_dataset)[16] = "FH_SUICIDALITY"


########### Longitudinal Summary Scores Sports Activity ########### 
lsssa = load_instrument("abcd_lsssa01",abcd_files_path)
lsssa[lsssa == 999] = NA

#remove empty columns
lsssa = lsssa[,colSums(is.na(lsssa)) != dim(lsssa)[1]]
summary(droplevels(lsssa))


########### Sum Scores Traumatic Brain Injury ########### 
tbi01 = load_instrument("abcd_tbi01",abcd_files_path)
tbi01 = unique(tbi01)

tbi01 = tbi01[, !grepl("_nm$", colnames(tbi01))]
tbi01 = tbi01[,colSums(is.na(tbi01)) != dim(tbi01)[1]]


summary(droplevels(tbi01))


########### Summary Scores Brief Problem Monitor-Teacher Form for Ages 6-18 ########### 
ssbpmtf = load_instrument("abcd_ssbpmtf01",abcd_files_path)
ssbpmtf = ssbpmtf[, !grepl("_(nm|nt)$", colnames(ssbpmtf))]






########### merge all tables
exposome_set = merge(ssmty,sscey01)

# write.csv(file = "outputs/exposome_sum_set.csv",x = exposome_set, row.names = F, na = "")




