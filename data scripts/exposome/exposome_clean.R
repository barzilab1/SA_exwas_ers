library(psych)
library(plyr)
library(janitor)
library(fastDummies)

source("config.R")
source("utility_fun.R")

########### Discrimination ###########
ydmes01 = load_instrument("abcd_ydmes01",abcd_files_path)
ydmes01[ydmes01 == 777 | ydmes01 == 999] = NA

col_names = grep("dim_matrix", colnames(ydmes01), value = T)
col_names_b = paste0(col_names, "_b")
ydmes01[, col_names_b] = ifelse(ydmes01[, col_names] > 1, 1,0)
ydmes01[, col_names] = NULL

describe(ydmes01)


################### TRAUMA ###################
ptsd01 = load_instrument("abcd_ptsd01",abcd_files_path)
describe(ptsd01)


########### School Risk and Protective Factors ###########
srpf01 = load_instrument("srpf01",abcd_files_path)

col_names = grep("school_", colnames(srpf01), value = T)
col_names_b = paste0(col_names, "_b")
srpf01[, col_names_b] = ifelse(srpf01[, col_names] > 2, 1,0)
srpf01[, col_names] = NULL

describe(srpf01)


########### Youth Family Environment Scale: Family Conflict Subscale ###########
fes01 = load_instrument("abcd_fes01",abcd_files_path)
describe(fes01)


########### Parent Family Environment Scale: Family Conflict Subscale ###########
fes02 = load_instrument("fes02",abcd_files_path)
fes02$fam_enviro_select_language___1 = NULL
describe(fes02)


########### Parental Monitoring Survey ###########
pmq01 = load_instrument("pmq01",abcd_files_path)

col_names = grep("parent_monitor", colnames(pmq01), value = T)
col_names_b = paste0(col_names, "_b")
pmq01[, col_names_b] = ifelse(pmq01[, col_names] >= 4, 1,0)
pmq01[, col_names] = NULL

describe(pmq01)


########### Youth Neighborhood Safety/Crime ###########
nsc01 = load_instrument("abcd_nsc01",abcd_files_path)
nsc01$neighborhood_crime_y_b = ifelse(nsc01$neighborhood_crime_y >= 4 , 1, 0)
nsc01$neighborhood_crime_y = NULL
describe(nsc01)


########### Parent Neighborhood Safety/Crime ###########
pnsc01 = load_instrument("abcd_pnsc01",abcd_files_path)
pnsc01$nei_p_select_language___1 = NULL

col_names = grep("neighborhood", colnames(pnsc01), value = T)
col_names_b = paste0(col_names, "_b")
pnsc01[, col_names_b] = ifelse(pnsc01[, col_names] >= 4, 1,0)
pnsc01[, col_names] = NULL

describe(pnsc01)


########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",abcd_files_path)
yle01$ple_admin = NULL
yle01 = yle01[,!grepl("_past_|_fu(2)?_y$", colnames(yle01))]

describe(yle01[yle01$eventname != "3_year_follow_up_y_arm_1",])


########### Parent Life Events ###########
ple = load_instrument("abcd_ple01",abcd_files_path)
ple$ple_p_select_language___1 = NULL
ple = ple[,!grepl("_past(_|$)|_fu(2)?_p$|_p_p", colnames(ple))]

describe(ple)


########### Parent Community Risk and Protective Factors ###########
crpf = load_instrument("abcd_crpf01",abcd_files_path)

crpf$su_select_language___1 = NULL
crpf[crpf == 999 | crpf == 4] = NA

crpf$su_risk_p_6[crpf$su_risk_p_6 == 2] = NA

col_names = grep("su_risk_p_[1-5,7-8]", colnames(crpf), value = T)
col_names_b = paste0(col_names, "_b")
crpf[, col_names_b] = ifelse(crpf[, col_names] == 0, 1,0)
crpf[, col_names] = NULL

crpf$su_risk_p_9 = crpf$su_risk_p_9 -7
crpf$su_risk_p_9[crpf$su_risk_p_9 == -7] = 0

describe(crpf)


########### Youth Community Risk and Protective Factors ###########
ycrpf = load_instrument("abcd_ycrpf01",abcd_files_path)

ycrpf[ycrpf == 999 ] = NA

col_names = grep("su_crpf_avail_[^69]", colnames(ycrpf), value = T)
col_names_b = paste0(col_names, "_b")
ycrpf[, col_names_b] = ifelse(ycrpf[, col_names] == 1, 1,0)
ycrpf[, col_names] = NULL

describe(ycrpf)


########### Parent PhenX Community Cohesion ###########
pxccp01 = load_instrument("abcd_pxccp01",abcd_files_path)
pxccp01[pxccp01 == 777| pxccp01 == 999] = NA
pxccp01$comc_phenx_select_language = NULL

col_names = grep("comc_phenx", colnames(pxccp01), value = T)
col_names_b = paste0(col_names, "_b")
pxccp01[, col_names_b] = ifelse(pxccp01[, col_names] >= 4, 1,0)
pxccp01[, col_names] = NULL

describe(pxccp01)


########### Developmental History ###########
dhx01 = load_instrument("dhx01",abcd_files_path)

dhx01[dhx01 == 999 | dhx01 == -1] = NA
dhx01$accult_select_language = NULL

#remove empty columns 
dhx01 = dhx01[,colSums(is.na(dhx01)) != nrow(dhx01)]
dhx01$devhx_1_p = NULL

#change the scale
dhx01$devhx_caffeine_11[dhx01$devhx_caffeine_11 == 0] = 4
dhx01$birth_weight_lbs_tot = dhx01$birth_weight_lbs + ifelse(!is.na(dhx01$birth_weight_oz), dhx01$birth_weight_oz/16, 0)
dhx01[,c("birth_weight_lbs","birth_weight_oz")] = NULL

#remove outliers
dhx01$devhx_3_p[dhx01$devhx_3_p > 55] = NA
dhx01$devhx_4_p[dhx01$devhx_4_p > 80] = NA

dhx01$devhx_11_p[dhx01$devhx_11_p > 50] = NA
dhx01$devhx_16_p[dhx01$devhx_16_p > 60] = NA

dhx01$devhx_19a_p[dhx01$devhx_19a_p > 24] = NA
dhx01$devhx_19b_p[dhx01$devhx_19b_p < 3 | dhx01$devhx_19b_p > 60] = NA
dhx01$devhx_19c_p[dhx01$devhx_19c_p < 7 | dhx01$devhx_19c_p > 60] = NA
dhx01$devhx_19d_p[dhx01$devhx_19d_p < 4 | dhx01$devhx_19d_p > 72] = NA

dhx01$devhx_caff_amt_week[dhx01$devhx_caff_amt_week > 24] = NA


#dichotomize features 
dhx01$devhx_mom_age_young = ifelse(dhx01$devhx_3_p < quantile(dhx01$devhx_3_p , .1, na.rm = T), 1, 0)
dhx01$devhx_mom_age_old   = ifelse(dhx01$devhx_3_p >= 35, 1, 0)

dhx01$devhx_caffeine_11_b = ifelse(dhx01$devhx_caffeine_11 == 4, 1, 0)

# dhx01$devhx_prenatal_care_HRP  = ifelse(dhx01$devhx_11_p > quantile(dhx01$devhx_11_p , .9, na.rm = T), 1, 0)
# dhx01$devhx_prenatal_care_low  = ifelse(dhx01$devhx_11_p < 14, 1, 0)

dhx01$devhx_late_motor_development = ifelse(dhx01$devhx_20_p == 5, 1, 0)
dhx01$devhx_late_speech_development = ifelse(dhx01$devhx_21_p == 5, 1, 0)

dhx01$devhx_low_birth_weight = ifelse(dhx01$birth_weight_lbs_tot < (2500/453.6), 1, 0)

# dhx01[,c("devhx_3_p", "devhx_11_p", "devhx_20_p", "devhx_21_p", "birth_weight_lbs_tot")] = NULL
dhx01[,c("devhx_3_p", "devhx_caffeine_11", "devhx_20_p", "devhx_21_p", "birth_weight_lbs_tot")] = NULL

#remove the medications names 
dhx01[,c("devhx_8_rxnorm_med1", "devhx_8_rxnorm_med2", "devhx_8_rxnorm_med3", "devhx_8_other1_name_oth", "devhx_8_other3_name_oth",
         "devhx_9_med1_rxnorm", "devhx_9_med2_rxnorm", "devhx_9_med3_rxnorm", "devhx_9_med4_rxnorm", 
         "devhx_9_med5_rxnorm", "devhx_9_other1_name_oth", "devhx_9_other2_name_oth")] = NULL

# this instrument wil be merged with all timepoints, not only baseline 
dhx01[,c("eventname", "interview_age", "interview_date")] = NULL
# View(as.data.frame(describe(dhx01)))


########### Children's Report of Parental Behavioral Inventory ###########
crpbi = load_instrument("crpbi01",abcd_files_path)
crpbi[,c("crpbi_studycaregiver_id", "crpbi_caregiver1_y", "crpbi_caregiver2_y")] = NULL

col_names = grep("crpbi_", colnames(crpbi), value = T)
col_names_b = paste0(col_names, "_b")
crpbi[, col_names_b] = ifelse(crpbi[col_names] == 3, 1,0)
crpbi[, col_names] = NULL

describe(crpbi)


########### Parental Rules on Substance Use ###########
prq = load_instrument("prq01",abcd_files_path)
prq$pr_select_language___1 = NULL
# prq = prq[,grepl("src|sex|event|interview|_q(1|4|7)$", colnames(prq))]

prq$parent_rules_q3[prq$parent_rules_q3 == 4] = NA
prq$parent_rules_q6[prq$parent_rules_q6 == 4] = NA
prq$parent_rules_q9[prq$parent_rules_q9 == 4] = NA

col_names = grep("_q(1|4|7)$", colnames(prq), value = T)
col_names_b = paste0(col_names, "_b")
prq[, col_names_b] = ifelse(prq[, col_names] > 1, 1,0)
prq[, col_names] = NULL

describe(prq)


########### Parent Acculturation Survey ###########
pacc = load_instrument("pacc01",abcd_files_path)
pacc[pacc == 777 | pacc == 999] = NA
pacc = pacc[,grep("src|sex|event|interview|accult_q[1-2]", colnames(pacc)) ]

pacc$accult_q1_p_b = ifelse(pacc$accult_q1_p < 4, 1,0)
pacc$accult_q1_p = NULL

describe(pacc)


########### Parent Ohio State Traumatic Brain Injury Screen ###########
otbi = load_instrument("abcd_otbi01",abcd_files_path)
otbi$tbi_select_language___1 = NULL

########### Longitudinal Parent Ohio State Traumatic Brain Injury Screen ###########
lpohstbi = load_instrument("abcd_lpohstbi01",abcd_files_path)
lpohstbi$tbi_l_select_language___1 = NULL

describe(lpohstbi)

### combine the 2 instruments 
colnames(lpohstbi) = sub("_l$", "", colnames(lpohstbi))
tbi = rbind.fill(otbi, lpohstbi)
describe(tbi)


########### Cyber Bully ###########
cb = load_instrument("abcd_cb01",abcd_files_path)
cb = cb[,grep("src|sex|event|interview|cybb_phenx_harm$", colnames(cb)) ]

cb[cb == 777 | cb == 999] = NA
describe(cb)


########### Peer Experiences Questionnaire ###########
peq01 = load_instrument("abcd_peq01",abcd_files_path)
peq01 = peq01[,grep("src|sex|event|interview|_vic$", colnames(peq01)) ]

col_names = grep("peq_", colnames(peq01), value = T)
col_names_b = paste0(col_names, "_b")
peq01[, col_names_b] = ifelse(peq01[, col_names] > 1, 1,0)
peq01[, col_names] = NULL

describe(peq01)


########### Youth Peer Behavior Profile ###########
pbp01 = load_instrument("abcd_pbp01",abcd_files_path)
pbp01[pbp01 == 999] = NA

negative = grep("shoplifted|suspended|skip_school", colnames(pbp01), value = T)
negative_b = paste0(negative, "_b")
pbp01[, negative_b] = ifelse(pbp01[,negative] > 1, 1,0)
pbp01[, negative] = NULL

positive = grep("athletes|church|good", colnames(pbp01), value = T)
positive_b = paste0(positive, "_b")
pbp01[, positive_b] = ifelse(pbp01[,positive] >= 4, 1,0)
pbp01[, positive] = NULL

describe(pbp01)


########### Parent Diagnostic Interview for DSM-5 Background Items Full ###########
dibf = load_instrument("dibf01",abcd_files_path)
dibf[dibf == 777 | dibf == 999 | dibf == -1] = NA
dibf$kbi_p_select_language___1 = NULL


########### Parent Diagnostic Interview for DSM-5 Background Items Full ###########
lpksad = load_instrument("abcd_lpksad01",abcd_files_path)
lpksad[lpksad == 777 | lpksad == -1 | lpksad == 999] = NA
lpksad$kbi_l_p_select_language___1 = NULL
lpksad = lpksad[lpksad$eventname != "3_year_follow_up_y_arm_1",]


### combine the 2 instruments
colnames(lpksad) = sub("_l$", "", colnames(lpksad))
colnames(lpksad) = sub("_l___", "___", colnames(lpksad))

ind = which(colnames(lpksad) == "kbi_p_c_det_reason___8")
colnames(lpksad)[ind] = "kbi_p_c_det_reason__8"

ind = which(colnames(lpksad) == "kbipcregfriend_groupopin")
colnames(lpksad)[ind] = "kbi_p_c_reg_friend_group_opin"

pksad = rbind.fill(dibf, lpksad)
pksad = remove_empty(pksad, "cols")

pksad[, c("kbi_p_c_guard___13", "kbi_p_c_guard___0", "kbi_p_conflict_causes___0",  
          "kbi_p_c_det_reason__999", "kbi_p_c_det_reason___999", 
          "kbi_p_c_det_reason__888", "kbi_p_c_det_reason___888")] = NULL


# getting help, suspensions, grades in school - not exposome 
pksad[, grep("kbi_p_c_spec_serv|kbi_p_c_det_susp|kbi_p_c_det_reason|kbi_p_how_well_c_school|kbi_p_grades_in_school|kbi_p_c_drop_in_grades", colnames(pksad))] = NULL
# guy trans
pksad[, grep("kbi_p_c_gay|kbi_p_c_trans", colnames(pksad), )] = NULL
# mental health services
pksad[, grep("kbi_p_c_mh_sa|kbi_p_c_age_services|kbi_p_c_(scheck|mental_health|substance_abuse)|kbi_ss_c_(mental_health|substance_abuse)|kbipcserviceschecklistl", colnames(pksad))] = NULL


#' 1 = Not in School ; 2 = Regular Public School; 3 = Regular Private School ; 4 = Vocational-Technical School; 
# 9 = Charter School ; 5 = Cyber School; 6 = Home School; 7 = Specialized School for Youth with Emotional/Behavioral Problems; 8 = Other/ Otra
pksad = dummy_cols(pksad, select_columns = "kbi_p_c_school_setting", ignore_na = T, remove_selected_columns = T)

pksad$kbi_p_c_best_friend[pksad$kbi_p_c_best_friend == 3] = NA
pksad$kbi_p_c_best_friend[pksad$kbi_p_c_best_friend == 2] = 0

pksad$kbi_p_c_reg_friend_group[pksad$kbi_p_c_reg_friend_group == 3] = NA
pksad$kbi_p_c_reg_friend_group[pksad$kbi_p_c_reg_friend_group == 2] = 0

pksad$kbi_p_c_reg_friend_group_len[pksad$kbi_p_c_reg_friend_group_len == 4] = NA

pksad$kbi_p_c_bully[pksad$kbi_p_c_bully == 2] = 0
# View(as.data.frame(describe(pksad)))


########### Youth Peer Network Health Protective Scaler ###########
pnhps01 = load_instrument("abcd_pnhps01",abcd_files_path)

pnhps01$pnh_substance[pnhps01$pnh_substance == 3] = 1
pnhps01$pnh_help[pnhps01$pnh_help == 2] = 1
pnhps01$pnh_encourage[pnhps01$pnh_encourage == 2] = 1
# pnhps01[,c("pnh_how_much_encourage", "pnh_how_much_help", "pnh_art_involve")] = NULL

describe(pnhps01)


########### Other Resilience ###########
ysr = load_instrument("abcd_ysr01",abcd_files_path)

ysr[,grep("remote|admin|device", colnames(ysr))] = NULL
ysr[ysr == -1 | ysr == "Don't know"] = NA

describe(ysr)


########### Youth Substance Use Attitudes ###########
ysua = load_instrument("abcd_ysua01",abcd_files_path)
ysua[ysua == 999] = NA
ysua[grep("^(ptu|path|phs)",colnames(ysua))] = NULL #these are not exposome. the child thoughts/options 
# describe(ysua)

########### Youth Substance Use Interview ###########
ysu02 = load_instrument("abcd_ysu02",abcd_files_path)
ysu02_peer_deviance = ysu02[,grep("src|sex|eventname|interview|peer", colnames(ysu02))]

### combine the 2 instruments
colnames(ysu02_peer_deviance) = sub("(?<=_[1-9])_[^_]+$", "", colnames(ysu02_peer_deviance), perl = T)
colnames(ysua) = sub("_l$", "", colnames(ysua))
peer_deviance = rbind.fill(ysu02_peer_deviance, ysua)

# dichotomize features
col_names = grep("peer_deviance", colnames(peer_deviance), value = T)
col_names_b = paste0(col_names, "_b")
peer_deviance[, col_names_b] = ifelse(peer_deviance[, col_names] > 0, 1,0)
peer_deviance[, col_names] = NULL

describe(peer_deviance)


########### Occupation Survey Parent ###########
occsp01 = load_instrument("abcd_occsp01",abcd_files_path)
occsp01 = occsp01[, grep("src|event|interview|sex|ocp_.*_acs_(indust|ocp)", colnames(occsp01))]
occsp01[occsp01 == 777 | occsp01 == 999] = NA

describe(occsp01)

columns_to_dummy = grep("ocp", colnames(occsp01), value = T)
occsp01 <- dummy_cols(occsp01, select_columns = columns_to_dummy, ignore_na = T, remove_selected_columns = T)
describe(occsp01)


########### merge all tables ###########
exposome_set = merge(ydmes01, ptsd01, all =T)
exposome_set = merge(exposome_set, srpf01, all = T)
exposome_set = merge(exposome_set, fes01, all =T)
exposome_set = merge(exposome_set, fes02, all =T)
exposome_set = merge(exposome_set, pmq01, all =T)
exposome_set = merge(exposome_set, nsc01, all =T)
exposome_set = merge(exposome_set, pnsc01, all =T)
exposome_set = merge(exposome_set, yle01, all =T)
exposome_set = merge(exposome_set, ple, all =T)
exposome_set = merge(exposome_set, crpf, all =T)
exposome_set = merge(exposome_set, ycrpf, all =T)
exposome_set = merge(exposome_set, pxccp01, all = T)
# exposome_set = merge(exposome_set, meim, all =T)
exposome_set = merge(exposome_set, crpbi, all =T)
# exposome_set = merge(exposome_set, macv, all =T)
exposome_set = merge(exposome_set, prq, all =T)
exposome_set = merge(exposome_set, pacc, all =T)
exposome_set = merge(exposome_set, tbi, all =T)
exposome_set = merge(exposome_set, cb, all =T)
exposome_set = merge(exposome_set, peq01, all =T)
exposome_set = merge(exposome_set, pbp01, all =T)
exposome_set = merge(exposome_set, pksad, all =T)
exposome_set = merge(exposome_set, pnhps01, all =T)
exposome_set = merge(exposome_set, ysr, all =T)
exposome_set = merge(exposome_set, peer_deviance, all =T)
exposome_set = merge(exposome_set, occsp01, all =T)

# remove 3 year follow up and empty columns
exposome_set = exposome_set[exposome_set$eventname != "3_year_follow_up_y_arm_1", ]
exposome_set = remove_empty(exposome_set, "cols")

# add pregnancy/birth/development to all time points 
exposome_set = merge(exposome_set, dhx01, all.x = T)

# exposome_set$sex = NULL
write.csv(exposome_set, "data/exposome_set_item.csv", row.names = F, na = "")




