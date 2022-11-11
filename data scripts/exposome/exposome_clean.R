library(psych)
library(plyr)

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
pmq01[, col_names_b] = ifelse(pmq01[, col_names] == 5, 1,0)
pmq01[, col_names] = NULL

describe(pmq01)


########### Youth Neighborhood Safety/Crime ###########
nsc01 = load_instrument("abcd_nsc01",abcd_files_path)
nsc01$neighborhood_crime_y_b = ifelse(nsc01$neighborhood_crime_y == 5, 1, 0)
nsc01$neighborhood_crime_y = NULL
describe(nsc01)


########### Parent Neighborhood Safety/Crime ###########
pnsc01 = load_instrument("abcd_pnsc01",abcd_files_path)
pnsc01$nei_p_select_language___1 = NULL

col_names = grep("neighborhood", colnames(pnsc01), value = T)
col_names_b = paste0(col_names, "_b")
pnsc01[, col_names_b] = ifelse(pnsc01[, col_names] == 5, 1,0)
pnsc01[, col_names] = NULL

describe(pnsc01)


########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",abcd_files_path)
yle01$ple_admin = NULL
yle01 = yle01[,!grepl("_past_|_fu(2)?_y$", colnames(yle01))]

yle01[yle01 ==6 | yle01 ==7 ] = NA

describe(yle01)


########### Parent Life Events ###########
ple = load_instrument("abcd_ple01",abcd_files_path)
ple$ple_p_select_language___1 = NULL
ple = ple[,!grepl("_fu(2)?_p$", colnames(ple))]

ple[ple ==6 | ple ==7 ] = NA

# View(describe(ple))


########### Parent Community Risk and Protective Factors ###########
crpf = load_instrument("abcd_crpf01",abcd_files_path)

crpf$su_select_language___1 = NULL
crpf[crpf == 999 | crpf == 4] = NA

crpf$su_risk_p_6[crpf$su_risk_p_6 == 2] = NA
crpf[,grep("su_risk_p_[7-9]", colnames(crpf))] = NULL

col_names = grep("su_risk_p_[1-5]", colnames(crpf), value = T)
col_names_b = paste0(col_names, "_b")
crpf[, col_names_b] = ifelse(crpf[, col_names] == 0, 1,0)
crpf[, col_names] = NULL

describe(crpf)


########### Parent PhenX Community Cohesion ###########
pxccp01 = load_instrument("abcd_pxccp01",abcd_files_path)
pxccp01[pxccp01 == 777| pxccp01 == 999] = NA
pxccp01$comc_phenx_select_language = NULL

col_names = grep("comc_phenx", colnames(pxccp01), value = T)
col_names_b = paste0(col_names, "_b")
pxccp01[, col_names_b] = ifelse(pxccp01[, col_names] == 5, 1,0)
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
dhx01$devhx_mom_age_old   = ifelse(dhx01$devhx_3_p > quantile(dhx01$devhx_3_p , .9, na.rm = T), 1, 0)

dhx01$devhx_prenatal_care_HRP  = ifelse(dhx01$devhx_11_p > quantile(dhx01$devhx_11_p , .9, na.rm = T), 1, 0)
dhx01$devhx_prenatal_care_low  = ifelse(dhx01$devhx_11_p < 14, 1, 0)

dhx01$devhx_late_motor_development = ifelse(dhx01$devhx_20_p == 5, 1, 0)
dhx01$devhx_late_speech_development = ifelse(dhx01$devhx_21_p == 5, 1, 0)

dhx01$devhx_low_birth_weight = ifelse(dhx01$birth_weight_lbs_tot < (2500/453.6), 1, 0)

dhx01[,c("devhx_3_p", "devhx_11_p", "devhx_20_p", "devhx_21_p", "birth_weight_lbs_tot")] = NULL

# View(describe(dhx01))


########### Parent Multi-Group Ethnic Identity-Revised Survey ###########
# meim = load_instrument("abcd_meim01",abcd_files_path)
# meim[,c("meim_select_language___1","meim_ethnic_id_p")] = NULL 
# 
# describe(meim)


########### Children's Report of Parental Behavioral Inventory ###########
crpbi = load_instrument("crpbi01",abcd_files_path)
crpbi[,c("crpbi_studycaregiver_id", "crpbi_caregiver1_y", "crpbi_caregiver2_y")] = NULL

col_names = grep("crpbi_", colnames(crpbi), value = T)
col_names_b = paste0(col_names, "_b")
crpbi[, col_names_b] = ifelse(crpbi[col_names] == 3, 1,0)
crpbi[, col_names] = NULL

describe(crpbi)


########### Parent Mexican American Cultural Values Scale Modified ###########
# macv = load_instrument("macv01",abcd_files_path)
# macv$mex_american_select_lang_1 = NULL
# 
# describe(macv)


########### Parental Rules on Substance Use ###########
prq = load_instrument("prq01",abcd_files_path)
prq = prq[,grepl("src|sex|event|interview|_q(1|4|7)$", colnames(prq))]

col_names = grep("parent_rules", colnames(prq), value = T)
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
cb[cb == 777 | cb == 999] = NA
describe(cb)


########### Peer Experiences Questionnaire ###########
peq01 = load_instrument("abcd_peq01",abcd_files_path)

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
pbp01[, positive_b] = ifelse(pbp01[,positive] == 5, 1,0)
pbp01[, positive] = NULL

describe(pbp01)


########### Youth Peer Network Health Protective Scaler ###########
pnhps01 = load_instrument("abcd_pnhps01",abcd_files_path)
pnhps01$pnh_substance[pnhps01$pnh_substance == 3] = 1
pnhps01$pnh_help[pnhps01$pnh_help == 2] = 1
pnhps01$pnh_encourage[pnhps01$pnh_encourage == 2] = 1
pnhps01[,c("pnh_how_much_encourage", "pnh_how_much_help", "pnh_art_involve")] = NULL

describe(pnhps01)


########### Other Resilience ###########
# ysr = load_instrument("abcd_ysr01",abcd_files_path)
# 
# ysr[,grep("remote|admin|device", colnames(ysr))] = NULL
# ysr[ysr == -1 | ysr == "Don't know"] = NA
# ysr$resiliency5a_y[ysr$resiliency5a_y > 100] = 100
# ysr$resiliency6a_y[ysr$resiliency6a_y > 100] = 100
# 
# describe(ysr)
# ysr_wide = get_wide_data(ysr)


########### Youth Substance Use Attitudes ###########
ysua = load_instrument("abcd_ysua01",abcd_files_path)
ysua[ysua == 999] = NA
ysua[grep("^(ptu|path|phs)",colnames(ysua))] = NULL
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

library("fastDummies")
columns_to_dummy = grep("ocp", colnames(occsp01), value = T)
occsp01 <- dummy_cols(occsp01, select_columns = columns_to_dummy, ignore_na = T, remove_selected_columns = T)


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
exposome_set = merge(exposome_set, pxccp01, all = T)
exposome_set = merge(exposome_set, dhx01, all = T)
# exposome_set = merge(exposome_set, meim, all =T)
exposome_set = merge(exposome_set, crpbi, all =T)
# exposome_set = merge(exposome_set, macv, all =T)
exposome_set = merge(exposome_set, prq, all =T)
exposome_set = merge(exposome_set, pacc, all =T)
exposome_set = merge(exposome_set, tbi, all =T)
exposome_set = merge(exposome_set, cb, all =T)
exposome_set = merge(exposome_set, peq01, all =T)
exposome_set = merge(exposome_set, pbp01, all =T)
exposome_set = merge(exposome_set, pnhps01, all =T)
# exposome_set = merge(exposome_set, ysr, all =T)
exposome_set = merge(exposome_set, peer_deviance, all =T)
exposome_set = merge(exposome_set, occsp01, all =T)

# remove 3 year follow up and empty columns
exposome_set = exposome_set[exposome_set$eventname != "3_year_follow_up_y_arm_1", ]
exposome_set = exposome_set[, colSums(is.na(exposome_set)) != nrow(exposome_set)]

write.csv(exposome_set, "data/exposome_set_item.csv", row.names = F, na = "")




