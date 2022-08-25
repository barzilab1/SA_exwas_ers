library(psych)
library(plyr)

source("config.R")
source("utility_fun.R")

########### Discrimination ###########
ydmes01 = load_instrument("abcd_ydmes01",abcd_files_path)
ydmes01[ydmes01 == 777 | ydmes01 == 999] = NA
describe(ydmes01)

ydmes01_wide = get_wide_data(ydmes01)

################### TRAUMA ###################
ptsd01 = load_instrument("abcd_ptsd01",abcd_files_path)
ptsd01_wide = get_wide_data(ptsd01)


########### School Risk and Protective Factors ###########
srpf01 = load_instrument("srpf01",abcd_files_path)
describe(srpf01)
srpf01_wide = get_wide_data(srpf01)


########### Youth Family Environment Scale: Family Conflict Subscale ###########
fes01 = load_instrument("abcd_fes01",abcd_files_path)
describe(fes01)
fes01_wide = get_wide_data(fes01)


########### Parent Family Environment Scale: Family Conflict Subscale ###########
fes02 = load_instrument("fes02",abcd_files_path)
fes02$fam_enviro_select_language___1 = NULL
describe(fes02)
fes02_wide = get_wide_data(fes02)


########### Parental Monitoring Survey ###########
pmq01 = load_instrument("pmq01",abcd_files_path)
describe(pmq01)
pmq01_wide = get_wide_data(pmq01)


########### Youth Neighborhood Safety/Crime ###########
nsc01 = load_instrument("abcd_nsc01",abcd_files_path)
describe(nsc01)
nsc01_wide = get_wide_data(nsc01)


########### Parent Neighborhood Safety/Crime ###########
pnsc01 = load_instrument("abcd_pnsc01",abcd_files_path)
pnsc01$nei_p_select_language___1 = NULL
describe(pnsc01)
pnsc01_wide = get_wide_data(pnsc01)


########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",abcd_files_path)
yle01$ple_admin = NULL
yle01 = yle01[,!grepl("_past_|_fu(2)?_y$", colnames(yle01))]

yle01[yle01 ==6 | yle01 ==7 ] = NA

describe(yle01)
yle01_wide = get_wide_data(yle01)


########### Parent Life Events ###########
ple = load_instrument("abcd_ple01",abcd_files_path)
ple$ple_p_select_language___1 = NULL
ple = ple[,!grepl("_fu(2)?_p$", colnames(ple))]

ple[ple ==6 | ple ==7 ] = NA

View(describe(ple))
ple_wide = get_wide_data(ple)



########### Parent Community Risk and Protective Factors ###########
crpf = load_instrument("abcd_crpf01",abcd_files_path)

crpf$su_select_language___1 = NULL
crpf[crpf == 999 | crpf == 4] = NA

crpf$su_risk_p_6[crpf$su_risk_p_6 == 2] = NA
crpf$su_risk_p_9 = ifelse(crpf$su_risk_p_9 > 0, crpf$su_risk_p_9 - 7 , 0)

crpf[,grep("su_risk_p_[7-9]", colnames(crpf))] = NULL
describe(crpf)
crpf_wide = get_wide_data(crpf)


########### Parent PhenX Community Cohesion ###########
pxccp01 = load_instrument("abcd_pxccp01",abcd_files_path)
pxccp01[pxccp01 == 777| pxccp01 == 999] = NA
pxccp01$comc_phenx_select_language = NULL
describe(pxccp01)
pxccp01_wide = get_wide_data(pxccp01)


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
# dhx01$devhx_3_p[dhx01$devhx_3_p > 55] = NA
dhx01$devhx_4_p[dhx01$devhx_4_p > 80] = NA

dhx01$devhx_11_p[dhx01$devhx_11_p > 50] = NA
dhx01$devhx_16_p[dhx01$devhx_16_p > 60] = NA

dhx01$devhx_19a_p[dhx01$devhx_19a_p > 24] = NA
dhx01$devhx_19b_p[dhx01$devhx_19b_p < 3 | dhx01$devhx_19b_p > 60] = NA
dhx01$devhx_19c_p[dhx01$devhx_19c_p < 7 | dhx01$devhx_19c_p > 60] = NA
dhx01$devhx_19d_p[dhx01$devhx_19d_p < 4 | dhx01$devhx_19d_p > 72] = NA

dhx01$devhx_caff_amt_week[dhx01$devhx_caff_amt_week > 24] = NA

View(describe(dhx01))
dhx01_wide = get_wide_data(dhx01)


########### Youth Risk Behavior Survey Exercise Physical Activity (YRB) ###########
yrb = load_instrument("abcd_yrb01",abcd_files_path)

#change scale
yrb$physical_activity2_y = yrb$physical_activity2_y - 1

describe(yrb)
yrb_wide = get_wide_data(yrb)


########### Parent Sports and Activities Involvement Questionnaire ###########
saiq02 = load_instrument("abcd_saiq02",abcd_files_path)
# saiq02$sai_p_lax_school[saiq02$sai_p_lax_school == 11] = 1

saiq02$sai_total_activities_p = rowSums(saiq02[,grep("sai_p_activities___(?!29)", colnames(saiq02), perl = T)])
saiq02 = saiq02[,grep("src|interview|sex|event|sai_p_activities___(?!29)|sai_total", colnames(saiq02), perl = T)]


########### Longitudinal Parent Sports and Activities Involvement Questionnaire ###########
lpsaiq = load_instrument("abcd_lpsaiq01",abcd_files_path)
lpsaiq$sai_l_p_select_language___1 = NULL

lpsaiq[lpsaiq == 999] = NA

lpsaiq$sai_total_activities_l_p = rowSums(lpsaiq[,grep("sai_p_activities_l___(?!29)",colnames(lpsaiq), perl = T)])
lpsaiq = lpsaiq[,grep("src|interview|sex|event|sai_p_activities_l___(?!29)|sai_total", colnames(lpsaiq), perl = T)]


### combine the 2 instruments 
colnames(lpsaiq) = sub("_l_", "_", colnames(lpsaiq))
saiq = rbind.fill(saiq02, lpsaiq)
describe(saiq)
saiq_wide = get_wide_data(saiq)


########### Parent Multi-Group Ethnic Identity-Revised Survey ###########
meim = load_instrument("abcd_meim01",abcd_files_path)
meim[,c("meim_select_language___1","meim_ethnic_id_p")] = NULL 

describe(meim)
meim_wide = get_wide_data(meim)


########### Youth Screen Time Survey ###########
stq = load_instrument("abcd_stq01",abcd_files_path)
stq[,grep("_(dk|min)$", colnames(stq), value = T)] = NULL

stq[stq == -1] = NA
stq$screentime_admin = NULL

col_names_clean = colnames(stq)[sapply(stq, function(x){any( x == 777 | x == 999, na.rm = T)})]
col_names_clean = setdiff(col_names_clean, c("screentime_smq_followers", "screentime_smq_following"))
  
temp = stq[,col_names_clean]
temp[temp == 777 | temp == 999] = NA
stq[,col_names_clean] = temp


View(describe(stq))
stq_wide = get_wide_data(stq)


########### Parent Screen Time Survey ###########
stq01 = load_instrument("stq01",abcd_files_path)
stq01[,c("scrtime_p_select_lang___1","screentime_scrn_media_p__777", "screentime_start_time_p")] = NULL

# clean "refuse to answer"
stq01[stq01 == 777] = NA
temp = stq01[,grep("_(short|online)_",colnames(stq01), value = T)]
temp[temp == 6] = NA
stq01[,grep("_(short|online)_",colnames(stq01), value = T)] = temp

stq01$screentime_device_cell_age_p[stq01$screentime_device_cell_age_p > 15] = NA
stq01$screentime_device_cell_age_p[which(stq01$screentime_device_cell_age_p*12 > (stq01$interview_age + 1))] = NA

#change value range to no -> sometimes -> yes
stq01$screentime_device_cell_no_p = round(stq01$screentime_device_cell_no_p /2 + stq01$screentime_device_cell_no_p %% 2)

View(describe(stq01))
stq01_wide = get_wide_data(stq01)


########### Children's Report of Parental Behavioral Inventory ###########
crpbi = load_instrument("crpbi01",abcd_files_path)
crpbi[,c("crpbi_studycaregiver_id", "crpbi_caregiver1_y", "crpbi_caregiver2_y")] = NULL

describe(crpbi)
crpbi_wide = get_wide_data(crpbi)


########### Parent Mexican American Cultural Values Scale Modified ###########
macv = load_instrument("macv01",abcd_files_path)
macv$mex_american_select_lang_1 = NULL

describe(macv)
macv_wide = get_wide_data(macv)


########### Parental Rules on Substance Use ###########
prq = load_instrument("prq01",abcd_files_path)
prq$pr_select_language___1 = NULL

col_to_fix = prq[,grepl("_q(1|4|7)$", colnames(prq))]
col_to_fix[col_to_fix==6] = NA
prq[,colnames(col_to_fix)] = col_to_fix

prq$parent_rules_q3[prq$parent_rules_q3 == 4] = NA
prq$parent_rules_q6[prq$parent_rules_q6 == 4] = NA
prq$parent_rules_q9[prq$parent_rules_q9 == 4] = NA

describe(prq)
prq_wide = get_wide_data(prq)


########### Youth Acculturation Survey Modified from PhenX (ACC) ###########
yacc = load_instrument("yacc01",abcd_files_path)
yacc$accult_q3_other_y = NULL
yacc[yacc == 777] = NA

yacc = yacc[,grep("src|sex|event|interview|accult_q[1-2]", colnames(yacc)) ]
describe(yacc)
yacc_wide = get_wide_data(yacc)


########### Parent Acculturation Survey ###########
pacc = load_instrument("pacc01",abcd_files_path)
pacc$accult_select_language___1 = NULL
pacc$accult_q3_other_p = NULL

pacc[pacc == 777 | pacc == 999] = NA
pacc = pacc[,grep("src|sex|event|interview|accult_q[1-2]", colnames(pacc)) ]
describe(pacc)
pacc_wide = get_wide_data(pacc)


########### Child Nutrition Assessment (by Parent) ###########
cna = load_instrument("abcd_cna01",abcd_files_path)
cna$cna_p_select_language___1 = NULL
cna[cna == 999] = NA
describe(cna)
cna_wide = get_wide_data(cna)


########### Youth Block Food Screen ###########
#TODO ask from ABCD for more information about the instrument
bkfs = load_instrument("abcd_bkfs01",abcd_files_path)
bkfs$ra_confirm = NULL
bkfs$bkfs_select_language = NULL
bkfs[bkfs == 777] = NA

describe(bkfs)
bkfs_wide = get_wide_data(bkfs)


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
tbi_wide = get_wide_data(tbi)


########### Cyber Bully ###########
cb = load_instrument("abcd_cb01",abcd_files_path)
cb[cb == 777 | cb == 999] = NA

### copy the 0 from ever to the last year items
names_last_year = grep("_12mo$", colnames(cb), value = T)
for (colname_last_year in names_last_year) {
  name_ever = sub("_12mo","",colname_last_year)
  name_new = paste0(colname_last_year, "_all")
  # create the new feature
  cb[,name_new] = ifelse( is.na(cb[,colname_last_year]) & cb[,name_ever] == 0, 0 ,cb[,colname_last_year])
}

cb[,names_last_year] = NULL
describe(cb)

# make sure the feature names don't overlap  
colnames(cb)[grep("cybb",colnames(cb))] = paste0(colnames(cb)[grep("cybb",colnames(cb))], "_y")
cb_wide = get_wide_data(cb)


########### Peer Experiences Questionnaire ###########
peq01 = load_instrument("abcd_peq01",abcd_files_path)

describe(peq01)
peq01_wide = get_wide_data(peq01)


########### Youth Peer Behavior Profile ###########
pbp01 = load_instrument("abcd_pbp01",abcd_files_path)
pbp01[pbp01 == 999] = NA

describe(pbp01)
pbp01_wide = get_wide_data(pbp01)


########### Youth Peer Network Health Protective Scaler ###########
pnhps01 = load_instrument("abcd_pnhps01",abcd_files_path)
pnhps01$pnh_substance[pnhps01$pnh_substance == 3] = 1
pnhps01$pnh_help[pnhps01$pnh_help == 2] = 1
pnhps01$pnh_encourage[pnhps01$pnh_encourage == 2] = 1
pnhps01[,c("pnh_how_much_encourage", "pnh_how_much_help", "pnh_art_involve")] = NULL

describe(pnhps01)
pnhps01_wide = get_wide_data(pnhps01)


########### Other Resilience ###########
ysr = load_instrument("abcd_ysr01",abcd_files_path)

ysr[,grep("remote|admin|device", colnames(ysr))] = NULL
ysr[ysr == -1 | ysr == "Don't know"] = NA
ysr$resiliency5a_y[ysr$resiliency5a_y > 100] = 100
ysr$resiliency6a_y[ysr$resiliency6a_y > 100] = 100

describe(ysr)
ysr_wide = get_wide_data(ysr)


########### Youth Substance Use Attitudes ###########
ysua = load_instrument("abcd_ysua01",abcd_files_path)
ysua[ysua == 999] = NA
ysua[grep("^(ptu|path|phs)",colnames(ysua))] = NULL
describe(ysua)
ysua_wide = get_wide_data(ysua)


###########  Youth Substance Use Interview ###########
ysu02 = load_instrument("abcd_ysu02",abcd_files_path)
ysu02[, grep("^(path)_|su_today|tlfb_age|_dk$", colnames(ysu02), value = T)] = NULL


###########  Youth Substance Use Introduction and Patterns ###########
ysuip = load_instrument("abcd_ysuip01",abcd_files_path)
ysuip$xskipout_device = NULL
ysuip[, grep("tlfb_age", colnames(ysuip), value = T)] = NULL


### combine the 2 instruments 
colnames(ysuip) = sub("_l$", "", colnames(ysuip))
colnames(ysuip) = sub("_l_", "_", colnames(ysuip))
colnames(ysuip) = sub("_lsd_", "_hall_", colnames(ysuip))

ind_to_fix = grep("isip_(.*)_2", colnames(ysu02))
colnames(ysu02)[ind_to_fix] = sub("_2", "", colnames(ysu02)[ind_to_fix])
  
ysu = rbind.fill(ysu02, ysuip)
ysu_wide = get_wide_data(ysu)
View(describe(ysu_wide))


########### Occupation Survey Parent ###########
occsp01 = load_instrument("abcd_occsp01",abcd_files_path)
occsp01 = occsp01[, grep("src|event|interview|sex|ocp_.*_acs_(indust|ocp)", colnames(occsp01))]
occsp01[occsp01 == 777 | occsp01 == 999] = NA

describe(occsp01)

library("fastDummies")
columns_to_dummy = grep("ocp", colnames(occsp01), value = T)
occsp01 <- dummy_cols(occsp01, select_columns = columns_to_dummy, ignore_na = T, remove_selected_columns = T)

occsp01_wide = get_wide_data(occsp01)



########### merge all tables ###########
exposome_set = merge(ydmes01_wide, ptsd01_wide, all =T)
exposome_set = merge(exposome_set, srpf01_wide, all = T)
exposome_set = merge(exposome_set, fes01_wide, all =T)
exposome_set = merge(exposome_set, fes02_wide, all =T)
exposome_set = merge(exposome_set, pmq01_wide, all =T)
exposome_set = merge(exposome_set, nsc01_wide, all =T)
# exposome_set = merge(exposome_set, pnsc01_wide, all =T)
exposome_set = merge(exposome_set, yle01_wide, all =T)
exposome_set = merge(exposome_set, ple_wide, all =T)
exposome_set = merge(exposome_set, crpf_wide, all =T)
exposome_set = merge(exposome_set, pxccp01_wide, all = T)
exposome_set = merge(exposome_set, dhx01_wide, all = T)
exposome_set = merge(exposome_set, yrb_wide, all = T)
exposome_set = merge(exposome_set, saiq_wide, all =T)
# exposome_set = merge(exposome_set, meim_wide, all =T)
exposome_set = merge(exposome_set, stq_wide, all =T)
exposome_set = merge(exposome_set, stq01_wide, all =T)
exposome_set = merge(exposome_set, crpbi_wide, all =T)
# exposome_set = merge(exposome_set, macv_wide, all =T)
exposome_set = merge(exposome_set, prq_wide, all =T)
exposome_set = merge(exposome_set, yacc_wide, all =T)
exposome_set = merge(exposome_set, pacc_wide, all =T)
exposome_set = merge(exposome_set, cna_wide, all =T)
exposome_set = merge(exposome_set, bkfs_wide, all =T)
exposome_set = merge(exposome_set, tbi_wide, all =T)
exposome_set = merge(exposome_set, cb_wide, all =T)
exposome_set = merge(exposome_set, peq01_wide, all =T)
exposome_set = merge(exposome_set, pbp01_wide, all =T)
exposome_set = merge(exposome_set, pnhps01_wide, all =T)
exposome_set = merge(exposome_set, ysr_wide, all =T)
exposome_set = merge(exposome_set, ysua_wide, all =T)
exposome_set = merge(exposome_set, ysu_wide, all =T)
exposome_set = merge(exposome_set, occsp01_wide, all =T)

write.csv(exposome_set, "data/exposome_set_item.csv", row.names = F, na = "")




