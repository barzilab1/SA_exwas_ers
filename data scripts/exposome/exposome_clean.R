library(psych)

source("config.R")
source("utility_fun.R")

########### Discrimination ###########
ydmes01 = load_instrument("abcd_ydmes01",abcd_files_path)

ydmes01[ydmes01 == 777 | ydmes01 == 999] = NA
ydmes01 = droplevels(ydmes01)

summary(ydmes01[ydmes01$eventname == "1_year_follow_up_y_arm_1",])

#check collinearity
library("psych")
matrix_names = colnames(ydmes01[ ,grep("_matrix_", colnames(ydmes01)) ])
ydmes01[,matrix_names] = apply(ydmes01[,matrix_names], 2, function(x) {as.numeric(as.character(x))})
xcor <- polychoric(ydmes01[,matrix_names])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]


################### TRAUMA ###################
ptsd01 = load_instrument("abcd_ptsd01",abcd_files_path)


########### School Risk and Protective Factors ###########
srpf01 = load_instrument("srpf01",abcd_files_path)
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
describe(pmq01)


########### Youth Neighborhood Safety/Crime ###########
nsc01 = load_instrument("abcd_nsc01",abcd_files_path)
describe(nsc01)


########### Parent Neighborhood Safety/Crime ###########
pnsc01 = load_instrument("abcd_pnsc01",abcd_files_path)
pnsc01$nei_p_select_language___1 = NULL
describe(pnsc01)


########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",abcd_files_path)
yle01[yle01 ==6 | yle01 ==7 ] = NA

yle01 = yle01[,!grepl("_fu(2)?_y$", colnames(yle01))]


### copy the 0 from ever to the last year items
# fix cols names
col_index = grep("^le", colnames(yle01))
colnames(yle01)[col_index] = paste0("p",colnames(yle01)[col_index] )

col_index = grep("_y_", colnames(yle01))
colnames(yle01)[col_index] = sub("y_","",colnames(yle01)[col_index])

names_last_yeat = grep("_past_yr_y$", colnames(yle01), value = T)
for (colname_last_yeat in names_last_yeat) {
  name_ever = sub("past_yr_","",colname_last_yeat)
  name_new = paste0(colname_last_yeat, "_all")
  #clean the past year (was asked only of ever == 1)
  yle01[,colname_last_yeat] = ifelse(yle01[,name_ever] == 0 | is.na(yle01[,name_ever]), NA, yle01[,colname_last_yeat])
  yle01[,name_new] = ifelse( is.na(yle01[,colname_last_yeat]) & yle01[,name_ever] == 0, 0 ,yle01[,colname_last_yeat])
}


########### Parent Life Events ###########
ple = load_instrument("abcd_ple01",abcd_files_path)
ple = ple[, !(colnames(ple) %in% c("ple_p_select_language___1"))]

ple[ple ==6 | ple ==7 ] = NA
ple = ple[,!grepl("_fu(2)?_p$", colnames(ple))]


### copy the 0 from ever to the last year items
# fix cols names
col_index = which(colnames(ple) == "ple_injur_p_p")
colnames(ple)[col_index] = "ple_injured_past_yr_p"

col_index = grep("_past$", colnames(ple))
colnames(ple)[col_index] = sub("_p_past","_past_yr_p",colnames(ple)[col_index])

names_last_yeat = grep("_past_yr_p$", colnames(ple), value = T)
for (colname_last_yeat in names_last_yeat) {
  name_ever = sub("past_yr_","",colname_last_yeat)
  name_new = paste0(colname_last_yeat, "_all")
  #clean the past year (was asked only of ever == 1)
  ple[,colname_last_yeat] = ifelse(ple[,name_ever] == 0 | is.na(ple[,name_ever]), NA, ple[,colname_last_yeat])
  ple[,name_new] = ifelse( is.na(ple[,colname_last_yeat]) & ple[,name_ever] == 0, 0 ,ple[,colname_last_yeat])
}

View(describe(ple))


########### Parent Community Risk and Protective Factors ###########
crpf = load_instrument("abcd_crpf01",abcd_files_path)

crpf$su_select_language___1 = NULL
crpf[crpf == 999 | crpf == 4] = NA

crpf$su_risk_p_6[crpf$su_risk_p_6 == 2] = NA
crpf$su_risk_p_9 = ifelse(crpf$su_risk_p_9 > 0, crpf$su_risk_p_9 - 7 , 0)

describe(crpf)


########### Parent PhenX Community Cohesion ###########
pxccp01 = load_instrument("abcd_pxccp01",abcd_files_path)
pxccp01[pxccp01 == 777| pxccp01 == 999] = NA
pxccp01$comc_phenx_select_language = NULL
describe(pxccp01)


########### Developmental History ###########
dhx01 = load_instrument("dhx01",abcd_files_path)

dhx01[dhx01 == 999 | dhx01 == -1] = NA
dhx01$accult_select_language = NULL

#remove empty columns 
dhx01 = dhx01[,colSums(is.na(dhx01)) != nrow(dhx01)]

#change the scale
dhx01$devhx_caffeine_11[dhx01$devhx_caffeine_11 == 0] = 4

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


########### Parent Sports and Activities Involvement Questionnaire ###########
saiq02 = load_instrument("abcd_saiq02",abcd_files_path)
# saiq02$sai_p_lax_school[saiq02$sai_p_lax_school == 11] = 1

saiq02$sai_total_activities_p = rowSums(saiq02[,grep("sai_p_activities___(?!29)",colnames(saiq02), perl = T)])
saiq02 = saiq02[,grep("src|interview|sex|event|sai_p_activities___(?!29)|sai_total", colnames(saiq02), perl = T)]


########### Longitudinal Parent Sports and Activities Involvement Questionnaire ###########
lpsaiq = load_instrument("abcd_lpsaiq01",abcd_files_path)

lpsaiq[lpsaiq == 999] = NA
lpsaiq$sai_l_p_select_language___1 = NULL

# Total activities
lpsaiq$sai_total_activities_l_p = rowSums(lpsaiq[,grep("sai_p_activities_l___(?!29)",colnames(lpsaiq), perl = T)])

# select variables
lpsaiq = lpsaiq[,grep("src|interview|sex|event|sai_p_activities_l___(?!29)|sai_total", colnames(lpsaiq), perl = T)]


########### Parent Multi-Group Ethnic Identity-Revised Survey ###########
meim = load_instrument("abcd_meim01",abcd_files_path)
meim[,c("meim_select_language___1","meim_ethnic_id_p")] = NULL 
describe(meim)


########### Youth Screen Time Survey ###########
stq = load_instrument("abcd_stq01",abcd_files_path)
stq[,grep("_dk$", colnames(stq), value = T)] = NULL

stq[stq == -1] = NA
stq$screentime_admin = NULL

col_names_clean = colnames(stq)[sapply(stq, function(x){any( x == 777 | x == 999, na.rm = T)})]
col_names_clean = setdiff(col_names_clean, c("screentime_smq_followers", "screentime_smq_following"))
  
temp = stq[,col_names_clean]
temp[temp == 777 | temp == 999] = NA
stq[,col_names_clean] = temp

View(describe(stq))


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


########### Children's Report of Parental Behavioral Inventory ###########
crpbi = load_instrument("crpbi01",abcd_files_path)


########### Parent Mexican American Cultural Values Scale Modified ###########
macv = load_instrument("macv01",abcd_files_path)
macv$mex_american_select_lang_1 = NULL
describe(macv)


########### Parent Vancouver Index of Acculturation-Short Survey (VIA) ###########
via = load_instrument("abcd_via01",abcd_files_path)
via[,c("vancouver_select_language___1", "vancouver_q1_ddn_p")] = NULL


########### Parent Adult Self Report Raw Scores Aseba ###########
pasr = load_instrument("pasr01",abcd_files_path)
pasr[,grep("language|_dk$", colnames(pasr), value = T)] = NULL
View(describe(pasr))


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


########### Youth Acculturation Survey Modified from PhenX (ACC) ###########
yacc = load_instrument("yacc01",abcd_files_path)
yacc[yacc == 777] = NA
yacc$accult_q3_other_y = NULL


########### Parent Acculturation Survey ###########
pacc = load_instrument("pacc01",abcd_files_path)
pacc$accult_select_language___1 = NULL
pacc$accult_q3_other_p = NULL

pacc[pacc == 777 | pacc == 999] = NA


########### Child Nutrition Assessment (by Parent) ###########
cna = load_instrument("abcd_cna01",abcd_files_path)
cna[cna == 999] = NA
cna$cna_p_select_language___1 = NULL
describe(cna)


########### Youth Block Food Screen ###########
#TODO ask from ABCD for more data
bkfs = load_instrument("abcd_bkfs01",abcd_files_path)
describe(bkfs)


########### Parent Ohio State Traumatic Brain Injury Screen ###########
otbi = load_instrument("abcd_otbi01",abcd_files_path)
otbi$tbi_select_language___1 = NULL


########### Longitudinal Parent Ohio State Traumatic Brain Injury Screen ###########
lpohstbi = load_instrument("abcd_lpohstbi01",abcd_files_path)
lpohstbi$tbi_l_select_language___1 = NULL


########### Cyber Bully ###########
cb = load_instrument("abcd_cb01",abcd_files_path)
cb[cb == 777 | cb == 999] = NA
describe(cb)


########### Youth Peer Behavior Profile ###########
pbp01 = load_instrument("abcd_pbp01",abcd_files_path)
pbp01[pbp01 == 999] = NA
describe(pbp01)


########### Youth Peer Network Health Protective Scaler ###########
pnhps01 = load_instrument("abcd_pnhps01",abcd_files_path)
pnhps01$pnh_substance[pnhps01$pnh_substance == 3] = 1
pnhps01$pnh_help[pnhps01$pnh_help == 2] = 1
pnhps01$pnh_encourage[pnhps01$pnh_encourage == 2] = 1
pnhps01[,c("pnh_how_much_encourage", "pnh_how_much_help", "pnh_art_involve")] = NULL
describe(pnhps01)


########### Multidimensional Neglectful Behavior Scale ###########
#TODO: remove!!!
neglectful_behavior = load_instrument("neglectful_behavior01",abcd_files_path)
neglectful_behavior$mnbs_admin = NULL
neglectful_behavior[neglectful_behavior == 777] = NA
describe(neglectful_behavior)


########### Occupation Survey Parent ###########
## TODO remember to handle these as factors, not continuous
occsp01 = load_instrument("abcd_occsp01",abcd_files_path)
occsp01 = occsp01[, grep("src|event|interview|sex|ocp_.*_acs_(indust|ocp)", colnames(occsp01))]
occsp01[occsp01 == 777 | occsp01 == 999] = NA
describe(occsp01)



########### merge all tables ###########
exposome_set = merge(ydmes01,ptsd01, all =T)
exposome_set = merge(exposome_set,srpf01, all = T)
exposome_set = merge(exposome_set,fes01, all =T)
exposome_set = merge(exposome_set,fes02, all =T)
exposome_set = merge(exposome_set,pmq01, all =T)
exposome_set = merge(exposome_set,nsc01, all =T)
exposome_set = merge(exposome_set,pnsc01, all =T)
exposome_set = merge(exposome_set,yle01, all =T)
exposome_set = merge(exposome_set,ple, all =T)
exposome_set = merge(exposome_set,crpf, all =T)
exposome_set = merge(exposome_set,pxccp01, all = T)
exposome_set = merge(exposome_set,dhx01, all = T)
exposome_set = merge(exposome_set,saiq02, all =T)
exposome_set = merge(exposome_set,lpsaiq, all =T)
exposome_set = merge(exposome_set,meim, all =T)
exposome_set = merge(exposome_set,stq, all =T)
exposome_set = merge(exposome_set,stq01, all =T)
exposome_set = merge(exposome_set,crpbi, all =T)
exposome_set = merge(exposome_set,macv, all =T)
exposome_set = merge(exposome_set,via, all =T)
exposome_set = merge(exposome_set,pasr, all =T)
exposome_set = merge(exposome_set,prq, all =T)
exposome_set = merge(exposome_set,yacc, all =T)
exposome_set = merge(exposome_set,pacc, all =T)
exposome_set = merge(exposome_set,cna, all =T)
# exposome_set = merge(exposome_set,bkfs, all =T)
exposome_set = merge(exposome_set,otbi, all =T)
exposome_set = merge(exposome_set,lpohstbi, all =T)
exposome_set = merge(exposome_set,cb, all =T)
exposome_set = merge(exposome_set,pbp01, all =T)
exposome_set = merge(exposome_set,pnhps01, all =T)
exposome_set = merge(exposome_set,neglectful_behavior, all =T)
exposome_set = merge(exposome_set,occsp01, all =T)

exposome_set = exposome_set[grep("^(1|2|baseline)",exposome_set$eventname),]
exposome_set = exposome_set[, colSums(is.na(exposome_set)) != nrow(exposome_set)]

write.csv(exposome_set, "outputs/exposome_set_item.csv", row.names = F, na = "")




