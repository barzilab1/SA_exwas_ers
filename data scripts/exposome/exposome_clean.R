
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
#abcd_ptsd01
ptsd01 = load_instrument("abcd_ptsd01",abcd_files_path)

ptsd01_baseline = ptsd01[which(grepl("baseline", ptsd01$eventname)),]
ptsd01_first_year = ptsd01[which(grepl("1_year_follow_up", ptsd01$eventname)),]

#update first year names
# colnames(ptsd01_first_year)[c(2,3)] = paste0(colnames(ptsd01_first_year)[c(2,3)], "_1_year")


########### School Risk and Protective Factors ###########
srpf01 = load_instrument("srpf01",abcd_files_path)
summary(srpf01)


########### Youth Family Environment Scale: Family Conflict Subscale ###########
fes01 = load_instrument("abcd_fes01",abcd_files_path)


########### Parent Family Environment Scale: Family Conflict Subscale ###########
fes02 = load_instrument("fes02",abcd_files_path)
fes02 = unique(fes02)

fes02 = fes02[, !(colnames(fes02) %in% c("fam_enviro_select_language___1"))]


########### Parental Monitoring Survey ###########
pmq01 = load_instrument("pmq01",abcd_files_path)
summary(pmq01)


########### Youth Neighborhood Safety/Crime ###########
nsc01 = load_instrument("abcd_nsc01",abcd_files_path)


########### Parent Neighborhood Safety/Crime ###########
pnsc01 = load_instrument("abcd_pnsc01",abcd_files_path)
pnsc01 = pnsc01[, !(colnames(pnsc01) %in% c("nei_p_select_language___1"))]



########### Parent Family History Summary Scores ###########
fhxssp = load_instrument("abcd_fhxssp01",abcd_files_path)
fhxssp_item = fhxssp[,grepl("^(src|sex|inter|event)|momdad|parent", colnames(fhxssp))]

#TODO check for each time point
#remove columns with more than 20% NA
# fhxssp = fhxssp[,-which(colSums(is.na(fhxssp)) >= 0.2*dim(fhxssp)[1])]


summary(fhxssp)

########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",abcd_files_path)
yle01[yle01 >=6 ] = NA

summary(droplevels(yle01[yle01$eventname == "1_year_follow_up_y_arm_1",]))


########### Parent Life Events ###########
ple = load_instrument("abcd_ple01",abcd_files_path)
ple = ple[, !(colnames(ple) %in% c("ple_p_select_language___1"))]


# ple = ple[ple$eventname == "1_year_follow_up_y_arm_1",]
# ple = ple[,-which(colSums(is.na(ple)) >= 0.2*dim(ple)[1])]
summary(droplevels(ple))


########### family relationship section ###########
acspsw03 = load_instrument("acspsw03",abcd_files_path)
acspsw03 = acspsw03[acspsw03$eventname == "baseline_year_1_arm_1",grepl("src|inter|sex|event|fam", colnames(acspsw03))]

summary(acspsw03)



########### Parent Community Risk and Protective Factors ###########
crpf = load_instrument("abcd_crpf01",abcd_files_path)

crpf$su_select_language___1 = NULL
crpf[crpf == 999] = NA


col_to_fix = crpf[,grepl("p_[1-5]$", colnames(crpf))]
col_to_fix[col_to_fix==4] = NA
crpf[,colnames(col_to_fix)] = col_to_fix

crpf$su_risk_p_6[crpf$su_risk_p_6 == 2] = NA

summary(droplevels(crpf))


########### Developmental History ###########
dhx01 = load_instrument("dhx01",abcd_files_path)

dhx01[dhx01 == 999 | dhx01 == -1] = NA
dhx01$accult_select_language = NULL

#change the scale
dhx01$devhx_caffeine_11 = as.numeric(as.character(dhx01$devhx_caffeine_11))
dhx01$devhx_caffeine_11[dhx01$devhx_caffeine_11 == 0] = 4

#remove outliers
dhx01$devhx_11_p = as.numeric(as.character(dhx01$devhx_11_p))
dhx01$devhx_11_p[dhx01$devhx_11_p > 50] = NA

dhx01$devhx_16_p = as.numeric(as.character(dhx01$devhx_16_p))
dhx01$devhx_16_p[dhx01$devhx_16_p > 60] = NA


dhx01[,grepl("devhx_19[a-d]_p$", colnames(dhx01))] = apply(dhx01[,grepl("devhx_19[a-d]_p$", colnames(dhx01))], 2,
                                                           function(i) as.numeric(as.character(i)))
dhx01$devhx_19a_p[dhx01$devhx_19a_p > 24] = NA
dhx01$devhx_19b_p[dhx01$devhx_19b_p < 3 | dhx01$devhx_19b_p > 60] = NA
dhx01$devhx_19b_p[dhx01$devhx_19c_p < 7 | dhx01$devhx_19c_p > 60] = NA
dhx01$devhx_19b_p[dhx01$devhx_19d_p < 4 | dhx01$devhx_19d_p > 72] = NA
#TODO check for each time point
#remove columns with more than 20% NA
# dhx01 = dhx01[,-which(colSums(is.na(dhx01)) >= 0.2*dim(dhx01)[1])]

summary(droplevels(dhx01))

########### ABCD Parent Sports and Activities Involvement Questionnaire (SAIQ) ###########

saiq02 = load_instrument("abcd_saiq02",abcd_files_path)
saiq02$sai_total_activities = rowSums(saiq02[,grepl("sai_p_activities___[0-28]",colnames(saiq02))])

saiq02 <- saiq02 %>% dplyr::select(src_subject_id, eventname, sai_total_activities)

########### Longitudinal Parent Sports and Activities Involvement Questionnaire ###########
lpsaiq = load_instrument("abcd_lpsaiq01",abcd_files_path)

lpsaiq[lpsaiq == 999] = NA
lpsaiq = lpsaiq[, !(colnames(lpsaiq) %in% c("sai_l_p_select_language___1"))]

#TODO check for each time point
#remove columns with more than 20% NA
# lpsaiq = lpsaiq[,-which(colSums(is.na(lpsaiq)) >= 0.2*dim(lpsaiq)[1])]
summary(droplevels(lpsaiq))
# Total activities
lpsaiq01$sai_total_activities = rowSums(lpsaiq01[,grepl("sai_p_activities_l___[0-28]",colnames(lpsaiq01))])

# select variables
lpsaiq01 <- lpsaiq01 %>% dplyr::select(src_subject_id, eventname, contains("perwk"), sai_total_activities)

########### Parent Multi-Group Ethnic Identity-Revised Survey ###########
meim = load_instrument("abcd_meim01",abcd_files_path)

#remove empty columns
meim = meim[,colSums(is.na(meim)) != dim(meim)[1]]
summary(droplevels(meim))


########### Youth Screen Time Survey ###########
stq = load_instrument("abcd_stq01",abcd_files_path)
stq = stq[,!grepl("___", colnames(stq))]

summary(droplevels(stq))


########### Children's Report of Parental Behavioral Inventory ###########
crpbi = load_instrument("crpbi01",abcd_files_path)
crpbi = crpbi[, !(colnames(crpbi) %in% c("timept"))]

summary(crpbi[crpbi$eventname == "1_year_follow_up_y_arm_1",])


########### Parent Mexican American Cultural Values Scale Modified ###########
macv = load_instrument("macv01",abcd_files_path)
macv = macv[, !(colnames(macv) %in% c("mex_american_select_lang_1"))]


########### Parent Acculturation Survey ###########
pacc = load_instrument("pacc01",abcd_files_path)
pacc = pacc[, !(colnames(pacc) %in% c("accult_select_language___1"))]

#remove empty columns
pacc = pacc[, colSums(is.na(pacc)) != dim(pacc)[1]]

pacc[pacc == 777 | pacc == 999] = NA

########### Parent Adult Self Report Raw Scores Aseba ###########
pasr = load_instrument("pasr01",abcd_files_path)
pasr = pasr[, !(colnames(pasr) %in% c("asr_select_language___1"))]


########### Parental Rules on Substance Use ###########
prq = load_instrument("prq01",abcd_files_path)
prq$pr_select_language___1 = NULL

col_to_fix = prq[,grepl("_q(1|4|7)$", colnames(prq))]
col_to_fix[col_to_fix==6] = NA
prq[,colnames(col_to_fix)] = col_to_fix

prq$parent_rules_q6[prq$parent_rules_q6 == 4] = NA

summary(droplevels(prq))
#TODO check for each time point
#remove columns with more than 20% NA
# prq = droplevels(prq[prq$eventname == "1_year_follow_up_y_arm_1",])
# prq = prq[,-which(colSums(is.na(prq)) >= 0.2*dim(prq)[1])]


########### Parent Screen Time Survey ###########
stq = load_instrument("stq01",abcd_files_path)
stq$scrtime_p_select_lang___1 = NULL

#TODO check for each time point
#remove columns with more than 20% NA
# stq = droplevels(stq[stq$eventname == "1_year_follow_up_y_arm_1",])
# stq = stq[,-which(colSums(is.na(stq)) >= 0.2*dim(stq)[1])]


########### Youth Acculturation Survey Modified from PhenX (ACC) ###########
yacc = load_instrument("yacc01",abcd_files_path)
yacc[yacc == 777] = NA

#TODO check for each time point
#remove columns with more than 20% NA
# yacc = droplevels(yacc[yacc$eventname == "1_year_follow_up_y_arm_1",])
# yacc = yacc[,-which(colSums(is.na(yacc)) >= 0.2*dim(yacc)[1])]


########### Child Nutrition Assessment ###########
cna = load_instrument("abcd_cna01",abcd_files_path)
cna[cna == 999] = NA
cna = cna[, !(colnames(cna) %in% c("cna_p_select_language___1"))]



########### Parent Ohio State Traumatic Brain Injury Screen ###########
otbi = load_instrument("abcd_otbi01",abcd_files_path)
otbi = otbi[, !(colnames(otbi) %in% c("tbi_select_language___1"))]

#remove empty columns
otbi = otbi[, colSums(is.na(otbi)) != dim(otbi)[1]]



########### Cyber Bully ###########
cb = load_instrument("abcd_cb01",abcd_files_path)
cb[cb == 777 | cb == 999] = NA



########### merge all tables
exposome_set = merge(srpf01,pmq01)
exposome_set = merge(exposome_set,yle01)
exposome_set = merge(exposome_set,fes01)
exposome_set = merge(exposome_set,nsc01)
exposome_set = merge(exposome_set,pnsc01)






