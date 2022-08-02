
source("config.R")
source("utility_fun.R")



################### Youth Diagnostic Interview for DSM-5 Background Items 5 (lgbt/bullying/drop a class) ################### 
yksad01 = load_instrument("abcd_yksad01",abcd_files_path)

yksad01 = yksad01[, !grepl("grade|drop|det|remote", colnames(yksad01))]
summary(yksad01)

yksad01[yksad01 == "777"] = NA

yksad01$LGBT = (yksad01$kbi_y_sex_orient == 1 | yksad01$kbi_y_trans_id == 1)*1
yksad01$LGBT = ifelse( (is.na(yksad01$LGBT) & (yksad01$kbi_y_sex_orient %in% c(2,3) | yksad01$kbi_y_trans_id %in% c(2,3) )),
                       0, yksad01$LGBT)

#View(yksad01[c("LGBT", "kbi_y_sex_orient" , "kbi_y_trans_id" )])

yksad01$LGBT_inclusive = (yksad01$kbi_y_sex_orient <= 2 | yksad01$kbi_y_trans_id <= 2)*1
yksad01$LGBT_inclusive = ifelse( (is.na(yksad01$LGBT_inclusive) & (yksad01$kbi_y_sex_orient  == 3 | yksad01$kbi_y_trans_id == 3 )),
                                 0, yksad01$LGBT_inclusive)

# View(yksad01[c("LGBT", "LGBT_inclusive","kbi_y_sex_orient" , "kbi_y_trans_id" )])


################### ABCD Prodromal Psychosis Scale ################### 
pps01 = load_instrument("pps01", abcd_files_path)

pps01 = pps01[,grepl("src|interview|event|sex|([0-9]_y)$",colnames(pps01))]
summary(pps01)

summary(pps01[pps01$eventname == "baseline_year_1_arm_1",]) 
summary(pps01[pps01$eventname == "1_year_follow_up_y_arm_1",]) 


################### Parent General Behavior Inventory-Mania ###################
pgbi01 = load_instrument("abcd_pgbi01", abcd_files_path)
pgbi01 = pgbi01[,-which(colnames(pgbi01) == "gbi_select_language___1")]

summary(pgbi01[pgbi01$eventname == "baseline_year_1_arm_1",]) 
summary(pgbi01[pgbi01$eventname == "1_year_follow_up_y_arm_1",]) 


################### Youth Brief Problem Monitor ###################
bpm = load_instrument("abcd_bpm01", abcd_files_path)

#remove duplicates 
bpm = unique(bpm)
bpm[bpm == "777" | bpm == "999"] = NA


################### Youth NIH Toolbox Positive Affect Items ###################
ytbpai = load_instrument("abcd_ytbpai01", abcd_files_path)
ytbpai[ytbpai == "777" | ytbpai == "999"] = NA


########## Parent Sleep Disturbance Scale for Children (SDS) ###########
sds01 = load_instrument("abcd_sds01",abcd_files_path)

#select variables
sds01 = sds01[,grepl("src|interview|event|sex|sleepdisturb1_p",colnames(sds01))]


########## Parent Diagnostic Interview for DSM-5 Background Items Full (KSAD) ###########
lpksad = load_instrument("abcd_lpksad01",abcd_files_path)

#select variables
lpksad = lpksad[,grepl("src|interview|event|sex|bully",colnames(lpksad))]
lpksad[lpksad == "777" | lpksad == "999"] = NA
lpksad$kbi_p_c_bully_l[lpksad$kbi_p_c_bully_l == 2] = 0



psychopathology = merge(pps01,pgbi01)

write.csv(file = "outputs/psychopathology.csv",x = psychopathology, row.names = F, na = "")

