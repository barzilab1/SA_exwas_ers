library(psych)

source("config.R")
source("utility_fun.R")



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


# collapse same questions, different timepoints
stq$screentime_8_wkdy_br = ifelse(!is.na(stq$screen_wkdy_y), stq$screen_wkdy_y, stq$screentime_8_wkdy_hr)
stq$screentime_1_wkdy_br = ifelse(!is.na(stq$screen1_wkdy_y), stq$screen1_wkdy_y, stq$screentime_1_wkdy_hr)
stq$screentime_2_wkdy_br = ifelse(!is.na(stq$screen2_wkdy_y), stq$screen2_wkdy_y, stq$screentime_2_wkdy_hr)
stq$screentime_5_wkdy_br = ifelse(!is.na(stq$screen4_wkdy_y), stq$screen4_wkdy_y, stq$screentime_5_wkdy_hr)
stq$screentime_6_wkdy_br = ifelse(!is.na(stq$screen5_wkdy_y), stq$screen5_wkdy_y, stq$screentime_6_wkdy_hr)
stq$screentime_7_wknd_br = ifelse(!is.na(stq$screen7_wknd_y), stq$screen7_wknd_y, stq$screentime_7_wknd_hr)
stq$screentime_8_wknd_br = ifelse(!is.na(stq$screen8_wknd_y), stq$screen8_wknd_y, stq$screentime_8_wknd_hr)
stq$screentime_11_wknd_br = ifelse(!is.na(stq$screen10_wknd_y), stq$screen10_wknd_y, stq$screentime_11_wknd_hr)
stq$screentime_12_wknd_br = ifelse(!is.na(stq$screen11_wknd_y), stq$screen11_wknd_y, stq$screentime_12_wknd_hr)
stq$screentime_14_wknd_br = ifelse(!is.na(stq$screen12_wknd_y), stq$screen12_wknd_y, stq$screentime_14_wknd_hr)

stq[,c("screen_wkdy_y", "screentime_8_wkdy_hr")] = NULL
stq[,c("screen1_wkdy_y", "screentime_1_wkdy_hr")] = NULL
stq[,c("screen2_wkdy_y", "screentime_2_wkdy_hr")] = NULL
stq[,c("screen4_wkdy_y", "screentime_5_wkdy_hr")] = NULL
stq[,c("screen5_wkdy_y", "screentime_6_wkdy_hr")] = NULL
stq[,c("screen7_wknd_y", "screentime_7_wknd_hr")] = NULL
stq[,c("screen8_wknd_y", "screentime_8_wknd_hr")] = NULL
stq[,c("screen10_wknd_y", "screentime_11_wknd_hr")] = NULL
stq[,c("screen11_wknd_y", "screentime_12_wknd_hr")] = NULL
stq[,c("screen12_wknd_y", "screentime_14_wknd_hr")] = NULL

View(describe(stq))
stq_wide = get_wide_data(stq)


########### Parent Screen Time Survey ###########
stq01 = load_instrument("stq01",abcd_files_path)
stq01[,c("scrtime_p_select_lang___1","screentime_scrn_media_p__777", "screentime_start_time_p")] = NULL

# clean "refuse to answer"
# TODO: SHORT qestions are well being and not exposome
stq01[stq01 == 777] = NA
temp = stq01[,grep("_(short|online)_",colnames(stq01), value = T)]
temp[temp == 6] = NA
stq01[,grep("_(short|online)_",colnames(stq01), value = T)] = temp

# remove not exposome 
stq01[,grep("_online_",colnames(stq01), value = T)] = NULL
stq01[,grep("_min",colnames(stq01), value = T)] = NULL

# collapse same questions, different timepoints
stq01$screentime_wkdy_hrs = ifelse(!is.na(stq01$screentime1_p_hours), stq01$screentime1_p_hours, stq01$screentime_1_wkdy_hrs_p)
stq01$screentime_wknd_hrs = ifelse(!is.na(stq01$screentime2_p_hours), stq01$screentime2_p_hours, stq01$screentime_1_wknd_hrs_p)
stq01[,c("screentime1_p_hours","screentime2_p_hours","screentime_1_wkdy_hrs_p","screentime_1_wknd_hrs_p")] = NULL

stq01$screentime_device_cell_age_p[stq01$screentime_device_cell_age_p > 15] = NA
stq01$screentime_device_cell_age_p[which(stq01$screentime_device_cell_age_p*12 > (stq01$interview_age + 1))] = NA

#change value range to no -> sometimes -> yes
stq01$screentime_device_cell_no_p = round(stq01$screentime_device_cell_no_p /2 + stq01$screentime_device_cell_no_p %% 2)

View(describe(stq01))
stq01_wide = get_wide_data(stq01)


########### Parent Sleep Disturbance Scale for Children ###########
sds = load_instrument("abcd_sds01",abcd_files_path)
sds$sleep_dis_select_language___1 = NULL
describe(sds)

sds_wide = get_wide_data(sds)

########### Youth Substance Use Attitudes ###########
ysua = load_instrument("abcd_ysua01",abcd_files_path)
ysua[ysua == 999] = NA
ysua[grep("^(ptu|path|phs)",colnames(ysua))] = NULL
describe(ysua)


###########  Youth Substance Use Interview ###########
ysu02 = load_instrument("abcd_ysu02",abcd_files_path)
ysu02_peer_deviance = ysu02[,grep("src|sex|eventname|interview|peer", colnames(ysu02))]
ysu02[, grep("^(path|peer)_|su_today|tlfb_age|_dk$", colnames(ysu02))] = NULL


### combine the 2 instruments
colnames(ysu02_peer_deviance) = sub("(?<=_[1-9])_[^_]+$", "", colnames(ysu02_peer_deviance), perl = T)
colnames(ysua) = sub("_l$", "", colnames(ysua))

peer_deviance = rbind.fill(ysu02_peer_deviance, ysua)
describe(peer_deviance)
peer_deviance_wide = get_wide_data(peer_deviance)


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



