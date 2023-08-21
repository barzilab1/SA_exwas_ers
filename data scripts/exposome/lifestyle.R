library(psych)
library(plyr)
library(fastDummies)
library(janitor)

source("config.R")
source("utility_fun.R")


########### Youth Acculturation Survey ###########
yacc = load_instrument("yacc01",abcd_files_path)
yacc[yacc == 777 | yacc == 999] = NA
yacc = yacc[,grep("src|sex|event|interview|accult_q[1-2]", colnames(yacc)) ]

yacc$accult_q1_y_b = ifelse(yacc$accult_q1_y < 3, 1,0)
yacc$accult_q1_y = NULL

describe(yacc)


########### Youth Risk Behavior Survey Exercise Physical Activity (YRB) ###########
yrb = load_instrument("abcd_yrb01",abcd_files_path)

#change scale
yrb$physical_activity2_y = yrb$physical_activity2_y - 1

describe(yrb)


########### Parent Sports and Activities Involvement Questionnaire ###########
saiq02 = load_instrument("abcd_saiq02",abcd_files_path)
saiq02$sai_p_select_language___1 = NULL

# View(describe(saiq02))
saiq02 = saiq02[,grep("src|interview|sex|event|sai_p_activities___|sai_p_(read|lmusic)$", colnames(saiq02))]


########### Longitudinal Parent Sports and Activities Involvement Questionnaire ###########
lpsaiq = load_instrument("abcd_lpsaiq01",abcd_files_path)
lpsaiq$sai_l_p_select_language___1 = NULL

lpsaiq[lpsaiq == 999] = NA
# View(describe(lpsaiq[lpsaiq$eventname != "3_year_follow_up_y_arm_1",]))

lpsaiq = lpsaiq[,grep("src|interview|sex|event|sai_p_(activities|music_instr___(?!801))|sai_p_(lmusic|read)_l$", colnames(lpsaiq), perl = T)]


### combine the 2 instruments 
colnames(lpsaiq) = sub("_l_", "_", colnames(lpsaiq))
colnames(lpsaiq) = sub("_l$", "", colnames(lpsaiq))
saiq = rbind.fill(saiq02, lpsaiq)
describe(saiq)


########### Child Nutrition Assessment (by Parent) ###########
cna = load_instrument("abcd_cna01",abcd_files_path)
cna$cna_p_select_language___1 = NULL
cna[cna == 999] = NA
describe(cna)

### cna 15-16 - mother pregnancy intake
cna_cross_timepoints = cna[, c("src_subject_id", "sex", "cna_15_p", "cna_16_p")]
cna[, c("cna_15_p", "cna_16_p")] = NULL


########### Youth Block Food Screen ###########
#TODO ask from ABCD for more information about the instrument
# bkfs = load_instrument("abcd_bkfs01",abcd_files_path)
# bkfs$ra_confirm = NULL
# bkfs$bkfs_select_language = NULL
# bkfs[bkfs == 777] = NA

# View(describe(bkfs[bkfs$eventname == "2_year_follow_up_y_arm_1", ]))

#odd values - no response from NIH on this data

########### Youth Screen Time Survey ###########
stq = load_instrument("abcd_stq01",abcd_files_path)

#exclude 3 year follow up and remove empty columns
stq = stq[stq$eventname != "3_year_follow_up_y_arm_1", ]
stq = remove_empty(stq, "cols")

# remove not exposome 
stq[, grep("admin|screentime_phone[1-8]|_(dk|min|(smqa|vgaq)[1-6])$", colnames(stq))] = NULL

# remove outliers
stq[stq == -1] = NA

# clean 777 and 999 that are Refuse to Answer and Don't Know
col_names_clean = colnames(stq)[sapply(stq, function(x){any( x == 777 | x == 999, na.rm = T)})]
col_names_clean = setdiff(col_names_clean, c("screentime_smq_followers", "screentime_smq_following"))
stq[col_names_clean] = lapply(stq[col_names_clean], \(x) ifelse(x == 777 | x == 999, NA, x))

# collapse same questions, different timepoints
stq$screentime_8_wkdy_br = ifelse(!is.na(stq$screen_wkdy_y), stq$screen_wkdy_y, stq$screentime_8_wkdy_hr)
stq$screentime_1_wkdy_br = ifelse(!is.na(stq$screen1_wkdy_y), stq$screen1_wkdy_y, stq$screentime_1_wkdy_hr)
stq$screentime_2_wkdy_br = ifelse(!is.na(stq$screen2_wkdy_y), stq$screen2_wkdy_y, stq$screentime_2_wkdy_hr)
stq$screentime_3_4_wkdy_br = ifelse(!is.na(stq$screen3_wkdy_y), stq$screen3_wkdy_y, rowSums(stq[, c("screentime_3_wkdy_hr" , "screentime_4_wkdy_hr")], na.rm = T))
stq$screentime_5_wkdy_br = ifelse(!is.na(stq$screen4_wkdy_y), stq$screen4_wkdy_y, stq$screentime_5_wkdy_hr)
stq$screentime_6_wkdy_br = ifelse(!is.na(stq$screen5_wkdy_y), stq$screen5_wkdy_y, stq$screentime_6_wkdy_hr)

stq$screentime_7_wknd_br = ifelse(!is.na(stq$screen7_wknd_y), stq$screen7_wknd_y, stq$screentime_7_wknd_hr)
stq$screentime_8_wknd_br = ifelse(!is.na(stq$screen8_wknd_y), stq$screen8_wknd_y, stq$screentime_8_wknd_hr)
stq$screentime_9_10_wknd_br = ifelse(!is.na(stq$screen9_wknd_y), stq$screen9_wknd_y, rowSums(stq[, c("screentime_9_wknd_hr" , "screentime_10_wknd_hr")], na.rm = T))
stq$screentime_11_wknd_br = ifelse(!is.na(stq$screen10_wknd_y), stq$screen10_wknd_y, stq$screentime_11_wknd_hr)
stq$screentime_12_wknd_br = ifelse(!is.na(stq$screen11_wknd_y), stq$screen11_wknd_y, stq$screentime_12_wknd_hr)
stq$screentime_14_wknd_br = ifelse(!is.na(stq$screen12_wknd_y), stq$screen12_wknd_y, stq$screentime_14_wknd_hr)

# as the range between time points is different, use the range of 0-4, with no fraction of time 
screentime_vars = grep("_(wkdy|wknd)_br$", colnames(stq), value = T)
stq[screentime_vars] = lapply(stq[screentime_vars], \(x) ifelse(x > 4, 4, 
                                                                ifelse(x < 1, 0 , x)))

stq[,c("screen_wkdy_y", "screentime_8_wkdy_hr")] = NULL
stq[,c("screen1_wkdy_y", "screentime_1_wkdy_hr")] = NULL
stq[,c("screen2_wkdy_y", "screentime_2_wkdy_hr")] = NULL
stq[,c("screen3_wkdy_y", "screentime_3_wkdy_hr", "screentime_4_wkdy_hr")] = NULL
stq[,c("screen4_wkdy_y", "screentime_5_wkdy_hr")] = NULL
stq[,c("screen5_wkdy_y", "screentime_6_wkdy_hr")] = NULL
stq[,c("screen7_wknd_y", "screentime_7_wknd_hr")] = NULL
stq[,c("screen8_wknd_y", "screentime_8_wknd_hr")] = NULL
stq[,c("screen9_wknd_y", "screentime_9_wknd_hr", "screentime_10_wknd_hr")] = NULL
stq[,c("screen10_wknd_y", "screentime_11_wknd_hr")] = NULL
stq[,c("screen11_wknd_y", "screentime_12_wknd_hr")] = NULL
stq[,c("screen12_wknd_y", "screentime_14_wknd_hr")] = NULL


# number of accounts on each platform: we don;t care if more than 3+ ==> 3 
accounts_number = grep("screentime_smq_(?!(foll|use|acc|sec|soc)).*$", colnames(stq), value = T, perl = T)
stq[accounts_number] = lapply(stq[accounts_number], \(x) ifelse(x > 3, 3, x))
# apply(stq[accounts_number], 2, boxplot)

# create dummy variable for most used social media site  
stq <- dummy_cols(stq, "screentime_smq_use_most", ignore_na = T, remove_selected_columns = T)
# View(stq[,grep("screentime_smq_use_most", colnames(stq))])

# change scale to:  1=Public; 0=Private 
stq$screentime_smq_account = stq$screentime_smq_account %% 2 

#screentime_smq_followers and screentime_smq_following  - according to David
stq$screentime_smq_followers = ifelse(stq$screentime_smq_followers > 5000, NA , stq$screentime_smq_followers)
stq$screentime_smq_following = ifelse(stq$screentime_smq_following > 5000, NA , stq$screentime_smq_following)

# screentime_sq: What do you usually do with your phone when you are ready to go to sleep?
stq <- dummy_cols(stq, "screentime_sq2", ignore_na = T, remove_selected_columns = T)
# View(stq[,grep("screentime_sq2", colnames(stq))])

View(describe(stq))

########### Parent Screen Time Survey ###########
stq01 = load_instrument("stq01",abcd_files_path)
# remove not exposome 
stq01[,c("scrtime_p_select_lang___1","screentime_scrn_media_p__777", "screentime_start_time_p")] = NULL
stq01[,grep("_(short|online)_|_min",colnames(stq01))] = NULL

# clean "refuse to answer"
stq01[stq01 == 777] = NA

# collapse same questions, different timepoints
stq01$screentime_wkdy_hrs = ifelse(!is.na(stq01$screentime1_p_hours), stq01$screentime1_p_hours, stq01$screentime_1_wkdy_hrs_p)
stq01$screentime_wknd_hrs = ifelse(!is.na(stq01$screentime2_p_hours), stq01$screentime2_p_hours, stq01$screentime_1_wknd_hrs_p)
stq01[,c("screentime_wkdy_hrs", "screentime_wknd_hrs")] = lapply(stq01[,c("screentime_wkdy_hrs", "screentime_wknd_hrs")], \(x) ifelse(x > 23, NA, x))
stq01[,c("screentime1_p_hours","screentime2_p_hours","screentime_1_wkdy_hrs_p","screentime_1_wknd_hrs_p")] = NULL

# What kind of cell phone does your child have? 1 = Apple/iPhone/Apple/iPhone; 2 = Android/Android; 3= Other/Otro
stq01 <- dummy_cols(stq01, "screentime_device_cell_p", ignore_na = T, remove_selected_columns = T)

# How old was your child when he/she got their own cell phone? 
stq01$screentime_device_cell_age_p[stq01$screentime_device_cell_age_p > 15] = NA
stq01$screentime_device_cell_age_p[which(stq01$screentime_device_cell_age_p*12 > stq01$interview_age)] = NA

#change value range to no -> sometimes -> yes
stq01$screentime_device_cell_no_p_e = round(stq01$screentime_device_cell_no_p /2 + stq01$screentime_device_cell_no_p %% 2)

#Do you suspect that your child has social media accounts that you are unaware of?
stq01$screentime_secs_media_p = NULL

View(describe(stq01))

########### Parent Sleep Disturbance Scale for Children ###########
sds = load_instrument("abcd_sds01",abcd_files_path)
sds$sleep_dis_select_language___1 = NULL

sds$sleepdisturb1_p_b = ifelse(sds$sleepdisturb1_p == 1, 1, 0)
sds = sds[,grep("src|inter|event|sex|_b$", colnames(sds))]
describe(sds)


###########  Youth Substance Use Interview (baseline) ###########
ysu02 = load_instrument("abcd_ysu02",abcd_files_path)
ysu02[, grep("^(path|peer)_|su_today|tlfb_age|_dk$", colnames(ysu02))] = NULL

#remove columns with too much missing data
ysu02 = ysu02[, colSums(is.na(ysu02)) < 6000]

View(describe(ysu02))


###########  Youth Substance Use Introduction and Patterns ###########
ysuip = load_instrument("abcd_ysuip01",abcd_files_path)
ysuip$xskipout_device = NULL
ysuip[, grep("tlfb_age", colnames(ysuip))] = NULL

#exclude 3 year follow up and remove empty columns
ysuip = ysuip[ysuip$eventname != "3_year_follow_up_y_arm_1", ]
ysuip = remove_empty(ysuip, "cols")

#remove columns with too much missing data
ysuip = ysuip[, colSums(is.na(ysuip)) < 6000]

View(describe(ysuip))


### combine the 2 instruments 
colnames(ysuip) = sub("_l$", "", colnames(ysuip))
colnames(ysuip) = sub("_l_", "_", colnames(ysuip))
colnames(ysuip) = sub("_lsd_", "_hall_", colnames(ysuip))

ind_to_fix = grep("isip_(.*)_2", colnames(ysu02))
colnames(ysu02)[ind_to_fix] = sub("_2", "", colnames(ysu02)[ind_to_fix])

ysu = rbind.fill(ysu02, ysuip)

#remove columns with sd = 0
zero_sd_cols = sapply(ysu, \(x) is.numeric(x) && sd(x, na.rm = T) == 0)
ysu = ysu[, !zero_sd_cols]

# What type of beverage was that? [he largest amount of a caffeinated beverage you drank in ONE DAY]
# 0 = Not Applicable; 1 = coffee; 2 = espresso; 3 = tea; 4 = soda; 5 = energy drink
ysu <- dummy_cols(ysu, "caff_max_type", ignore_na = T, remove_selected_columns = T)
ysu$caff_max_type_0 = NULL

View(describe(ysu))

ysu[,c("interview_date", "interview_age")] = NULL

########### merge all tables ###########
lifestyle  = merge(yacc, yrb, all = T)
lifestyle  = merge(lifestyle, saiq, all = T)
lifestyle  = merge(lifestyle, cna, all = T)
# lifestyle  = merge(lifestyle, bkfs, all = T)
lifestyle  = merge(lifestyle, stq, all = T)
lifestyle  = merge(lifestyle, stq01, all = T)
lifestyle  = merge(lifestyle, sds, all = T)
lifestyle  = merge(lifestyle, ysu, all = T)

# remove 3 year follow up and empty columns
lifestyle = lifestyle[lifestyle$eventname != "3_year_follow_up_y_arm_1", ]
lifestyle = remove_empty(lifestyle, "cols")
zero_sd_cols = sapply(lifestyle, \(x) is.numeric(x) && sd(x, na.rm = T) == 0)
lifestyle = lifestyle[,!zero_sd_cols]


# add pregnancy intake  to all time points 
lifestyle = merge(lifestyle, cna_cross_timepoints, all.x = T)

write.csv(lifestyle, "data/lifestyle_item.csv", row.names = F, na = "")






