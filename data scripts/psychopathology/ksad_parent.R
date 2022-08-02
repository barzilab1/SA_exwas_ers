#note: 2-year follow up: there are new features available that weren't tag yet by ran as internal/external and dsm5

source("config.R")
source("utility_fun.R")

ksad_p <- load_instrument("abcd_ksad01", abcd_files_path)

#555 and 888 will be treated as NA
ksad_p[ksad_p == "888" | ksad_p == "555"] <- NA

# ksad_p <- "~/Box Sync/2-ABCD Data Files/ABCD data/4.0/abcd_ksad01.txt" %>% read.csv(sep <- '\t',header <- TRUE, row.names=NULL, na.string <- c("","NA"), check.names=FALSE)
# ksads_eating_disorder_diagnosis


#################### externalizing Symptoms ####################
externalize_ksad_p = ksad_p[,which(grepl("^(src|inter|event|sex|ksads_1[4-6]_([7-9][0-9]|10[0-9]|4[0-6][0-9]|39[4-9])_)", colnames(ksad_p)))]
#remove not relevant Symptoms
#ksads_15_446_p ksads_15_445_p ksads_15_438_p ksads_15_97_p ksads_15_431_p ksads_14_429_p ksads_14_430_p
externalize_ksad_p = externalize_ksad_p[,!( grepl("ksads_(15_(97|44[5-6]|43(1|8)))|(14_429|430)_p", colnames(externalize_ksad_p)))]


#ADHD
#for each pair, create one variable to represent the adhd symptoms

pairs_items = c(394 , 410)
pairs_items = rbind(pairs_items, c(395 , 411))
pairs_items = rbind(pairs_items, c(396 , 412))
pairs_items = rbind(pairs_items, c(397 , 413))
pairs_items = rbind(pairs_items, c(398 , 414))
pairs_items = rbind(pairs_items, c(399 , 415))
pairs_items = rbind(pairs_items, c(400 , 416))
pairs_items = rbind(pairs_items, c(401 , 417))
pairs_items = rbind(pairs_items, c(402 , 418))
pairs_items = rbind(pairs_items, c(403 , 419))
pairs_items = rbind(pairs_items, c(404 , 420))
pairs_items = rbind(pairs_items, c(405 , 421))
pairs_items = rbind(pairs_items, c(406 , 422))
pairs_items = rbind(pairs_items, c(407 , 423))
pairs_items = rbind(pairs_items, c(408 , 424))
pairs_items = rbind(pairs_items, c(409 , 425))


for(i in 1:dim(pairs_items)[1]){
    new_col_name = paste0("temp_adhd_",i)
    e_1 = paste0("ksads_14_",pairs_items[i,1],"_p")
    e_2 = paste0("ksads_14_",pairs_items[i,2],"_p")

    externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
    externalize_ksad_p[,new_col_name] = ifelse( ( is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0)) ),
                                                0, externalize_ksad_p[,new_col_name])
    print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}
#add "sustaining attention" symptom
externalize_ksad_p$temp_adhd_17 = apply(externalize_ksad_p[,grepl("ksads_14_(7[6-9])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
summary(externalize_ksad_p[,grepl("temp_adhd_17|ksads_14_(7[6-9])_p", colnames(externalize_ksad_p))])

#add "Easily distracted" symptom
externalize_ksad_p$temp_adhd_18 = apply(externalize_ksad_p[,grepl("ksads_14_(8[0-3])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
summary(externalize_ksad_p[,grepl("temp_adhd_18|ksads_14_(8[0-3])_p", colnames(externalize_ksad_p))])

#add "Difficulty remaining seated" symptom
externalize_ksad_p$temp_adhd_19 = apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
externalize_ksad_p$temp_adhd_19 = ifelse( ( is.na(externalize_ksad_p$temp_adhd_19) & (apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 0)})) ),
                                          0, externalize_ksad_p$temp_adhd_19)
summary(externalize_ksad_p[,grepl("temp_adhd_19|ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))])

#add "Impulsivity" symptom
externalize_ksad_p$temp_adhd_20 = (externalize_ksad_p$ksads_14_88_p == 1 | externalize_ksad_p$ksads_14_89_p == 1 |externalize_ksad_p$ksads_14_90_p == 1) *1
summary(externalize_ksad_p[,c("temp_adhd_20", "ksads_14_88_p", "ksads_14_89_p", "ksads_14_90_p")])


#create summary for adhd symptoms
externalize_ksad_p$ksads_ADHD_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("t", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_ADHD_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_adhd_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == dim(pairs_items)[1]] = NA


#ADHD exclude attention
#for each pair, create one variable to represent the adhd symptoms excluding attention
pairs_items = c(401 , 417)
pairs_items = rbind(pairs_items, c(402 , 418))
pairs_items = rbind(pairs_items, c(403 , 419))
pairs_items = rbind(pairs_items, c(404 , 420))
pairs_items = rbind(pairs_items, c(405 , 421))
pairs_items = rbind(pairs_items, c(406 , 422))
pairs_items = rbind(pairs_items, c(407 , 423))
pairs_items = rbind(pairs_items, c(408 , 424))


for(i in 1:dim(pairs_items)[1]){
    new_col_name = paste0("temp_adhd_ex_",i)
    e_1 = paste0("ksads_14_",pairs_items[i,1],"_p")
    e_2 = paste0("ksads_14_",pairs_items[i,2],"_p")

    externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
    externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))),
                                                0, externalize_ksad_p[,new_col_name])
    print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}

#add "Difficulty remaining seated" symptom
externalize_ksad_p$temp_adhd_ex_9 = apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
externalize_ksad_p$temp_adhd_ex_9 = ifelse( ( is.na(externalize_ksad_p$temp_adhd_ex_9) & (apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 0)})) ),
                                            0, externalize_ksad_p$temp_adhd_ex_9)
summary(externalize_ksad_p[,grepl("temp_adhd_ex_9|ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))])

#add "Impulsivity" symptom
externalize_ksad_p$temp_adhd_ex_10 = (externalize_ksad_p$ksads_14_88_p == 1 | externalize_ksad_p$ksads_14_89_p == 1 |externalize_ksad_p$ksads_14_90_p == 1) *1
summary(externalize_ksad_p[,c("temp_adhd_ex_10", "ksads_14_88_p", "ksads_14_89_p", "ksads_14_90_p")])

#create summary for adhd symptoms excluding attention
externalize_ksad_p$ksads_ADHD_exclude_attention_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_adhd_ex_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_ADHD_exclude_attention_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_adhd_ex_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == dim(pairs_items)[1]] = NA
# View(externalize_ksad_p[,which(grepl("adhd_ex", colnames(externalize_ksad_p), ignore.case=TRUE))])


#ODD
#for each pair, create one variable to represent the ODD symptoms
pairs_items = c(91, 92)
pairs_items = rbind(pairs_items, c(93 , 94))
pairs_items = rbind(pairs_items, c(95 , 96))
pairs_items = rbind(pairs_items, c(432 , 439))
pairs_items = rbind(pairs_items, c(433 , 440))
pairs_items = rbind(pairs_items, c(434 , 441))
pairs_items = rbind(pairs_items, c(435 , 442))
pairs_items = rbind(pairs_items, c(436 , 443))
pairs_items = rbind(pairs_items, c(437 , 444))

for(i in 1:dim(pairs_items)[1]){
    new_col_name = paste0("temp_odd_",i)
    e_1 = paste0("ksads_15_",pairs_items[i,1],"_p")
    e_2 = paste0("ksads_15_",pairs_items[i,2],"_p")

    externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
    externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))),
                                                0, externalize_ksad_p[,new_col_name])
    print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}

#create summary for ODD
externalize_ksad_p$ksads_ODD_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_odd_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_ODD_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_odd_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == dim(pairs_items)[1]] = NA
# View(adhd_ksad_p[is.na(adhd_ksad_p$ksads_15_95_p),which(grepl("odd", colnames(adhd_ksad_p), ignore.case=TRUE))])


#Conduct
pairs_items = c(98 , 99)
pairs_items = rbind(pairs_items, c(100, 101))
pairs_items = rbind(pairs_items, c(102 , 103))
pairs_items = rbind(pairs_items, c(104 , 105))
pairs_items = rbind(pairs_items, c(106 , 107))
pairs_items = rbind(pairs_items, c(447 , 448))
pairs_items = rbind(pairs_items, c(449 , 450))
pairs_items = rbind(pairs_items, c(451 , 452))
pairs_items = rbind(pairs_items, c(453 , 454))
pairs_items = rbind(pairs_items, c(455 , 456))
pairs_items = rbind(pairs_items, c(457 , 458))
pairs_items = rbind(pairs_items, c(459 , 460))
pairs_items = rbind(pairs_items, c(461 , 462))
pairs_items = rbind(pairs_items, c(463 , 464))
pairs_items = rbind(pairs_items, c(465 , 466))

for(i in 1:dim(pairs_items)[1]){
    new_col_name = paste0("temp_conduct_",i)
    e_1 = paste0("ksads_16_",pairs_items[i,1],"_p")
    e_2 = paste0("ksads_16_",pairs_items[i,2],"_p")

    externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
    externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))),
                                                0, externalize_ksad_p[,new_col_name])
    print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}

#create summary for Conduct
externalize_ksad_p$ksads_CONDUCT_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_conduct_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_CONDUCT_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_conduct_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == dim(pairs_items)[1]] = NA
# View(adhd_ksad_p[is.na(adhd_ksad_p$ksads_16_102_p),which(grepl("conduct", colnames(adhd_ksad_p), ignore.case=TRUE))])


externalize_ksad_p$ksads_externalizing_symptoms_sum = externalize_ksad_p$ksads_ADHD_symptoms_sum + externalize_ksad_p$ksads_ODD_symptoms_sum + externalize_ksad_p$ksads_CONDUCT_symptoms_sum
externalize_ksad_p$ksads_externalizing_exclude_attentation_symptoms_sum = externalize_ksad_p$ksads_ADHD_exclude_attention_symptoms_sum + externalize_ksad_p$ksads_ODD_symptoms_sum + externalize_ksad_p$ksads_CONDUCT_symptoms_sum

#remove temp
externalize_ksad_p = externalize_ksad_p[,!( grepl("temp_", colnames(externalize_ksad_p)))]



summary(externalize_ksad_p[externalize_ksad_p$eventname == "baseline_year_1_arm_1",])
summary(externalize_ksad_p[externalize_ksad_p$eventname == "1_year_follow_up_y_arm_1",])

write.csv(file = "outputs/externalize_ksad_symptoms_p.csv",x = externalize_ksad_p, row.names=F, na = "")





#################### externalizing Diagnosis ####################
#unlike suicide, here if 0 or NA then 0
externalize_diagnosis <- ksad_p[,which(grepl("^(src|inter|event|sex|ksads_1[3-6]_(8|9)(0|2|3|4|5|9)[0-9]_)", colnames(ksad_p)))]

externalize_diagnosis$ksads_ADHD_Diagnosis <- apply(externalize_diagnosis[,which(grepl("ksads_14_(85[3-6])", colnames(externalize_diagnosis)))], 1, function(x) {any(x == 1)*1})
externalize_diagnosis$ksads_ADHD_Diagnosis = ifelse( (is.na(externalize_diagnosis$ksads_ADHD_Diagnosis) &
                                                          (apply(externalize_diagnosis[,which(grepl("ksads_14_(85[3-6])", colnames(externalize_diagnosis)))], 1, function(x) {any(x == 0)}))),
                                                     0, externalize_diagnosis$ksads_ADHD_Diagnosis)


externalize_diagnosis$ksads_ODD_Diagnosis <- apply(externalize_diagnosis[,which(grepl("ksads_15_(901|902)", colnames(externalize_diagnosis)))],1 , function(x) {any(x == 1)*1})
externalize_diagnosis$ksads_ODD_Diagnosis = ifelse( (is.na(externalize_diagnosis$ksads_ODD_Diagnosis) &
                                                         (apply(externalize_diagnosis[,which(grepl("ksads_15_(901|902)", colnames(externalize_diagnosis)))], 1, function(x) {any(x == 0)}))),
                                                    0, externalize_diagnosis$ksads_ODD_Diagnosis)


externalize_diagnosis$ksads_CONDUCT_Diagnosis <- apply(externalize_diagnosis[,which(grepl("ksads_16_(897|898|899|900)", colnames(externalize_diagnosis)))], 1, function(x) {any(x == 1)*1})
externalize_diagnosis$ksads_CONDUCT_Diagnosis = ifelse( (is.na(externalize_diagnosis$ksads_CONDUCT_Diagnosis) &
                                                             (apply(externalize_diagnosis[,which(grepl("ksads_16_(897|898|899|900)", colnames(externalize_diagnosis)))], 1, function(x) {any(x == 0)}))),
                                                        0, externalize_diagnosis$ksads_CONDUCT_Diagnosis)

externalize_diagnosis$ksads_any_externalizing_diagnosis <- (externalize_diagnosis$ksads_ADHD_Diagnosis | externalize_diagnosis$ksads_ODD_Diagnosis | externalize_diagnosis$ksads_CONDUCT_Diagnosis)*1

# externalize_diagnosis$ksads_Alcohol_Substance_Diagnosis <- apply(externalize_diagnosis[,which(grepl("ksads_(19|20)_8[7-9][0-9]", colnames(externalize_diagnosis)))], 1, function(x) {any(x == 1)*1})
# print(summary(externalize_diagnosis[,which(grepl("ksads_Alcohol_Substance_Diagnosis|ksads_(19|20)_8[7-9][0-9]", colnames(externalize_diagnosis)))]))


summary(externalize_diagnosis[externalize_diagnosis$eventname == "baseline_year_1_arm_1",])
summary(externalize_diagnosis[externalize_diagnosis$eventname == "1_year_follow_up_y_arm_1",])
summary(externalize_diagnosis[externalize_diagnosis$eventname == "2_year_follow_up_y_arm_1",])

write.csv(file = "outputs/externalize_ksad_diagnosis_p.csv",x = externalize_diagnosis, row.names=F, na = "")

#################### Binge Eating Disorder Diagnosis ####################

eating_disorder_diagnosis <- ksad_p[,which(grepl("^(src|inter|event|sex|ksads_13_(939|938|929|934|933|932|931|930|936|935|937|940|943|942|944|941)_)", colnames(ksad_p)))]

eating_disorder_diagnosis$ksads_eating_disorder_diagnosis <- apply(eating_disorder_diagnosis[,which(grepl("ksads", colnames(eating_disorder_diagnosis)))], 1, function(x) {any(x == 1)*1})
eating_disorder_diagnosis$ksads_eating_disorder_diagnosis = ifelse( (is.na(eating_disorder_diagnosis$ksads_eating_disorder_diagnosis) &
                                                                         (apply(eating_disorder_diagnosis[,which(grepl("ksads", colnames(eating_disorder_diagnosis)))], 1, function(x) {any(x == 0)}))),
                                                                    0, eating_disorder_diagnosis$ksads_eating_disorder_diagnosis)

write.csv(file = "outputs/binge_eating_ksad_diagnosis_p.csv", x = eating_disorder_diagnosis, row.names=F, na = "")


#################### PTSD ####################
#unlike suicide, here if 0 or NA then 0
ptsd = ksad_p[,which(grepl("^(src|inter|event|sex|ksads_21)", colnames(ksad_p)))]

ptsd$ksads_PTSD_diagnosis = (ptsd$ksads_21_921_p | ptsd$ksads_21_922_p)*1
ptsd$ksads_trauma_diagnosis = (ptsd$ksads_21_923_p | ptsd$ksads_21_924_p)*1
ptsd$ksads_any_trauma_diagnosis = (ptsd$ksads_PTSD_diagnosis | ptsd$ksads_trauma_diagnosis)*1
ptsd$ksads_any_trauma_diagnosis = ifelse(is.na(ptsd$ksads_any_trauma_diagnosis) & (ptsd$ksads_trauma_diagnosis == 0 | ptsd$ksads_PTSD_diagnosis ==0),
                                         0, ptsd$ksads_any_trauma_diagnosis)


ptsd$ksads_ptsd_nightmares = (ptsd$ksads_21_137_p | ptsd$ksads_21_138_p)*1
ptsd$ksads_ptsd_avoidance = (ptsd$ksads_21_135_p | ptsd$ksads_21_136_p)*1
ptsd$ksads_ptsd_distress = (ptsd$ksads_21_139_p | ptsd$ksads_21_140_p)*1
ptsd$ksads_ptsd_symptoms_summary = rowSums(ptsd[,c("ksads_ptsd_nightmares", "ksads_ptsd_avoidance","ksads_ptsd_distress")])






