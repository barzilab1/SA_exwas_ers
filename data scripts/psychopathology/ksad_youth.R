#note: 2-year follow up: there are new features available that weren't tag yet by ran as internal/external and dsm5

source("config.R")
source("utility_fun.R")

ksad_y = load_instrument("abcd_ksad501",abcd_files_path)

#555 and 888 will be treated as NA
ksad_y[ksad_y == "888" | ksad_y == "555"] = NA


# ksad externalizing symptoms
# externalize_ksad_y = ksad_y[,which(grepl("^(src|inter|event|sex|ksads_([1-3]|8|10|22)_(8|9)[0-9][0-9])", colnames(ksad_y)))]

#################### internal diagnosis ####################
#all kids diagnosis
ksad_y_diagnosis = ksad_y[,grepl("src|inter|event|sex|_((8[3-4][0-9])|863|864|869|870|(91[1-4])|969|970)_t",colnames(ksad_y))]

#remove empty col
ksad_y_diagnosis = ksad_y_diagnosis[,!colSums(is.na(ksad_y_diagnosis)) == nrow(ksad_y_diagnosis)]

#create diagnosis variables
#if 0 or NA then 0
ksad_y_diagnosis$diagnosis_bipolar_y = apply(ksad_y_diagnosis[,grepl("ksads_2_.*_t", colnames(ksad_y_diagnosis))], 1, function(x) {any(x == 1)*1})
ksad_y_diagnosis$diagnosis_bipolar_y = ifelse( (is.na(ksad_y_diagnosis$diagnosis_bipolar_y) &
                                                    (apply(ksad_y_diagnosis[,which(grepl("ksads_2_.*_t", colnames(ksad_y_diagnosis)))], 1, function(x) {any(x == 0)}))),
                                               0, ksad_y_diagnosis$diagnosis_bipolar_y)


ksad_y_diagnosis$diagnosis_depression_y = apply(ksad_y_diagnosis[,grepl("ksads_1_.*_t", colnames(ksad_y_diagnosis))], 1, function(x) {any(x == 1)*1})
ksad_y_diagnosis$diagnosis_depression_y = ifelse( (is.na(ksad_y_diagnosis$diagnosis_depression_y) &
                                                       (apply(ksad_y_diagnosis[,which(grepl("ksads_1_.*_t", colnames(ksad_y_diagnosis)))], 1, function(x) {any(x == 0)}))),
                                                  0, ksad_y_diagnosis$diagnosis_depression_y)


ksad_y_diagnosis$diagnosis_DMDD_y = ksad_y_diagnosis$ksads_3_848_t



ksad_y_diagnosis$diagnosis_anxiety_y = apply(ksad_y_diagnosis[,grepl("ksads_(8|10)_.*_t", colnames(ksad_y_diagnosis))], 1, function(x) {any(x == 1)*1})
ksad_y_diagnosis$diagnosis_anxiety_y = ifelse( (is.na(ksad_y_diagnosis$diagnosis_anxiety_y) &
                                                    (apply(ksad_y_diagnosis[,which(grepl("ksads_(8|10)_.*_t", colnames(ksad_y_diagnosis)))], 1, function(x) {any(x == 0)}))),
                                               0, ksad_y_diagnosis$diagnosis_anxiety_y)


ksad_y_diagnosis$diagnosis_sleep_y = apply(ksad_y_diagnosis[,grepl("ksads_22_.*_t", colnames(ksad_y_diagnosis))], 1, function(x) {any(x == 1)*1})
ksad_y_diagnosis$diagnosis_sleep_y = ifelse( (is.na(ksad_y_diagnosis$diagnosis_sleep_y) &
                                                  (apply(ksad_y_diagnosis[,which(grepl("ksads_22_.*_t", colnames(ksad_y_diagnosis)))], 1, function(x) {any(x == 0)}))),
                                             0, ksad_y_diagnosis$diagnosis_sleep_y)


ksad_y_diagnosis$diagnosis_ocd_y = apply(ksad_y_diagnosis[,grepl("ksads_11_.*_t", colnames(ksad_y_diagnosis))], 1, function(x) {any(x == 1)*1})
ksad_y_diagnosis$diagnosis_ocd_y = ifelse( (is.na(ksad_y_diagnosis$diagnosis_ocd_y) &
                                                (apply(ksad_y_diagnosis[,which(grepl("ksads_11_.*_t", colnames(ksad_y_diagnosis)))], 1, function(x) {any(x == 0)}))),
                                           0, ksad_y_diagnosis$diagnosis_ocd_y)


ksad_y_diagnosis$diagnosis_ptsd_y = apply(ksad_y_diagnosis[,grepl("ksads_21_.*_t", colnames(ksad_y_diagnosis))], 1, function(x) {any(x == 1)*1})
ksad_y_diagnosis$diagnosis_ptsd_y = ifelse( (is.na(ksad_y_diagnosis$diagnosis_ptsd_y) &
                                                 (apply(ksad_y_diagnosis[,which(grepl("ksads_21_.*_t", colnames(ksad_y_diagnosis)))], 1, function(x) {any(x == 0)}))),
                                            0, ksad_y_diagnosis$diagnosis_ptsd_y)


summary(ksad_y_diagnosis[ksad_y_diagnosis$eventname == "baseline_year_1_arm_1",])
summary(ksad_y_diagnosis[ksad_y_diagnosis$eventname == "1_year_follow_up_y_arm_1",])
summary(ksad_y_diagnosis[ksad_y_diagnosis$eventname == "2_year_follow_up_y_arm_1",])

write.csv(file = "outputs/ksad_y_diagnosis.csv", x = ksad_y_diagnosis, row.names=F, na = "")


#################### internal symptoms ####################

ksad_y_symptoms = ksad_y[,grepl("src|inter|event|sex|ksads_(1|8|10)_([1-9]|[2-4][0-9]|1[5-8][0-9]|3[0-3][0-9])_|ksads_22_14[1-2]_",colnames(ksad_y))]
ksad_y_symptoms = ksad_y_symptoms[,!(colnames(ksad_y_symptoms) %in% c("ksads_1_185_t","ksads_1_186_t","ksads_1_187_t","ksads_1_188_t","ksads_8_30_t" ,"ksads_10_330_t"))]

summary(ksad_y_symptoms[ksad_y_symptoms$eventname == "baseline_year_1_arm_1",])
summary(ksad_y_symptoms[ksad_y_symptoms$eventname == "1_year_follow_up_y_arm_1",])


write.csv(file = "outputs/ksad_y_symptoms.csv",x = ksad_y_symptoms, row.names=F, na = "")

