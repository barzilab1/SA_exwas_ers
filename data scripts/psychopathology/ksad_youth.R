
source("config.R")
source("utility_fun.R")

ksad_y = load_instrument("abcd_ksad501",abcd_files_path)

#555 and 888 will be treated as NA
ksad_y[ksad_y == "888" | ksad_y == "555"] = NA


#################### internal diagnosis ####################
#all kids diagnosis
ksad_y_diagnosis = ksad_y[,grepl("src|inter|event|sex|_((8[3-4][0-9])|863|864|869|870|(91[1-4])|969|970)_t",colnames(ksad_y))]

#remove empty col
ksad_y_diagnosis = ksad_y_diagnosis[,!colSums(is.na(ksad_y_diagnosis)) == nrow(ksad_y_diagnosis)]

#create diagnosis variables
#if 0 or NA then 0
ksad_y_diagnosis$diagnosis_depression_y = apply(ksad_y_diagnosis[,grepl("ksads_1_.*_t", colnames(ksad_y_diagnosis))], 1, function(x) {any(x == 1)*1})
ksad_y_diagnosis$diagnosis_depression_y = ifelse( (is.na(ksad_y_diagnosis$diagnosis_depression_y) &
                                                     (apply(ksad_y_diagnosis[,which(grepl("ksads_1_.*_t", colnames(ksad_y_diagnosis)))], 1, function(x) {any(x == 0)}))),
                                                  0, ksad_y_diagnosis$diagnosis_depression_y)



summary(ksad_y_diagnosis[ksad_y_diagnosis$eventname == "baseline_year_1_arm_1",])
summary(ksad_y_diagnosis[ksad_y_diagnosis$eventname == "1_year_follow_up_y_arm_1",])
summary(ksad_y_diagnosis[ksad_y_diagnosis$eventname == "2_year_follow_up_y_arm_1",])

write.csv(file = "data/ksad_y_diagnosis.csv", x = ksad_y_diagnosis, row.names=F, na = "")



write.csv(file = "data/ksad_y_symptoms.csv",x = ksad_y_symptoms, row.names=F, na = "")

