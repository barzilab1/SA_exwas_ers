
source("config.R")
source("utility_fun.R")

################### cbcls ################### 
cbcls01 = load_instrument("abcd_cbcls01",abcd_files_path)

#get the t scores
cbcls_t_score = cbcls01[, grepl("^(src|interview|event|sex)|_t$", colnames(cbcls01))]

summary(cbcls_t_score[cbcls_t_score$eventname == "1_year_follow_up_y_arm_1",]) 


################### Sum Scores Mental Health Youth ################### 
mhy = load_instrument("abcd_mhy02", abcd_files_path)

# mhy = mhy[,grepl("^(src|interview|event|sex|ple_y|pps_)",colnames(mhy))]

#remove nt (Number Total Questions) and nm (Number Missing Answers)
mhy = mhy[,!grepl("^(sup_|gish_)|_(nm|nt)$",colnames(mhy))]

summary(mhy[mhy$eventname == "baseline_year_1_arm_1" ,])
summary(mhy[mhy$eventname == "2_year_follow_up_y_arm_1" ,])

library(data.table)
setDT(mhy)
mhy[,bully_vic:= peq_ss_relational_victim +peq_ss_reputation_victim +peq_ss_overt_victim]
mhy[,bully_aggs:= peq_ss_relational_aggs+peq_ss_reputation_aggs+peq_ss_overt_aggression]

mhy[,bully_vic_90_q:= {
  vic_90_q = quantile(bully_vic, prob = seq(0, 1, length = 11), na.rm = T)["90%"]
  fcase(
    bully_vic > vic_90_q, 1,
    bully_vic <= vic_90_q, 0,
    default = NA)
}]

mhy[,bully_aggs_90_q:= {
  aggs_90_q = quantile(bully_aggs, prob = seq(0, 1, length = 11), na.rm = T)["90%"]
  fcase(
    bully_aggs > aggs_90_q, 1,
    bully_aggs <= aggs_90_q, 0,
    default = NA)
}]




################### Sum Scores Mental Health Parent ################### 
mhp02 = load_instrument("abcd_mhp02", abcd_files_path)
mhp02 = mhp02[,grepl("^(src|interview|event|sex)|(score)$",colnames(mhp02))]

summary(mhp02[mhp02$eventname == "baseline_year_1_arm_1" ,])
summary(mhp02[mhp02$eventname == "1_year_follow_up_y_arm_1" ,])


################### Youth Summary Scores BPM and POA ################### 
yssbpm01 = load_instrument("abcd_yssbpm01", abcd_files_path)
yssbpm01 = yssbpm01[,grepl("^(src|interv|event|sex)|_(r|t|mean|sum)$", colnames(yssbpm01))]


psychopathology_sum_scores = merge(mhy,mhp02)


write.csv(file = "outputs/psychopathology_sum_scores.csv",x = psychopathology_sum_scores, row.names = F, na = "")

