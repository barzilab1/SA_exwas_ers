library(dplyr)
library(plyr)
source("config.R")
source("utility_fun.R")

########### Parent Medical History Questionnaire (MHX) ###########
mx01 = load_instrument("abcd_mx01",abcd_files_path)

#select variables
mx01 = mx01[,grepl("src|interview|event|sex|(2(a|b|d|g|m)|6(a|l))$",colnames(mx01))]


########### Longitudinal Parent Medical History Questionnaire ###########
lpmh01 = load_instrument("abcd_lpmh01",abcd_files_path)

#select variables
lpmh01 = lpmh01[,grepl("src|interview|event|sex|(2(a|b|d|g|m)|6(a|l))_l$",colnames(lpmh01))]


lpmh01$asthma_composite_l = apply(lpmh01[,c("medhx_2a_l","medhx_6l_l")], 1, function(r) any(r==1)*1)
# summary(lpmh01[lpmh01$eventname == "1_year_follow_up_y_arm_1",])


########### Parent Pubertal Development Scale and Menstrual Cycle Survey History (PDMS) ###########
ppdms = load_instrument("abcd_ppdms01",abcd_files_path)

#"Don't know" will be treated as NA
ppdms[ppdms == "999"] = NA

#remove empty col
ppdms = ppdms[, !colSums(is.na(ppdms)) == nrow(ppdms)]
ppdms$pds_select_language___1 = NULL

#fix scale
ppdms$pds_f5b_p = ppdms$pds_f5b_p - 1
ppdms$pds_f5b_p[ppdms$pds_f5b_p == 3] = 1

ppdms$pds_f6_p[ppdms$pds_f6_p >= 99] = NA
ppdms$menstrualcycle2_p[ppdms$menstrualcycle2_p >= 400] = NA
ppdms$menstrualcycle2_p[ppdms$menstrualcycle2_p <= 3] = NA

describe(ppdms)


########### ABCD Youth Pubertal Development Scale and Menstrual Cycle Survey History (PDMS) ###########
ypdms = load_instrument("abcd_ypdms01",abcd_files_path)

#"Don't know" and "Decline to answer" will be treated as NA
ypdms[ypdms == "777" | ypdms == "999"] = NA
ypdms$pds_device = NULL

#remove empty col
ypdms = ypdms[, !colSums(is.na(ypdms)) == nrow(ypdms)]

#fix scale
ypdms$pds_f5_y = ypdms$pds_f5_y - 1
ypdms$pds_f5_y[ypdms$pds_f5_y == 3] = 1

ypdms$menstrualcycle4_y[ypdms$menstrualcycle4_y >= 2] = NA

describe(ypdms)


########### ABCD Youth Youth Risk Behavior Survey Exercise Physical Activity (YRB) ###########
yrb = load_instrument("abcd_yrb01",abcd_files_path)

#select variables
yrb = yrb[,grepl("src|interview|event|sex|physical_activity(1|2)_y",colnames(yrb))]

#change scale
yrb$physical_activity2_y = as.numeric(as.character(yrb$physical_activity2_y)) - 1



########### Pain Questionnaire ###########
pq01 = load_instrument("abcd_pq01",abcd_files_path)
pq01 = pq01[,grepl("src|interview|event|sex|__b0[1-4]",colnames(pq01))]

summary(pq01)


########### ABCD Parent Sleep Disturbance Scale for Children (SDS) ###########
sds01 = load_instrument("abcd_sds01",abcd_files_path)
sds01 = sds01[,grepl("src|interview|event|b1_p|b4_p|b15_p|b23_p|b25_p",colnames(sds01))]


physicalhealth = rbind.fill(mx01, lpmh01)

physicalhealth_1year = merge(ppdms,ypdms)
write.csv(file = "outputs/physicalhealth_1year.csv",x = physicalhealth_1year, row.names = F, na = "")

physicalhealth_baseline = merge(mx01,yrb)
write.csv(file = "outputs/physicalhealth_baseline.csv",x = physicalhealth_baseline, row.names = F, na = "")


