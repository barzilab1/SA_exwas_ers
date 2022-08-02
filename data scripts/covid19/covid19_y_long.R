library(psych)
library(plyr)

source("config.R")
source("utility_fun.R")


covidy_r1 = load_instrument("yabcdcovid19questionnaire01",abcd_covid_r1_files_path)
covidy_r2 = load_instrument("yabcdcovid19questionnaire01",abcd_covid_r2_files_path)
covidy_r3 = load_instrument("yabcdcovid19questionnaire01",abcd_covid_r3_files_path)

covidy = rbind.fill(covidy_r1, covidy_r2, covidy_r3)
covidy[covidy == 777 | covidy == 999] = NA

# new variable to use in reshape from long to wide format
covidy$timepoint = regmatches(covidy$eventname, regexpr("cv[1-7]", covidy$eventname))


####################################
###### outputs
####################################

###### 1. Sadness Scale
sadness_scale = grep("felt_", colnames(covidy), value = T)
sadness_scale = sadness_scale[! sadness_scale %in% c("felt_angry_cv", "felt_nervous_cv", "felt_scared_cv")] 

# sum only if there are 8 answers 
covidy$felt_sad_cv_raw_tot_bar = rowSums(covidy[,sadness_scale])


###### 2. Substance

# convert to binary feature 
covidy$su_alcohol_binary = ifelse(covidy$su_alcohol_cv > 1, 1, 0)
covidy$su_alcohol_binary[is.na(covidy$su_alcohol_cv)] = NA

covidy$su_liquids_binary = ifelse(covidy$su_liquids_cv > 1, 1, 0)
covidy$su_liquids_binary[is.na(covidy$su_liquids_cv)] = NA

covidy$su_medication_binary = ifelse(covidy$su_medication_cv > 1, 1, 0)
covidy$su_medication_binary[is.na(covidy$su_medication_cv)] = NA

covidy$su_meth_binary = ifelse(covidy$su_meth_cv > 1, 1, 0)
covidy$su_meth_binary[is.na(covidy$su_meth_cv )] = NA

covidy$su_other_cv = covidy$su_liquids_binary + covidy$su_medication_binary + covidy$su_meth_binary

# check if the variables can be combined
xcor <- polychoric(covidy[ ,grep("binary|mj_use", colnames(covidy), value = T) ])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]

covidy$su_total_cv = rowSums(covidy[ ,grep("binary|mj_use", colnames(covidy), value = T) ])



###### 3. Perceived Stress Scale
covidy$pstr_cv_raw_tot_bar = 10 - covidy$pstr_confidence_p_cv - covidy$pstr_way_p_cv + covidy$pstr_overcome_p_cv + covidy$pstr_unable_control_cv



###### 4. Mental health
# nothing to clean


####################################
###### exposures
####################################

exposures = grep("exercise|money|outdoor|parent_monitor|pstr|routine|screentime_wknd|strle|exp_rac|witness_rac|^worr(ied|y_y)_cv", colnames(covidy), value = T)


###### parents monitor
parents_monitor = grep("parent_monitor", colnames(covidy), value = T)

# check if the variables can be combined
xcor <- polychoric(covidy[ ,parents_monitor ])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]

covidy$parent_monitor_tot_bar = rowSums(covidy[,parents_monitor])
  

###### stressful life event 
# stressful_events = grep("strle", colnames(covidy), value = T)
# 
# # check if the variables can be combined
# xcor <- polychoric(covidy[ ,stressful_events])$rho
# VSS.scree(xcor)
# eigen(xcor)$values[1]/eigen(xcor)$values[2]
# 
# covidy$strle_tot_bar = rowSums(covidy[,stressful_events])




covidy_long = covidy[,c("src_subject_id", "timepoint", "interview_date", "interview_age",
                        sadness_scale, "felt_sad_cv_raw_tot_bar", "enjoy_school_y_cv",
                        grep("binary|mj_use|pstr|mental|parent_monitor|strle|su_total_cv|parent_monitor_tot_bar", colnames(covidy), value = T),
                        exposures)]

write.csv(covidy_long, "outputs/covidy_long.csv", row.names=F, na = "")

