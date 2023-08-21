##############################################
#' most of the code below is adjusted from
#' the abcd official github:
#' https://github.com/ABCD-STUDY/analysis-nda
##############################################

library(data.table)

source("config.R")
source("utility_fun.R")

demographics_set = load_instrument("pdem02",abcd_files_path)
demographics_set[demographics_set == 777 | demographics_set == 999] = NA

########### rearrange data ###########
###convert variables names to be more readable
demographics_set = data.table(demographics_set)

########### sex
#convert the NIH sex at birth (equal to demo_sex_v2)
demographics_set[, sex_br := (sex == "F")*1]
demographics_set[,demo_sex_v2 := NULL]

########### age
#interview age will be used instead of age
demographics_set[,demo_brthdat_v2:=NULL]
demographics_set[, age := interview_age]

########### gender
demographics_set[, gender:= demo_gender_id_v2-1]
demographics_set[, demo_gender_id_v2:= NULL]

########### ethnicity
demographics_set[demo_ethn_v2 == 1, ethnicity_hisp := 1]
demographics_set[demo_ethn_v2 == 2, ethnicity_hisp := 0]
demographics_set = demographics_set[, demo_ethn_v2 := NULL]


########### child race
#"refuse to answer" and "dont know" will be 0

# White
demographics_set[, race_white:= demo_race_a_p___10 ]

# Black
demographics_set[, race_black:= demo_race_a_p___11  ]

# Asian
demographics_set[, race_asian:= 0]
demographics_set[ (demo_race_a_p___18 == 1 | demo_race_a_p___19 == 1 | demo_race_a_p___20 == 1 |
                       demo_race_a_p___21 == 1 | demo_race_a_p___22 == 1 | demo_race_a_p___23 == 1 |
                       demo_race_a_p___24 ==1), race_asian:= 1 ]

# AIAN: American Indian and Alaska Native
demographics_set[, race_aian:= 0]
demographics_set[ (demo_race_a_p___12 == 1 | demo_race_a_p___13 == 1), race_aian:=1 ]


#NHPI: Native Hawaiian and Other Pacific
demographics_set[, race_nhpi:= 0]
demographics_set[ demo_race_a_p___14 == 1 | demo_race_a_p___15 == 1 | demo_race_a_p___16 == 1 |
                      demo_race_a_p___17 == 1, race_nhpi:= 1 ]

# Other
demographics_set[, race_other:= 0 ]
demographics_set[ demo_race_a_p___25 == 1, race_other:= 1 ]

# Mixed
demographics_set[, race_mixed:= (race_white + race_black + race_asian + race_aian + race_nhpi + race_other)]
demographics_set[, table(race_mixed, useNA = "ifany")]
demographics_set[ race_mixed <= 1, race_mixed:= 0]
demographics_set[ race_mixed > 1, race_mixed:= 1]
demographics_set[, table(race_mixed, useNA = "ifany")]

demographics_set[, grep("^demo_race_a_p___",colnames(demographics_set), value = T) := NULL]


demographics_set[!is.na(ethnicity_hisp), non_hispanic_black := 0]
demographics_set[ethnicity_hisp == 0 & race_black == 1, non_hispanic_black := 1]
demographics_set[!is.na(ethnicity_hisp) ,non_hispanic_white := 0]
demographics_set[ethnicity_hisp == 0 & race_white == 1, non_hispanic_white := 1]


########### child's country of birth
demographics_set[, born_in_usa := 0]
demographics_set[demo_origin_v2 == 189, born_in_usa := 1]

########### parents education
demographics_set[, parents_avg_edu:= rowMeans(.SD, na.rm = T), .SDcols = c("demo_prnt_ed_v2", "demo_prtnr_ed_v2")]

########### family income
demographics_set[,household_income:= demo_comb_income_v2]
demographics_set[,demo_comb_income_v2 := NULL]

########### parents married status
demographics_set[,separated_or_divorced := 0]
demographics_set[(demo_prnt_marital_v2 %in%  c(3,4)), separated_or_divorced := 1]
demographics_set[is.na(demo_prnt_marital_v2), separated_or_divorced := NA]

demographics_set[,parents_married := 0]
demographics_set[(demo_prnt_marital_v2 == 1), parents_married := 1]
demographics_set[is.na(demo_prnt_marital_v2), parents_married := NA]

demographics_set[,living_with_partenr_or_married := 0]
demographics_set[(demo_prnt_marital_v2 %in% c(1,6)), living_with_partenr_or_married := 1]
demographics_set[is.na(demo_prnt_marital_v2), living_with_partenr_or_married := NA]

######## both parents immgretation
# 1. if demo_prnt_16 == 0 ==> immgration = 0
# 2. go over prim feature and update immgration accordenglly
# not clear what to do with demo_prim == 3
# demographics_set[demo_prnt_16 == 0 ,parents_immigrants := 0]
# demographics_set[demo_prnt_16 == 1 & demo_prim == 1 & demo_prnt_origin_v2 != 189 & demo_biofather_v2 != 189 ,parents_immigrants := 1]
# demographics_set[demo_prnt_16 == 1 & demo_prim == 2 & demo_prnt_origin_v2 != 189 & demo_biomother_v2 != 189 ,parents_immigrants := 1]
# demographics_set[demo_prnt_16 == 1 & demo_prim == 3 & demo_prnt_origin_v2 != 189 & demo_biomother_v2 != 189 ,parents_immigrants := 1]


########### economic hardship
economic_hardship_names = grep("demo_fam_exp", colnames(demographics_set),value = T)


library("psych")
xcor <- polychoric(as.data.frame(demographics_set)[ ,economic_hardship_names ])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]


demographics_set[, demo_fam_poverty := {
    fcase(
        rowSums(is.na(.SD)) != 7,rowSums(.SD, na.rm = T) ,
        default = NA
    )
}, .SDcols = economic_hardship_names]


# demographics_set[ , View(.SD), .SDcols = c(economic_hardship_names, "demo_fam_poverty") ]


#remove irrelevant columns
demographics_set [, c("demo_adopt_agex_v2_bl_dk","demo_years_us_v2_dk" ) := NULL]

#remove outliers of "number of household members"
demographics_set[demo_roster_v2 %in% c(60,77), demo_roster_v2:= NA]



selected_features = c("src_subject_id", "sex", "sex_br", "age", "eventname", "interview_age", "interview_date",
                      "race_white", "race_black", "race_aian", "race_nhpi", "race_asian", "race_other","race_mixed" ,
                      "ethnicity_hisp", "non_hispanic_black", "non_hispanic_white",
                      "separated_or_divorced","parents_married", "living_with_partenr_or_married", 
                      "parents_avg_edu", "household_income", "demo_fam_poverty",
                      "born_in_usa")

write.csv(file = "data/demographics_baseline.csv", x = demographics_set[,..selected_features], row.names=F, na = "")


