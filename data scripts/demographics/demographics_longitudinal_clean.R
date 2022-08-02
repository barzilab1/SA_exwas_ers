##############################################
#' most of the code below is adjusted from
#' the abcd official github:
#' https://github.com/ABCD-STUDY/analysis-nda
##############################################

library(data.table)

source("config.R")
source("utility_fun.R")

demographics_set = load_instrument("abcd_lpds01",abcd_files_path)
demographics_set[demographics_set == 777 | demographics_set == 999] = NA


########### rearrange data ###########
### convert variables names to be more readable
demographics_set = data.table(demographics_set)

########### sex
#convert the NIH sex at birth (equal to demo_sex_v2)
demographics_set[, sex_br := (sex == "F")*1]

########### age
#interview age will be used instead of age
demographics_set[, age := interview_age]

########### gender
demographics_set[, gender:= demo_gender_id_v2_l-1]
demographics_set[, demo_gender_id_v2_l:= NULL]

########### parents education
demographics_set[, parents_avg_edu:= rowMeans(.SD, na.rm = T), .SDcols = c("demo_prnt_ed_v2_l", "demo_prtnr_ed_v2_l")]

########### family income
demographics_set[,household_income:= demo_comb_income_v2_l]
demographics_set[,demo_comb_income_v2_l := NULL]

########### parents married status
demographics_set[,separated_or_divorced := 0]
demographics_set[(demo_prnt_marital_v2_l %in%  c(3,4)), separated_or_divorced := 1]
demographics_set[is.na(demo_prnt_marital_v2_l), separated_or_divorced := NA]

demographics_set[,parents_married := 0]
demographics_set[(demo_prnt_marital_v2_l == 1), parents_married := 1]
demographics_set[is.na(demo_prnt_marital_v2_l), parents_married := NA]

demographics_set[,living_with_partenr_or_married := 0]
demographics_set[(demo_prnt_marital_v2_l %in% c(1,6)), living_with_partenr := 1]
demographics_set[is.na(demo_prnt_marital_v2_l), living_with_partenr := NA]


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



demographics_set = droplevels(demographics_set)


selected_features = c("src_subject_id", "interview_date", "interview_age", "demo_prim_l" , "eventname", "sex",
                      "household_income","demo_prnt_ed_v2_l", "demo_prtnr_ed_v2_l", "parents_avg_edu",
                      "demo_prnt_marital_v2_l", "separated_or_divorced", "parents_married", "living_with_partenr_or_married",
                      "age", "sex_br", "gender", "demo_ed_v2_l",
                      economic_hardship_names, "demo_fam_poverty")

write.csv(file = "outputs/demographics_long.csv", x = demographics_set[,..selected_features], row.names=F, na = "")




