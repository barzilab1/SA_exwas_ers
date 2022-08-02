library(psych)
library(plyr)

source("config.R")
source("utility_fun.R")


covidp_r1 = load_instrument("pabcdcovid19questionnaire01",abcd_covid_r1_files_path)
covidp_r2 = load_instrument("pabcdcovid19questionnaire01",abcd_covid_r2_files_path)
covidp_r3 = load_instrument("pabcdcovid19questionnaire01",abcd_covid_r3_files_path)

covidp = rbind.fill(covidp_r1, covidp_r2, covidp_r3)
covidp[covidp == 777 | covidp == 999] = NA

# new variable to use in reshape from long to wide format
covidp$timepoint = regmatches(covidp$eventname, regexpr("cv[1-7]", covidp$eventname))

covidp = covidp[,grep("src|timepoint|^fam_(a|w|dia|exp([1-7]|_rac))|increased_conflict|^child_(sep|tested|worried)|(to_|.?)school_(at|close_cv|cv)|work_ability|eventname|interview_age", colnames(covidp), value = T)]

#### family actions  
fam_actions_cols = grep("fam_a.*_([1-9]|1[0-3])$", colnames(covidp), value = T)
covidp$fam_isolate_tot_cv = rowSums(covidp[,fam_actions_cols])

#### financial strain
financial_strain = grep("fam_(w|exp[1-6])", colnames(covidp), value = T)
covidp[,financial_strain] = covidp[,financial_strain] - 1
covidp$fam_exp_tot_cv = rowSums(covidp[,grep("exp[1-6]", financial_strain, value = T)])

#### child_separate
covidp$child_separate_cv = covidp$child_separate_cv -1

#### child test
covidp$child_tested_pos_cv = ifelse(covidp$child_tested_cv == 4, 1, 0)
covidp$child_tested_neg_cv = ifelse(covidp$child_tested_cv == 3, 1, 0)

#### family diagnosed with coronavirus
covidp$fam_tested_pos_cv = ifelse( covidp$fam_diag_cv > 0, 1,
                                   ifelse(is.na(covidp$fam_diag_cv), NA, 0))

#### school
school = grep("school", colnames(covidp), value = T)
covidp[, school][covidp[, school] == 3] = NA
covidp$school_close_spring_2020_cv = covidp$school_close_cv -1
# covidp$school_close_fall_2020_cv = (school$school_at_home_cv_4 | school$school_at_home_cv_5)*1

covidp$went_to_school_cv = covidp$went_to_school_cv-1


write.csv(covidp, "outputs/covidp_long.csv", row.names=F, na = "")



