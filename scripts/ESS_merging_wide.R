library(data.table)

source("config.R")
source("utility_fun.R")

#### create wide dataset for exposome sum score ####
site <- read.csv("data/site.csv")
race <- read.csv("data/demo_race.csv")
FH_suicide <- read.csv("data/family_history.csv")
genetics <- read.csv(file.path(abcd_genetics_path, "genetic.csv"))

suicide_test <- read.csv("data/DV_suicide_test.csv")
individual_level <- read.csv("data/individual_level_test.csv")
structural_level <- read.csv("data/structural_level_test.csv")


### suicide wide + ever
suicide_test$timepoint = sub("_year.*", "", suicide_test$eventname)
suicide_test[, c("interview_age", "interview_date", "eventname")] = NULL

suicide_wide = reshape(suicide_test[,c("src_subject_id", "sex", "SA_y","timepoint")], direction = "wide", idvar = c("src_subject_id", "sex"), timevar = "timepoint", sep = "__")
suicide_wide$SA_y_ever = apply(suicide_wide[,grep("SA", colnames(suicide_wide))], 1,
                                       function(r){  any( r == 1)*1 })
### select only kids that have value in SA ever
suicide_wide = suicide_wide[!is.na(suicide_wide$SA_y_ever), c("src_subject_id", "sex", "SA_y_ever")]

### exposome wide
individual_level_wide = get_wide_data(individual_level, "tbi_ss_worst_overall")
structural_level_wide = get_wide_data(structural_level) 


dataset = merge(suicide_wide, race, all.x = T)
dataset = merge(dataset, unique(suicide_test[,c("src_subject_id", "rel_family_id")]) , all.x = T)
dataset = merge(dataset, FH_suicide[,c("src_subject_id", "famhx_ss_momdad_scd_p")], all.x = T)
dataset = merge(dataset, genetics[,c("src_subject_id", "suicide_PRSice_Pt0_05", "genetic_afr")], all.x = T)


#### add age and site according to 2 year follow up####
site$timepoint = sub("_year.*", "", site$eventname)
age_site_wide = reshape(site, direction = "wide", idvar = c("src_subject_id", "sex"), timevar = "timepoint", sep = "__")
age_site_wide = age_site_wide[,grep("src|age|site|sex", colnames(age_site_wide))]
setDT(age_site_wide)

age_site_wide[, site_id_l_br:={
  fcase(
    !is.na(site_id_l_br__2), site_id_l_br__2,
    !is.na(site_id_l_br__1), site_id_l_br__1,
    !is.na(site_id_l_br__baseline), site_id_l_br__baseline,
    default = NA
  )
}]

# age_site_wide[,View(.SD), .SDcols = grep("site", colnames(age_site_wide))]

#### imputation of age at 2 year ####
age_site_wide[,describe(.SD), .SDcols = grep("age", colnames(age_site_wide))]

### calculate the mean diff between 2 year and both 1 year and baseline
mean_baseline_2year = age_site_wide[, mean(interview_age__2 - interview_age__baseline, na.rm = T)]
mean_1year_2year = age_site_wide[, mean(interview_age__2 - interview_age__1, na.rm = T)]

age_site_wide[, age := {
  fcase(
    !is.na(interview_age__2), round(interview_age__2), 
    !is.na(interview_age__1), round(interview_age__1 + mean_1year_2year),
    !is.na(interview_age__baseline), round(interview_age__baseline + mean_baseline_2year) ,
    default = NA
  )
}]
# age_site_wide[,View(.SD), .SDcols = grep("age", colnames(age_site_wide))]

dataset = merge(dataset, age_site_wide[,.(src_subject_id, sex, age, site_id_l_br )])


#### calculate sum scores ####
individual_level_results <- read_csv("outputs/individual_level_results.csv")
structural_level_results <- read_csv("outputs/structural_level_results.csv")


#### p value correction 
individual_level_results$fdr = p.adjust(individual_level_results$P_value, "fdr")
structural_level_results$fdr = p.adjust(structural_level_results$P_value, "fdr")

individual_cut_off = individual_level_results[individual_level_results$fdr <= 0.05, c("Feature", "Coefficients")]
structural_cut_off = structural_level_results[structural_level_results$fdr <= 0.05, c("Feature", "Coefficients")]


write.csv(file = paste0("outputs/individual_level_results_fdr_wide.csv"), individual_level_results, row.names = F)
write.csv(file = paste0("outputs/structural_level_results_fdr_wide.csv"), structural_level_results, row.names = F)


colnames(individual_level_wide) = sub("_(mean|ever|max|_(1|2|baseline))","",colnames(individual_level_wide))
colnames(structural_level_wide) = sub("_(mean|ever|max|_(1|2|baseline))","",colnames(structural_level_wide))

setdiff(individual_cut_off$Feature, colnames(individual_level_wide) )
setdiff(structural_cut_off$Feature, colnames(structural_level_wide) )


individual_level_wide$exwas_individual_sum = apply(individual_level_wide[,individual_cut_off$Feature], 1 , function(r){
  return(sum(r*individual_cut_off$Coefficients, na.rm = T))
})

structural_level_wide$exwas_structural_sum = apply(structural_level_wide[,structural_cut_off$Feature], 1 , function(r){
  return(sum(r*structural_cut_off$Coefficients, na.rm = T))
})


individual_level_wide$exwas_individual_sum_z = scale(individual_level_wide$exwas_individual_sum)
structural_level_wide$exwas_structural_sum_z = scale(structural_level_wide$exwas_structural_sum)

dataset = merge(dataset, individual_level_wide[,c("src_subject_id", "sex", "exwas_individual_sum", "exwas_individual_sum_z")] )
dataset = merge(dataset, structural_level_wide[,c("src_subject_id", "sex", "exwas_structural_sum", "exwas_structural_sum_z")] )

write.csv(file = paste0("data/dataset_ESS_wide.csv"), dataset, row.names = F)



# View(describe(individual_level_wide[,c(individual_cut_off_risk$Feature, individual_cut_off_prote$Feature)]))


# individual_level_results$bonferroni = p.adjust(individual_level_results$P_value, "bonferroni")
# structural_level_results$bonferroni = p.adjust(structural_level_results$P_value, "bonferroni")


#### create exposome weights ####
#' cut_off_levels = c(.1,.05,.01)
#' 
#' #' 1. check for each variable if it below the cut off
#' individual_cut_off_variables = list(fdr = list(), bonferroni = list()) 
#' structural_cut_off_variables = list(fdr = list(), bonferroni = list()) 
#' 
#' for(p in cut_off_levels){
#'   individual_cut_off_variables$fdr[[as.character(p) ]] = individual_level_results$feature[individual_level_results$fdr <= p]
#'   individual_cut_off_variables$bonferroni[[as.character(p) ]] = individual_level_results$feature[individual_level_results$bonferroni <= p]
#'   
#'   structural_cut_off_variables$fdr[[as.character(p) ]] = structural_level_results$feature[structural_level_results$fdr <= p]
#'   structural_cut_off_variables$bonferroni[[as.character(p) ]] = structural_level_results$feature[structural_level_results$bonferroni <= p]
#' }









