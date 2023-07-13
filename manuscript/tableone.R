library(tableone)
library(data.table)
library(plyr)

demo_race = read.csv("data/demo_race.csv")
train = read.csv(file = "data/DV_suicide_train.csv")
test = read.csv(file = "data/DV_suicide_test.csv")


create_wide_dataset = function(df){
  df$timepoint = sub("_year.*", "", df$eventname)
  
  df_wide = reshape(df[,c("src_subject_id", "sex", "SA_y","timepoint", "matched_group")], direction = "wide", idvar = c("src_subject_id", "sex", "matched_group"), timevar = "timepoint", sep = "__")
  setDT(df_wide)
  
  df_wide[, SA_y_ever:={
    fcase(
      SA_y__2 == 1 | SA_y__1 == 1 | SA_y__baseline == 1, 1,
      SA_y__2 ==0 | SA_y__1 == 0 | SA_y__baseline == 0, 0,
      default = NA
    )
  }]
  
  
  ### calculate the mean diff between 2 year and both 1 year and baseline
  # mean_baseline_2year = age_site_wide[, mean(interview_age__2 - interview_age__baseline, na.rm = T)]
  # mean_1year_2year = age_site_wide[, mean(interview_age__2 - interview_age__1, na.rm = T)]
  # 
  # age_site_wide[, age := {
  #   fcase(
  #     !is.na(interview_age__2), round(interview_age__2), 
  #     !is.na(interview_age__1), round(interview_age__1 + mean_1year_2year),
  #     !is.na(interview_age__baseline), round(interview_age__baseline + mean_baseline_2year) ,
  #     default = NA
  #   )
  # }]
  
  return(df_wide)
}


train_wide = create_wide_dataset(train)
test_wide = create_wide_dataset(test)

train_wide = merge(train_wide, demo_race)
test_wide = merge(test_wide, demo_race)


df = rbind.fill(train_wide,test_wide)


vars = colnames(df)[-c(1,3)]

## Create TableOne
tab <- CreateTableOne(vars = vars, data = df,  factorVars = vars , strata = "matched_group", addOverall = T)
table1 <- print(tab, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)
write.csv(table1, file = "outputs/Table1.csv")



