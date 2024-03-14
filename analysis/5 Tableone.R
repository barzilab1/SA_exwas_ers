library(tableone)
library(data.table)
library(plyr)


create_wide_dataset = function(df){
  
  df$timepoint = sub("_year.*", "", df$eventname)
  
  df_wide = reshape(df[,c("src_subject_id", "sex", "SA_y","timepoint", "matched_group","age", "LGBT", "LGBT_inclusive" )], 
                    direction = "wide", 
                    idvar = c("src_subject_id", "sex", "matched_group"), timevar = "timepoint", sep = "__")
  setDT(df_wide)
  
  df_wide[, SA_y_ever:={
    fcase(
      SA_y__2 == 1 | SA_y__1 == 1 | SA_y__baseline == 1, 1,
      SA_y__2 ==0 | SA_y__1 == 0 | SA_y__baseline == 0, 0,
      default = NA
    )
  }]
  
  df_wide[, LGBT_inclusive__ever:={
    fcase(
      LGBT_inclusive__2 == 1 | LGBT_inclusive__1 == 1 | LGBT_inclusive__baseline == 1, 1,
      LGBT_inclusive__2 == 0 | LGBT_inclusive__1 == 0 | LGBT_inclusive__baseline == 0, 0,
      default = NA
    )
  }]
  
  return(df_wide)
}


demo_race = read.csv("data/demo_race.csv")
train = read.csv(file = "data/DV_suicide_train.csv")
test = read.csv(file = "data/DV_suicide_test.csv")
lgbt <- read.csv("data/lgbtqia.csv")


train$age = train$interview_age / 12
test$age = test$interview_age / 12

train = merge(train, lgbt)
test = merge(test, lgbt)

train_wide = create_wide_dataset(train)
test_wide = create_wide_dataset(test)

train_wide = merge(train_wide, demo_race)
test_wide = merge(test_wide, demo_race)


df = rbind.fill(train_wide,test_wide)


vars = colnames(df)[-c(1,3)]

## Create TableOne
tab <- CreateTableOne(vars = vars, data = df,  factorVars = vars[grep("age", vars, invert = T)] , strata = "SA_y_ever", addOverall = T)
table1 <- print(tab, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T, pDigits = 5)
write.csv(table1, file = "outputs/Table1.csv")

# only in testing
tab <- CreateTableOne(vars = vars, data = train_wide,  factorVars = vars[grep("age", vars, invert = T)] , strata = "SA_y_ever", addOverall = T)
table2 <- print(tab, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)
table2
