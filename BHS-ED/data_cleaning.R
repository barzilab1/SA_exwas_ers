library(readr)
library(data.table)
library(psych)


bhs_main = read_csv("exwas/data/bhs_main.csv")
patient = read_csv("exwas/data/patient.csv")
coverage = read_csv("exwas/data/coverage.csv")

#### clean demographics #### 
dataset = merge(patient, bhs_main, by = "pat_id")
setDT(dataset)

# add coverage
setDT(coverage)
dataset = dataset[, started_at_datetime := as.Date(started_at_datetime)]
coverage = coverage[, started_at_datetime := as.Date(survey_start_datetime)]
# dataset1 = merge(dataset, coverage[,c("pat_id", "payor_category", "started_at_datetime")], all.x = T)
#### TODO same pat_id & date but different coverage [YE914D9OZZKEF]???

# keep only kids age 12 - 18
dataset = dataset[age_at_screen >= 12 & age_at_screen <= 18,]


# patient table's race and ethnicity are preceding to bhs 
dataset[, race := {
  fcase(
    !is.na(race.x), race.x,
    !is.na(race.y), race.y,
    default = NA
  )
}]

dataset[race == "Black or African American", race := "Black/African American"]
dataset[race == "More Than One Race", race := "Multiple Races"]
dataset[race == "Native Hawaiian or Other Pacific Islander", race := "Native Hawaiian/Other Pacific Islander"]
dataset[race == "Not Sure", race := NA]
dataset[,c("race.x", "race.y") := NULL]


dataset[, ethnicity_new := {
  fcase(
    !is.na(ethnicity) & ethnicity == "Hispanic or Latino", 1,
    !is.na(ethnicity) & ethnicity == "Not Hispanic or Latino", 0,
    !is.na(hispan) & hispan == "Yes", 1,
    !is.na(hispan) & hispan == "No", 0,
    default = NA
  )
}]
dataset[,table(ethnicity_new,ethnicity)]
dataset[,table(ethnicity_new,hispan)]
dataset[,c("ethnicity", "hispan") := NULL]

# create non hispanic black and white
dataset[, race_white_non_hisp := {
  fcase(
    ethnicity_new == 0 & race == "White", 1,
    !is.na(race) & !is.na(ethnicity_new) , 0,
    default = NA
  )
}] 

dataset[, race_black_non_hisp := {
  fcase(
    ethnicity_new == 0 & race == "Black/African American", 1,
    !is.na(race) & !is.na(ethnicity_new) , 0,
    default = NA
  )
}] 

dataset[,table(race, ethnicity_new)]
dataset[,table(race, race_black_non_hisp)]
dataset[,table(race, race_white_non_hisp)]


dataset[, race_black := {
  fcase(
    race == "Black/African American", 1,
    !is.na(race) , 0,
    default = NA
  )
}]
dataset[,table(race, race_black, useNA = "ifany")]

dataset[, race_white := {
  fcase(
    race == "White", 1,
    !is.na(race) , 0,
    default = NA
  )
}]
dataset[,table(race, race_white, useNA = "ifany")]


dataset[, sex := {
  fcase(
    sex_abbr == "F", 1,
    sex_abbr == "M", 0,
    default = NA
  )
}]
dataset[,table(sex, sex_abbr, useNA = "ifany")]


#### change IV to binary ####

dataset[,bhsb01_br := {
  fcase(
    bhsb01 == "Never with friends", 1,
    bhsb01 == "Often with friends" | bhsb01 == "Sometimes with friends" , 0,
    default = NA
  )
}]

dataset[,bhsb02_br := {
  fcase(
    bhsb02 == "Never", 0,
    bhsb02 == "Often" | bhsb02 == "Sometimes" , 1,
    default = NA
  )
}]

dataset[,bhsb03_br := {
  fcase(
    bhsb03 == "Never", 0,
    bhsb03 == "Often" | bhsb03 == "Sometimes" , 1,
    default = NA
  )
}]

dataset[,bhsb04_br := {
  fcase(
    bhsb04 == "Never", 0,
    bhsb04 == "Often" | bhsb04 == "Sometimes" , 1,
    default = NA
  )
}]

dataset[,bhsb04a_br := {
  fcase(
    bhsb04a == "Not at all", 0,
    bhsb04a == "Somewhat" | bhsb04a == "A lot" , 1,
    default = NA
  )
}]

dataset[,c("bhsb01", "bhsb02", "bhsb03", "bhsb04", "bhsb04a") := NULL ]

# convert Yes/No to 1/0
bhs_cols = dataset[,.SD, .SDcols =grep("bhs[^c]", colnames(dataset))][,colnames(.SD),.SDcols = is.character]
dataset[, (bhs_cols) := lapply(.SD, function(x) replace(x, x == "Yes", 1)), .SDcols = bhs_cols]
dataset[, (bhs_cols) := lapply(.SD, function(x) replace(x, x == "No", 0)), .SDcols = bhs_cols]
dataset = type.convert(dataset)


#### select features #### 
demogrphics_f = c("age_at_screen" ,"sex_abbr", "sex","race_black_non_hisp","race_white_non_hisp","race","race_black", "race_white" ,"ethnicity_new")
exposome_f = grep("bhsf|bhssf0[1-4]|bhst0[2-4]|bhst01d?$|bhsb", colnames(dataset), value = T) 
outcome = "bhssu04"
dataset_individual = dataset[,.SD, .SDcols = c("pat_id", "started_at_datetime", demogrphics_f, exposome_f,outcome )]
dataset_individual = dataset_individual[, bhssf01avo := NULL]
dataset_individual = dataset_individual[!is.na(bhssu04),]
# dataset1 = dataset_individual[, colSums(is.na(.SD)) != .N]
# empty_cols = dataset_individual[, names(which(sapply(.SD, function(x) all(is.na(x)))))] 
# dataset_individual[, (empty_cols) := NULL]

# number of participants  --> 19803
dataset_individual[, length(unique(pat_id))]


#### add visit number (time point) ####
# there are missing values in started_at_datetime, so first order by age
dataset_individual = dataset_individual[order(age_at_screen)]
dataset_individual = dataset_individual[order(started_at_datetime)]
dataset_individual[, visit_number := 1:.N, by = pat_id ]


#### remove columns with more then 10% missing data ####
dataset_individual[,colSums(is.na(.SD))]

missing_data_columns = dataset_individual[, names(which(colSums(is.na(.SD)) > 0.1*.N))]
dataset_individual[, (missing_data_columns) := NULL]

saveRDS(dataset_individual, "exwas/data/dataset_individual_full.rds")

