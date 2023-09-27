library(readr)
library(data.table)
library(psych)
library(janitor)

setwd("~/sa_exwas")

bhs_main = read_csv("data/bhs_main.csv")
patient = read_csv("data/patient.csv")
coverage = read_csv("data/coverage.csv")

dataset = merge(patient, bhs_main, by = "pat_id")
dataset = merge(dataset, coverage, all.x = T)

setDT(dataset)


dataset[payor_category == "SELF-PAY", payor_category := NA]
dataset[, MEDICAID := {
  fcase(
    payor_category == "MEDICAID",1,
    payor_category == "PRIVATE",0,
    default = NA
  )
}]
dataset[,table(payor_category, MEDICAID, useNA = "if")]

# keep only kids age 12 - 18
### TODO - we lose kids that their second visit was after age 18 ---> 
### remove kids with first visit > 18 in later stage ---- age_at_screen <= 18
dataset = dataset[age_at_screen >= 12 & age_at_screen <= 18,]

##########################################
#### 1. clean race, ethnicity and sex ####
##########################################

# patient-table race and ethnicity are preceding to bhs 
dataset[, race := fifelse(!is.na(race.x), tolower(race.x), race.y )]

dataset[race == "black/african american", race := "black or african american" ]
dataset[race == "more than one race", race := "multiple races"]
dataset[race == "native hawaiian/other pacific islander", race := "native hawaiian or other pacific islander" ]
dataset[race == "not sure", race := NA]

dataset[, table(race, useNA = "if")]
dataset[, same_race := length(unique(race)) == 1, by = pat_id]
# View(dataset[same_race == F,c("pat_id", "encounter_id", "race.x", "race.y", "race")])
dataset[,c("race.x", "race.y", "same_race") := NULL]


dataset[, ethnicity_new := {
  fcase(
    !is.na(ethnicity) & ethnicity == "Hispanic or Latino", 1,
    !is.na(ethnicity) & ethnicity == "Non-Hispanic or Non-Latino", 0,
    !is.na(hispan) & hispan == "yes", 1,
    !is.na(hispan) & hispan == "no", 0,
    default = NA
  )
}]

dataset[,table(ethnicity_new, ethnicity, useNA = "ifany")]
dataset[,table(ethnicity_new, hispan, useNA = "ifany")]

dataset[, same_eth := length(unique(ethnicity_new)) == 1, by = pat_id]
# View(dataset[same_eth == F,c("pat_id", "encounter_id", "ethnicity", "hispan", "ethnicity_new")])
dataset[, ethnicity_new := ifelse(is.na(ethnicity_new), ethnicity_new[!is.na(ethnicity_new)], ethnicity_new), by = pat_id]
dataset[,c("ethnicity", "hispan", "same_eth") := NULL]

# create non hispanic black and white
dataset[, race_white_non_hisp := {
  fcase(
    ethnicity_new == 0 & race == "white", 1,
    !is.na(race) & !is.na(ethnicity_new) , 0,
    default = NA
  )
}] 

dataset[, race_black_non_hisp := {
  fcase(
    ethnicity_new == 0 & race == "black or african american", 1,
    !is.na(race) & !is.na(ethnicity_new) , 0,
    default = NA
  )
}] 

dataset[,table(race, ethnicity_new, useNA = "if")]
dataset[,table(race, race_black_non_hisp, useNA = "if")]
dataset[,table(race, race_white_non_hisp, useNA = "if")]

dataset[, race_eth := {
  fcase(
    race_white_non_hisp == 1, "NH-White",
    race_black_non_hisp == 1, "NH-Black",
    race_white_non_hisp == 0, "Hispanic"
  )
}]

dataset[,table(race_eth, ethnicity_new, useNA = "if")]
dataset[,table(race_eth, race_black_non_hisp, useNA = "if")]
dataset[,table(race_eth, race_white_non_hisp, useNA = "if")]

dataset[, race_black := {
  fcase(
    race == "black or african american", 1,
    !is.na(race) , 0,
    default = NA
  )
}]
dataset[,table(race, race_black, useNA = "if")]

dataset[, race_white := {
  fcase(
    race == "white", 1,
    !is.na(race) , 0,
    default = NA
  )
}]
dataset[,table(race, race_white, useNA = "if")]


dataset[, sex := {
  fcase(
    sex_abbr == "F", 1,
    sex_abbr == "M", 0,
    default = NA
  )
}]
dataset[,table(sex, sex_abbr, useNA = "if")]


dataset[, gender_new := fifelse(!is.na(gender), gender, tolower(gender_identity_name ))]
dataset[, table(gender_new, gender, useNA = "ifany")]
dataset[, table(gender_new, sex_abbr, useNA = "ifany")]
dataset[, TRANS := {
  fcase(
    gender_new %in% c("female","male"), 0,
    !is.na(gender_new), 1,
    default = NA
  )
}]
dataset[, table(gender_new, TRANS, useNA = "ifany")]

#################################
#### 2. change IVs to binary ####
#################################

# During free time at school, how often do you spend time with friends or are you mostly alone?
dataset[,bhsb01_br := {
  fcase(
    bhsb01 == "never with friends", 1,
    bhsb01 == "often with friends" | bhsb01 == "sometimes with friends" , 0,
    default = NA
  )
}]

# How often do you feel kids tease you, make fun of you, or ignore you?
dataset[,bhsb02_br := {
  fcase(
    bhsb02 == "never", 0,
    bhsb02 == "often" | bhsb02 == "sometimes" , 1,
    default = NA
  )
}]

# How often do kids physically hurt you or threaten to hurt you?
dataset[,bhsb03_br := {
  fcase(
    bhsb03 == "never", 0,
    bhsb03 == "often" | bhsb03 == "sometimes" , 1,
    default = NA
  )
}]

# How often are you cyber bullied (e.g., chat rooms, Facebook, instant messaging, 
# text messages on your cell phone)?
dataset[,bhsb04_br := {
  fcase(
    bhsb04 == "never", 0,
    bhsb04 == "often" | bhsb04 == "sometimes" , 1,
    default = NA
  )
}]

# You said that you were at least sometimes (alone, teased, physically threatened, 
# or cyber bullied). How upsetting are these kinds of experiences for you?
dataset[,bhsb04a_br := {
  fcase(
    bhsb04a == "not at all", 0,
    bhsb04a == "somewhat" | bhsb04a == "a lot" , 1,
    default = NA
  )
}]

dataset[,c("bhsb01", "bhsb02", "bhsb03", "bhsb04", "bhsb04a") := NULL ]

# convert Yes/No to 1/0
#names(dataset)[sapply(dataset, is.character)]
bhs_cols = dataset[,.SD, .SDcols = grep("bhs[^c]", colnames(dataset))][,colnames(.SD),.SDcols = is.character]
dataset[, (bhs_cols) := lapply(.SD, \(x) {x[x == "yes"] = 1; x[x == "no"] = 0;return(x)}), .SDcols = bhs_cols]


dataset[, table(military, useNA = "ifany")]
dataset[, military_child := {
  fcase(
    military == "i'm a child of a veteran parent" | military == "i'm a child of an active duty parent", 1,
    military == "non-military", 0,
    default = NA
  )
}]
dataset[ ,military := NULL]

# fill BHSSA01a and BHSSA01b with 0 from BHSSA01 - tobacco
dataset[,table(bhssa01a, bhssa01, useNA = "if")]
dataset[,table(bhssa01b, bhssa01, useNA = "if")]
dataset[bhssa01 == 0, c("bhssa01a", "bhssa01b") := 0]

# fill also "bhssa02" with 0 from "bhssa02a" - alcohol
dataset[,table(bhssa02a, bhssa02, useNA = "if")]
dataset[bhssa02 == 0, bhssa02a := 0]

# bhssa03 bhssa03a - mj
dataset[,table(bhssa03a, bhssa03, useNA = "if")]
dataset[bhssa03 == 0, bhssa03a := 0]

# clean outlier: questions are about days used in the past 30 days
dataset[bhssa01a > 30, bhssa01a := NA]
dataset[bhssa01b > 30, bhssa01b := NA]
dataset[bhssa02a > 30, bhssa02a := NA]
dataset[bhssa03a > 30, bhssa03a := NA]

# bhst01a - how recent was this (physical fight)?
dataset[,table(bhst01a, bhst01, useNA = "if")]
dataset[, bhst01a_br := {
  fcase(
    bhst01 == 0, 0, 
    bhst01a == "more than 4 weeks ago", 1,
    bhst01a == "2 to 4 weeks ago", 2,
    bhst01a == "less than 2 weeks ago", 3,
    default = NA
  )
}]

dataset[,table(bhst01a_br, bhst01a, bhst01, useNA = "if")]


# bhst01b - Is this [the fight] why you are here today?
dataset[,table( bhst01b, bhst01, useNA = "if")]
dataset[,table( bhst01b, bhst01a, useNA = "if")]
dataset[bhst01 == 0, bhst01b := 0]

# bhst01d Do you think that any of your friends or family members will hurt anyone because of what happened?
dataset[,table( bhst01d, bhst01, useNA = "if")]
dataset[bhst01 == 0, bhst01d := 0]

dataset[, bhst01a := NULL ]

# Has this [something sexual] happened in the past year?
dataset[,table( bhst03, bhst03a, useNA = "if")]
dataset[bhst03 == 0, bhst03a := 0]

# Has this [ physically or sexually hurt by an adult who lives in or frequently stays in your home ] happened in the past year?
dataset[,table( bhst04, bhst04a, useNA = "if")]
dataset[bhst04 == 0, bhst04a := 0]


# bhssf01ao - Is there a gun in your home?
# bhssf01ai	Select any/all of the following: Rifle
# bhssf01aii	Handgun
# bhssf01aiii	Shotgun
# bhssf01aiv	BB gun
# bhssf01av	Other
dataset[,table( bhssf01ai, bhssf01ao, useNA = "if")]
dataset[bhssf01ao == 0, bhssf01ai := 0]
dataset[bhssf01ao == 0, bhssf01aii := 0]
dataset[bhssf01ao == 0, bhssf01aiii := 0]
dataset[bhssf01ao == 0, bhssf01aiv := 0]
dataset[bhssf01ao == 0, bhssf01av := 0]

###############################################
#### 3. select exposome features & dataset #### 
###############################################

demogrphics_f = c("age_at_screen" ,"sex_abbr", "sex","race_black_non_hisp","race_white_non_hisp",
                  "race","race_black", "race_white" ,"ethnicity_new", "gender", "TRANS", "race_eth")
exposome_f = grep("bhssc|bhsf|bhssf0|bhssa0|bhssx|sx02|bhsed02|bhst01(a|b|d)|bhst0[2-4]|bhsb|military_child|MEDICAID", 
                  colnames(dataset), value = T) #35
outcome = "bhssu04"
dataset_exposome = dataset[,.SD, .SDcols = c("pat_id", "encounter_id", "contact_date",
                                               demogrphics_f, exposome_f, outcome )]
dataset_exposome = dataset_exposome[!is.na(bhssu04),]
dataset_exposome = remove_empty(dataset_exposome)


#### add visit number (time point) ####
dataset_exposome[, contact_date := as.Date(contact_date)]
dataset_exposome = dataset_exposome[order(contact_date)]
dataset_exposome[, visit_number := 1:.N, by = pat_id ]


# in this project, we use only first visit 
dataset_exposome = dataset_exposome[visit_number == 1,]


###########################################################
#### 4. remove columns with more then 10% missing data ####
###########################################################
# dataset_exposome[,colSums(is.na(.SD))]

missing_data_columns = dataset_exposome[, names(which(colSums(is.na(.SD)) > 0.1*.N))]
dataset_exposome[, (missing_data_columns) := NULL]

########################################################
### 5. remove columns with less then 0.1 information ###
########################################################
# dataset_exposome = type.convert(dataset_exposome) # is.numeric(x) &&
vari_delete = dataset_exposome[, names(which(sapply(.SD,\(x)  (sum(x != 0, na.rm = T) / sum(!is.na(x))  < 0.01))) )
                               , .SDcols = setdiff(exposome_f, missing_data_columns)]
dataset_exposome[, (vari_delete) := NULL]


write.csv(dataset_exposome, "data/exposome_full_clean.csv", na = "", row.names = F)












##########################################################################################

######## check SQL on how to merge
# blood_pressure_data = setDT(read_csv("data/blood_pressure.csv"))
# bmi_data = setDT(read_csv("data/bmi.csv"))
# 
# blood_pressure_data = blood_pressure_data[, started_at_datetime := as.Date(started_at_datetime)]
# bmi_data = bmi_data[, started_at_datetime := as.Date(started_at_datetime)]
# dataset_individual = dataset_individual[, started_at_datetime := as.Date(started_at_datetime)]
# 
# dataset1 = merge(dataset_individual, blood_pressure_data ,all.x = T )
# dataset1 = merge(dataset_individual, bmi_data, all.x = T)
# 
# dataset1 = dataset1[!is.na(flowsheet_value),]
# dataset1 = dataset1[!is.na(bmi),]
# 
# 
# dataset1[,length(unique(pat_id))]

# YE9105ZSAXUDC --> same started_at_datetime but very different age [16 vs 17]



