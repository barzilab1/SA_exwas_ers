#' ############################################################
#' Here should be all the merging of the different datasets.
#' as well as creation of additional variables that are specific to this project
#' #############################################################

library(readr)




#' example of merging:
#' 1. first read the datasets from the outputs folder
demographics_baseline <- read_csv("outputs/demographics_baseline.csv")
family_set_baseline <- read_csv("outputs/family_set_baseline.csv")

#' 2. merge the datasets (make sure you merge datasets from the same time points)
baseline_set = merge(demographics_baseline,family_set_baseline)

#' optional:
#' 3. create any new required variables
#' 4. get only the relevant time point
baseline_set = baseline_set[baseline_set$eventname == "baseline_year_1_arm_1",]

#' 5. export the dataset to csv
write.csv(file = "outputs/baseline_set.csv",x = baseline_set, row.names = F, na = "")

###################################################################
library(readr)
library(plyr)


demographics_baseline <- read_csv("outputs/demographics_baseline.csv")
demographics_long <- read_csv("outputs/demographics_long.csv")
medications <- read_csv("outputs/medications.csv",col_types = cols(.default = "d", src_subject_id = "c", eventname = "c",))
physicalhealth_sum <- read_csv("outputs/physicalhealth_sum.csv")
physicalhealth <- read_csv("outputs/physicalhealth.csv")


# combine demographics of all time points
demo_race = demographics_baseline[,grep("src|race|hisp", colnames(demographics_baseline))]

demographics_long = merge(demographics_long, demo_race)
demographics_long = demographics_long[demographics_long$eventname != "baseline_year_1_arm_1",]

demographics = rbind.fill(demographics_baseline, demographics_long)


# define headaches medications
medications = medications[,grep("src|inter|event|Migraine|Daily.Preventive|Rescue.Medications", colnames(medications))]
medications$any_migraine_med_2w = Reduce("|",medications[,c("Migraine.Medications_2w", "Daily.Preventive.medications_2w", "Rescue.Medications_2w")])*1
medications$any_migraine_med_1yr = Reduce("|",medications[,c("Migraine.Medications_1yr", "Daily.Preventive.medications_1yr", "Rescue.Medications_1yr")])*1

describe(medications)


dataset = merge(demographics, medications)
dataset = merge(dataset, physicalhealth_sum)
dataset = merge(dataset, physicalhealth)








