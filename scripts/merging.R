library(readr)
library(plyr)

source("utility_fun.R")


## create suicide wide ####
family_relationship <- read_csv("data/family_relationship.csv")
site <- read_csv("data/site.csv")
suicide <- read_csv("data/suicide_long.csv")


suicide_site = merge(suicide, site)

suicide_site$timepoint = sub("_year.*", "", suicide_site$eventname)
suicide_site[,c("interview_date","interview_age","eventname")] = NULL
suicide_site_wide = reshape(suicide_site, direction = "wide", idvar = c("src_subject_id", "sex"), timevar = "timepoint", sep = "__")

## define SA_ever ###
#' 1 = if the kid has ever said “yes” to any of the questions of SA (no matter if he answered all questions or no),
#' 0 = if the kid said no to all questions. 
#' NA = In case of missing data and the kid didn’t answer “Yes” to any question related to SA
suicide_site_wide$SA_y_ever = apply(suicide_site_wide[,grep("SA_y", colnames(suicide_site_wide))], 1, function(r){ any(r == 1)*1 })

# select only kids that have value in SA ever
suicide_site_wide = suicide_site_wide[!is.na(suicide_site_wide$SA_y_ever), grep("src|sex|SA_y_ever|site", colnames(suicide_site_wide))]

# select the latest site
suicide_site_wide$site_id_l_br = ifelse(!is.na(suicide_site_wide$site_id_l_br__2), suicide_site_wide$site_id_l_br__2,
                                   ifelse(!is.na(suicide_site_wide$site_id_l_br__1), suicide_site_wide$site_id_l_br__1, 
                                          suicide_site_wide$site_id_l_br__baseline))
suicide_site_wide[, grep("site_id_l_br_", colnames(suicide_site_wide))] = NULL
suicide_site_wide$site_id_l_br[suicide_site_wide$site_id_l_br == 22] = 21

## merge wide and family relationship data 
dataset_wide = merge(suicide_site_wide, family_relationship[,c("src_subject_id","sex","rel_family_id")])
write.csv(file = "data/suicide_wide.csv", x = dataset_wide, row.names=F, na = "")



## organize demographics #### 
demographics_baseline <- read_csv("data/demographics_baseline.csv")
demographics_long <- read_csv("data/demographics_long.csv")

demo_race = demographics_baseline[,grep("src|race|hisp|born_in_usa", colnames(demographics_baseline))]
demographics_baseline = demographics_baseline[, grep("race|hisp|born_in_usa", colnames(demographics_baseline), invert= T)]

demographics_long = demographics_long[demographics_long$eventname != "baseline_year_1_arm_1",]

demographics_exposome = rbind.fill(demographics_baseline, demographics_long)

# not relevant for now 
demographics_exposome[,c("age")] = NULL
demographics_exposome_wide = get_wide_data(demographics_exposome)

write.csv(file = "data/demographics_exposome_wide.csv", x = demographics_exposome_wide, row.names=F, na = "")
write.csv(file = "data/demo_race.csv", x = demo_race, row.names=F, na = "")















# ## merge wide and participants ####
# participants$src_subject_id = sub("sub-NDAR", "NDAR_", participants$participant_id)
# participants = unique(participants[, c("src_subject_id", "matched_group")])
# dataset_wide = unique(merge(dataset_wide, participants))









