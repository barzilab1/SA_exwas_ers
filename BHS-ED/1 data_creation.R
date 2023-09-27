library(bigrquery)
library(data.table)
library(janitor)

bq_auth(path = "/vault/secrets/gcp-sa-default.json")
project_id = bq_projects()
setwd("~/sa_exwas")

# patient data
query_patient = 'select pat_id, sex_abbr, race, ethnicity, gender_identity_name from arcus.patient'

#bhs data
query_bhs = 'select * from arcus.era2_bhs_with_enc_062823'

# coverage data
query_coverage = 'select cov.payor_category, enco.pat_id, enco.encounter_id
                  from arcus.coverage as cov
                  INNER JOIN arcus.encounter as enco
                  ON cov.coverage_id = enco.coverage_id;'

# # blood pressure
# query_blood_pressure = 'select bhs.pat_id, recorded_age, value_type_name, flowsheet_value, started_at_datetime
#                         from arcus.bhs_chop_2013_06_21_to_2020_05_04 bhs
#                         inner join arcus.flowsheet_measure fsm
#                         on fsm.pat_id = bhs.pat_id
#                         and fsm.survey_start_datetime = bhs.started_at_datetime
#                         where fsm.value_type_name = "Blood Pressure";'
# 
# # bmi
# query_bmi = 'select bhs.pat_id, bmi , started_at_datetime
#              from arcus.bhs_chop_2013_06_21_to_2020_05_04 as bhs
#              INNER JOIN arcus.bmi as bmi_t 
#              ON bmi_t.pat_id=bhs.pat_id AND bmi_t.survey_start_datetime=bhs.started_at_datetime;'


patients = bq_table_download(bq_project_query(project_id, query_patient), bigint = "integer64")
bhs_main = bq_table_download(bq_project_query(project_id, query_bhs), bigint = "integer64")
coverage_data = bq_table_download(bq_project_query(project_id, query_coverage), bigint = "integer64")
# blood_pressure_data = bq_table_download(bq_project_query(project_id, query_blood_pressure), bigint = "integer64")
# bmi_data = bq_table_download(bq_project_query(project_id, query_bmi), bigint = "integer64")

# remove redundant columns 
bhs_main[,grep("facility|date_(referred|attended)|extracted_date|(t|e)_datetime|form|date_of_birth|insurance_", colnames(bhs_main), value = T)] = NULL

# convert to NA and lower strings that are not ID
char_cols = sapply(bhs_main, is.character) & !grepl("_id", names(bhs_main))
bhs_main[char_cols] = lapply(bhs_main[char_cols], \(x) ifelse(x == "NA", NA, tolower(x)))

bhs_questions = grep("^bhs[^c]|sx02|race|hispan|gender", colnames(bhs_main), value = T)
bhs_main[,bhs_questions] = lapply(bhs_main[,bhs_questions], \(x) ifelse(x %in% c( "i don't know the answer", "i don't want to answer"),NA,x))
bhs_main = remove_empty(bhs_main)

patients[patients == "Refused" | patients == "Unknown"] = NA

write.csv(file = "data/bhs_main.csv",x = bhs_main, row.names = F, na = "")  
write.csv(file = "data/patient.csv",x = patients, row.names = F, na = "") 
write.csv(file = "data/coverage.csv", x = coverage_data, row.names = F, na = "" )
# write.csv(file = "data/blood_pressure.csv", x = blood_pressure_data, row.names = F, na = "" )
# write.csv(file = "data/bmi.csv", x = bmi_data, row.names = F, na = "" )


