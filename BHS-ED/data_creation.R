library(bigrquery)
library(data.table)

bq_auth(path = "/vault/secrets/gcp-sa-default.json")
project_id = bq_projects()

# patient data
query_patient = 'select pat_id, sex_abbr, race, ethnicity, gender_identity_name from arcus.patient'

#bhs data
query_bhs = 'select * from arcus.bhs_chop_2013_06_21_to_2020_05_04'

# census_track_code data
query_census = 'select pat_id, fips_census_track_code from arcus.patient_address_history'

# coverage data
query_coverage = 'select * from arcus.coverage'

# blood pressure
query_blood_pressure = 'select bhs.pat_id, recorded_age, value_type_name, flowsheet_value, started_at_datetime
                        from arcus.bhs_chop_2013_06_21_to_2020_05_04 bhs
                        inner join arcus.flowsheet_measure fsm
                        on fsm.pat_id = bhs.pat_id
                        and fsm.survey_start_datetime = bhs.started_at_datetime
                        where fsm.value_type_name = "Blood Pressure";'

# bmi
query_bmi = 'select bhs.pat_id, bmi , started_at_datetime
             from arcus.bhs_chop_2013_06_21_to_2020_05_04 as bhs
             INNER JOIN arcus.bmi as bmi_t 
             ON bmi_t.pat_id=bhs.pat_id AND bmi_t.survey_start_datetime=bhs.started_at_datetime;'


patients = bq_table_download(bq_project_query(project_id, query_patient), bigint = "integer64")
bhs_main = bq_table_download(bq_project_query(project_id, query_bhs), bigint = "integer64")
census_track_code = bq_table_download(bq_project_query(project_id, query_census), bigint = "integer64")
coverage_data = bq_table_download(bq_project_query(project_id, query_coverage), bigint = "integer64")
blood_pressure_data = bq_table_download(bq_project_query(project_id, query_blood_pressure), bigint = "integer64")
bmi_data = bq_table_download(bq_project_query(project_id, query_bmi), bigint = "integer64")


bhs_main[,grep("facility|date_(referred|attended)|extracted_date", colnames(bhs_main), value = T)] = NULL
bhs_main = bhs_main[,colSums(is.na(bhs_main)) != nrow(bhs_main)]

bhs_questions = grep("^bhs[^c]|race|hispan|gender", colnames(bhs_main), value = T)
bhs_main[,bhs_questions] = lapply(bhs_main[,bhs_questions], function(x) ifelse(x %in% c( "I don't know the answer", "I don't want to answer"),NA,x))

patients[patients == "Refused" | patients == "Unknown"] = NA


write.csv(file = "data/bhs_main.csv",x = bhs_main, row.names = F, na = "")  
write.csv(file = "data/patient.csv",x = patients, row.names = F, na = "") 
write.csv(file = "data/patient_address.csv", x = census_track_code, row.names = F, na = "" )
write.csv(file = "data/coverage.csv", x = coverage_data, row.names = F, na = "" )
write.csv(file = "data/blood_pressure.csv", x = blood_pressure_data, row.names = F, na = "" )
write.csv(file = "data/bmi.csv", x = bmi_data, row.names = F, na = "" )


