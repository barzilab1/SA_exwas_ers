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
query_coverage = 'select * from arcus.coverage '

patients = bq_table_download(bq_project_query(project_id, query_patient), bigint = "integer64")
bhs_main = bq_table_download(bq_project_query(project_id, query_bhs), bigint = "integer64")
census_track_code = bq_table_download(bq_project_query(project_id, query_census), bigint = "integer64")
coverage_data = bq_table_download(bq_project_query(project_id, query_coverage), bigint = "integer64")


bhs_main[,grep("facility|date_(referred|attended)|extracted_date", colnames(bhs_main), value = T)] = NULL
bhs_main = bhs_main[,colSums(is.na(bhs_main)) != nrow(bhs_main)]

bhs_questions = grep("^bhs[^c]|race|hispan|gender", colnames(bhs_main), value = T)
bhs_main[,bhs_questions] = lapply(bhs_main[,bhs_questions], function(x) ifelse(x %in% c( "I don't know the answer", "I don't want to answer"),NA,x))

patients[patients == "Refused" | patients == "Unknown"] = NA


write.csv(file = "exwas/data/bhs_main.csv",x = bhs_main, row.names = F, na = "")  
write.csv(file = "exwas/data/patient.csv",x = patients, row.names = F, na = "") 
write.csv(file = "exwas/data/patient_address.csv", x = census_track_code, row.names = F, na = "" )
write.csv(file = "exwas/data/coverage.csv", x = coverage_data, row.names = F, na = "" )

