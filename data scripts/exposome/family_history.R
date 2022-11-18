library(psych)

source("config.R")
source("utility_fun.R")


########### family history ########### 
# fhx.1 = load_instrument("fhxp102",abcd_files_path)
# fhx.2 = load_instrument("fhxp201",abcd_files_path)
# 
# fh_dataset = merge(fhx.1, fhx.2)
# fh_dataset = fh_dataset[, grep("^(src|interview|event|sex)|famhx_4_p$|_yes_no$", colnames(fh_dataset))]
# 
# fh_dataset[fh_dataset == 7|fh_dataset == 999] = NA
# describe(fh_dataset)
# 
# ###rename variables to meaningful names
# colnames(fh_dataset)[6] = "FH_ALCOHOL"
# colnames(fh_dataset)[7] = "FH_DRUGS"
# colnames(fh_dataset)[8] = "FH_DEPRESSION"
# colnames(fh_dataset)[9] = "FH_BIPOLAR"
# colnames(fh_dataset)[10] = "FH_PSYCHOSIS"
# colnames(fh_dataset)[11] = "FH_EXTERNALIZING"
# colnames(fh_dataset)[12] = "FH_NERVES"
# colnames(fh_dataset)[13] = "FH_MENTAL_HEALTH_PROFFESSIONAL"
# colnames(fh_dataset)[14] = "FH_PSYCHIATRIC_HOSPITALIZATION"
# colnames(fh_dataset)[15] = "FH_SUICIDALITY"


########### Parent Family History Summary Scores ###########
fhxssp = load_instrument("abcd_fhxssp01",abcd_files_path)
fhxssp = fhxssp[,grepl("^(src|sex|inter|event)|momdad_scd_p", colnames(fhxssp))]


write.csv(file = "data/family_history.csv",x = fhxssp, row.names = F, na = "")

