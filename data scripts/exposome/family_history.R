library(psych)

source("config.R")
source("utility_fun.R")


########### Parent Family History Summary Scores ###########
fhxssp = load_instrument("abcd_fhxssp01",abcd_files_path)
fhxssp = fhxssp[,grepl("^(src|sex|inter|event)|momdad_scd_p", colnames(fhxssp))]


write.csv(file = "data/family_history.csv",x = fhxssp, row.names = F, na = "")

