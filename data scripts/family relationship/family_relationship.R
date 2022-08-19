
source("config.R")
source("utility_fun.R")

########### family relationship section ###########
acspsw03 = load_instrument("acspsw03",abcd_files_path)
acspsw03 = acspsw03[acspsw03$eventname == "baseline_year_1_arm_1",grepl("src|inter|sex|event|fam", colnames(acspsw03))]


write.csv(file = "data/family_relationship.csv",x = acspsw03, row.names = F, na = "")
