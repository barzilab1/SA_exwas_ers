
source("config.R")
source("utility_fun.R")

################### cbcls ###################
cbcls01 = load_instrument("abcd_cbcls01",abcd_files_path)

#get the t scores
cbcls_t_score = cbcls01[, grepl("^(src|interview|event|sex)|_t$", colnames(cbcls01))]

write.csv(file = "data/cbcl.csv",x = cbcls_t_score, row.names = F, na = "")

