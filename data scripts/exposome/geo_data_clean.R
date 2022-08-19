
source("config.R")
source("utility_fun.R")


########### School Risk and Protective Factors ########### 
rhds01 = load_instrument("abcd_rhds01",abcd_files_path)

rhds01 = rhds01[, grep("^(src|interview|event|sex)|addr1_", colnames(rhds01))]
rhds01 = rhds01[, grep("(status|years|addr1_id)$", colnames(rhds01), invert = T)]

# make sure there is no overlap between feature names
colnames(rhds01)[grep("reshist_addr1_(no2|pm25|leadrisk)$", colnames(rhds01))] = paste0(colnames(rhds01)[grep("reshist_addr1_(no2|pm25|leadrisk)$", colnames(rhds01))], "_e")

rhds01_wide = get_wide_data(rhds01)

write.csv(file = "data/geo_data.csv",x = rhds01_wide, row.names = F, na = "")


