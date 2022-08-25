
source("config.R")
source("utility_fun.R")


########### School Risk and Protective Factors ########### 
rhds01 = load_instrument("abcd_rhds01",abcd_files_path)

rhds01 = rhds01[, grep("^(src|interview|event|sex)|addr1_", colnames(rhds01))]
rhds01 = rhds01[, grep("(status|years|addr1_id)$", colnames(rhds01), invert = T)]

# make sure there is no overlap between feature names
colnames(rhds01)[grep("reshist_addr1_(no2|pm25|leadrisk)$", colnames(rhds01))] = paste0(colnames(rhds01)[grep("reshist_addr1_(no2|pm25|leadrisk)$", colnames(rhds01))], "_e")

rhds01 = rhds01[rhds01$reshist_addr1_valid == 1,]
rhds01[,c("reshist_addr1_valid")] = NULL
rhds01_wide = get_wide_data(rhds01)

# scale columns 
rhds01_wide_range = sapply(rhds01_wide[,grep("reshist", colnames(rhds01_wide))], range, na.rm = T)
cols_to_scale = names(which(rhds01_wide_range[2,]-rhds01_wide_range[1,] >= 1e+04))
rhds01_wide[,cols_to_scale] = scale(rhds01_wide[,cols_to_scale])


write.csv(file = "data/geo_data.csv",x = rhds01_wide, row.names = F, na = "")


