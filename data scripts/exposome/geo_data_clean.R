
source("config.R")
source("utility_fun.R")


########### Residential History Derived Scores ########### 
rhds01 = load_instrument("abcd_rhds01",abcd_files_path)

rhds01 = rhds01[, grep("^(src|interview|event|sex)|reshist_(addr1|state)_", colnames(rhds01))]
rhds01 = rhds01[, grep("(status|years|addr1_id)$", colnames(rhds01), invert = T)]

rhds01 = rhds01[!is.na(rhds01$reshist_addr1_valid) & rhds01$reshist_addr1_valid == 1,]
rhds01[,c("reshist_addr1_valid","reshist_addr1_move_out_year", "reshist_addr1_move_in_year")] = NULL

rhds01$reshist_state_mj_laws_b = ifelse(rhds01$reshist_state_mj_law < 4 , 1, 0)
rhds01$reshist_addr1_urban_area_b = ifelse(rhds01$reshist_addr1_urban_area >1 , 1, 0)
rhds01[,c("reshist_state_mj_law","reshist_addr1_urban_area")] = NULL

# scale columns 
rhds01_range = sapply(rhds01[,grep("reshist", colnames(rhds01))], range, na.rm = T)
cols_to_scale = names(which(rhds01_range[2,]-rhds01_range[1,] >= 1e+04))
rhds01[,cols_to_scale] = scale(rhds01[,cols_to_scale])

# remove 3 year follow up
rhds01 = rhds01[rhds01$eventname != "3_year_follow_up_y_arm_1", ]

write.csv(file = "data/geo_data.csv",x = rhds01, row.names = F, na = "")


