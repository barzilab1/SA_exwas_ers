library(psych)

source("config.R")
source("utility_fun.R")


########### Residential History Derived Scores ########### 
rhds01 = load_instrument("abcd_rhds01",abcd_files_path)

rhds01 = rhds01[, grep("^(src|interview|event|sex)|reshist_(addr1|state)_", colnames(rhds01))]
rhds01 = rhds01[, grep("(status|years|addr1_id)$", colnames(rhds01), invert = T)]

# rhds01 = rhds01[!is.na(rhds01$reshist_addr1_valid) & rhds01$reshist_addr1_valid == 1,]
# clean reshist_addr1 features in case it is not valid or NA
reshist_addr1_features = grep("reshist_addr1", colnames(rhds01), value = T)
reshist_addr1_features = reshist_addr1_features[-which(reshist_addr1_features == "reshist_addr1_valid")]
rhds01[is.na(rhds01$reshist_addr1_valid) | rhds01$reshist_addr1_valid == 0, reshist_addr1_features] = NA
rhds01[,c("reshist_addr1_valid","reshist_addr1_move_out_year", "reshist_addr1_move_in_year")] = NULL

rhds01$reshist_state_mj_laws_b = ifelse(rhds01$reshist_state_mj_law < 4 , 1, 0)
rhds01$reshist_addr1_urban_area_b = ifelse(rhds01$reshist_addr1_urban_area > 1 , 1, 0)
rhds01[,c("reshist_state_mj_laws","reshist_addr1_urban_area")] = NULL


col_names = grep("reshist_addr1_coi_c5", colnames(rhds01), value = T)
col_names_b = paste0(col_names, "_b")
rhds01[, col_names_b] = ifelse(rhds01[, col_names] == 1, 1,0)
rhds01[, col_names] = NULL


describe(rhds01[grep("reshist_addr1_svi", colnames(rhds01))])
temp = rhds01[,grep("reshist_addr1_svi", colnames(rhds01))]
temp[temp == -999] = NA
rhds01[,grep("reshist_addr1_svi", colnames(rhds01))] = temp

describe(rhds01[grep("reshist_addr1_adi", colnames(rhds01))])
View(describe(rhds01[grep("reshist_addr1_coi", colnames(rhds01))]))
describe(rhds01[grep("reshist_addr1_opat", colnames(rhds01))])
describe(rhds01[grep("reshist_addr1_scan", colnames(rhds01))])
describe(rhds01[grep("reshist_state", colnames(rhds01))])
describe(rhds01[grep("src|event|interv|reshist_(state|addr1_(svi|adi|coi|opat|scan))", colnames(rhds01), invert = T)])


# remove 3 year follow up
rhds01 = rhds01[rhds01$eventname != "3_year_follow_up_y_arm_1", ]


write.csv(file = "data/geo_data.csv",x = rhds01, row.names = F, na = "")


