
source("config.R")
source("utility_fun.R")



################### Youth Diagnostic Interview for DSM-5 Background Items 5 (lgbt/bullying/drop a class) ################### 
yksad01 = load_instrument("abcd_yksad01",abcd_files_path)

yksad01 = yksad01[, !grepl("grade|drop|det|remote", colnames(yksad01))]
summary(yksad01)

yksad01[yksad01 == "777"] = NA

# kbi_y_sex_orient and kbi_y_trans_id: 4 - I do not understand the question --> NA
yksad01$kbi_y_sex_orient[yksad01$kbi_y_sex_orient == 4] = NA
yksad01$kbi_y_trans_id[yksad01$kbi_y_trans_id == 4] = NA

yksad01$LGBT = (yksad01$kbi_y_sex_orient == 1 | yksad01$kbi_y_trans_id == 1)*1
yksad01$LGBT = ifelse( (is.na(yksad01$LGBT) & (yksad01$kbi_y_sex_orient %in% c(2,3) | yksad01$kbi_y_trans_id %in% c(2,3) )),
                       0, yksad01$LGBT)

#View(yksad01[c("LGBT", "kbi_y_sex_orient" , "kbi_y_trans_id" )])

yksad01$LGBT_inclusive = (yksad01$kbi_y_sex_orient <= 2 | yksad01$kbi_y_trans_id <= 2)*1
yksad01$LGBT_inclusive = ifelse( (is.na(yksad01$LGBT_inclusive) & (yksad01$kbi_y_sex_orient  == 3 | yksad01$kbi_y_trans_id == 3 )),
                                 0, yksad01$LGBT_inclusive)

# View(yksad01[c("LGBT", "LGBT_inclusive","kbi_y_sex_orient" , "kbi_y_trans_id" )])

yksad01$sex = NULL
write.csv(file = "data/lgbtqia.csv",x = yksad01, row.names = F, na = "")

