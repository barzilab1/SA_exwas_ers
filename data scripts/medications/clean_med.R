library(readxl)
library(data.table)

source("config.R")
source("utility_fun.R")

medsy01 = load_instrument("medsy01",abcd_files_path)

#remove odd values
colnames_to_clean = colnames(medsy01)[which(grepl("<span style=", medsy01))]
for(colname in colnames_to_clean){
  ind = which(grepl("<span style=", medsy01[[colname]]))
  medsy01[[colname]][ind] = NA
}

#remove empty col
medsy01 = medsy01[, colSums(is.na(medsy01)) != nrow(medsy01)]


med_dataset = medsy01[,grepl("(src|interview_a|gender|event|brou|^med(.)*_rxnorm(_1yr)*_p$|med(.)*_2wk_p)", colnames(medsy01))]

setDT(med_dataset)
# split the med name to get the numbers 
cols_to_fix = grep("^med(.)*_rxnorm(_1yr)*_p$", colnames(med_dataset), value = T)
for (j in cols_to_fix) 
  set(med_dataset, j = j, value = sapply(strsplit(med_dataset[[j]]," "), `[`, 1))

# remove .0 at the end of the number
for (j in cols_to_fix) 
  set(med_dataset, j = j, value = as.numeric(med_dataset[[j]]))


# copy the medications number from last 1 year to last 2 wk
for (j in 1:15) {
  
  col_name_1y = paste0("med",j,"_rxnorm_1yr_p")
  col_name_1y_otc = paste0("med_otc_",j,"_rxnorm_1yr_p")
  col_name_2w = paste0("med",j,"_2wk_p")
  col_name_2w_otc = paste0("med_otc_",j,"_2wk_p")
  
  new_col_name = paste0("med",j,"_rxnorm_2wk_p")
  new_col_name_otc = paste0("med_otc_",j,"_rxnorm_2wk_p")
  
  med_dataset[get(col_name_2w) ==1, (new_col_name) := .SD, .SDcols= col_name_1y] 
  med_dataset[get(col_name_2w_otc) ==1, (new_col_name_otc) := .SD, .SDcols= col_name_1y_otc]
}

# med_dataset[,View(.SD), .SDcols = c(grepl("med_otc_10_(rxnorm_)?(2wk|1yr)", colnames(med_dataset)))]


#######################################################  
# create the medication table according to the tagging
#######################################################
tagged_med = read_excel(paste0(additional_files_path, "coded_meds_07012022.xlsx"))

#add medication category to each child according to tagging
last_2wk_colnames = grep("_rxnorm_(2wk_)?p$",colnames(med_dataset),value = T)
last_1yr_colnames = grep("_rxnorm_1yr_p$",colnames(med_dataset),value = T)

setDF(med_dataset)
for(i in 13:ncol(tagged_med)){
  
  #get med category
  colname = colnames(tagged_med)[i]
  colname_1yr = paste0(colname,"_1yr")
  colname_2w = paste0(colname,"_2w")
  colname_1yr_sum = paste0(colname_1yr,"_total")
  colname_2w_sum = paste0(colname_2w,"_total")
  
  #get the medications in the category
  meds = unique(tagged_med$number[which(tagged_med[,i] == 1)])
  
  #tag the kids
  med_dataset[,c(colname_2w, colname_1yr)] = 0
  med_dataset[apply(med_dataset[,last_2wk_colnames], 1, function(r) any(r %in% meds)), colname_2w] = 1
  med_dataset[, colname_2w_sum] = apply(med_dataset[,last_2wk_colnames], 1, function(r) {s = sum(r %in% meds)
  ifelse(s==0,NA,s)})
  med_dataset[apply(med_dataset[,last_1yr_colnames], 1, function(r) any(r %in% meds)), colname_1yr] = 1
  med_dataset[, colname_1yr_sum] = apply(med_dataset[,last_1yr_colnames], 1, function(r) {s = sum(r %in% meds)
  ifelse(s==0,NA,s)})
  
}


#remove medications for parents that refused to answer (brought_medications == 2) or NA /////brought_medications_1yr_p
# med_dataset[brought_medications %in% c(2,NA), get(cols_to_fix) := NA ]


write.csv(file = paste0("outputs/medications.csv"),x = med_dataset ,row.names=F, na = "")






# tagged_med_k = read_excel("~/Desktop/KWH 20220513_coded_medication[3].xlsx")# read.csv(paste0(additional_files_path, "20220214_coded_medication.csv"))
# tagged_med_M = read_excel("~/Desktop/MMA6_16codedmeds.xlsx")# read.csv(paste0(additional_files_path, "20220214_coded_medication.csv"))
# 
# library(arsenal)
# 
# dif=diffs(comparedf(tagged_med_k, tagged_med_M), vars = c("supplement"))
# marissa_1_rows = dif$..row.names..[dif$values.y==1]
# Migraine.Medications_m = unique(tagged_med_M[marissa_1_rows,c("number","name")])
# View(Migraine.Medications_m)



