#### get all medications
library(tidyr)

source("config.R")
source("utility_fun.R")
source("data scripts/medications/medication_nih.R")

medsy01 = load_instrument("medsy01",abcd_files_path)


#remove odd values
colnames_to_clean = colnames(medsy01)[which(grepl("<span style=", medsy01))]
for(colname in colnames_to_clean){
  ind = which(grepl("<span style=", medsy01[[colname]]))
  medsy01[[colname]][ind] = NA
}

#remove empty col
medsy01 = medsy01[,!colSums(is.na(medsy01)) == nrow(medsy01)]

######################################## 
# get a list of all meds in the dataset
######################################## 
meds_cols_names = grep("^med(.)*_rxnorm_p$|med(.)*_rxnorm_1yr_p", colnames(medsy01), value = T)

# get the entire list of meds
meds = na.exclude(unique(unlist(medsy01[meds_cols_names])))
meds = data.frame(full_name = meds)

# seperate the meds number and name
# \S matches any non-whitespace character (equivalent to [^\r\n\t\f\v ])
# \K resets the starting point of the reported match. Any previously consumed characters are no longer included in the final match
# \s matches any whitespace character (equivalent to [\r\n\t\f\v ])
meds = separate(meds, full_name, into = c("number", "name"), sep = "^\\S*\\K\\s+")
#bug while reading the txt file: fix numbers that include ".0" at the end of the number. a few numbers start with a letter 
meds$number = ifelse(is.na(as.numeric(meds$number)), meds$number, as.numeric(meds$number))
# remove duplicates following the bug fix 
meds = meds[!duplicated(meds),]
rownames(meds) = NULL
#clean duplicates - when the med name is long, it doesn't always include all of it
meds$number[which(duplicated(meds$number))] # 9 duplicates 
# keep the rows with the longest description 
meds = meds[-c(1359,2344,2476,2523,2608,2739,2888,2971,1292),]


################## 
# get atc classes 
################## 
meds_atc = get_ATC_classes(meds)
sum(is.na(meds_atc$activeIngredient)) #940 are missing info. some just don't have ATC classification. some are inactive. 
meds_atc[,c("classType","classId_ATC0", "className_ATC0")] = NULL


################
# get diseases
################
meds_diseases = get_diseases(meds)
sum(is.na(meds_diseases$diseases)) #760

meds_classified = merge(meds_diseases,meds_atc)


##########################
# get current tagged meds
##########################


### TODO - update to new version and print only the new medications?
tagged_med = read.csv(paste0(additional_files_path, "ABCD Meds KWH.csv"), encoding = )

### update the code according to the new excel version
#split the med name to get the numbers 
tagged_med = separate(tagged_med, med, into = c("number", "name"), sep = "^\\S*\\K\\s+", remove = T)
tagged_med[is.na(tagged_med)] = 0

#check that all meds are tagged
length(setdiff(meds$number, tagged_med$med_number) )
setdiff(tagged_med$med_number, meds$number) 


combine_meds = merge(meds_classified,tagged_med, all.x = T)


write.csv(file="combine_meds.csv",combine_meds,row.names=F, na = "")


library(openxlsx)

wb <- createWorkbook()

## Add a worksheet
addWorksheet(wb, "medications")

headerStyle <- createStyle(
  textDecoration = "bold", border = "Bottom"
)
writeData(wb,1,combine_meds, withFilter = T, headerStyle = headerStyle)

i =1
while(i <= nrow(combine_meds)){ # loop over rows for merging
  
  j = 0
  while (combine_meds$number[i+j] == combine_meds$number[i+j+1]) {
    j = j+1
  }
  
  range_to_merge = seq(i+1, i+j+1)
  mergeCells(wb, 1, cols = 1, rows = range_to_merge)
  mergeCells(wb, 1, cols = 2, rows = range_to_merge)
  mergeCells(wb, 1, cols = 3, rows = range_to_merge)
  
  i = i+j+1
}




saveWorkbook(wb, "mergeCellsExample.xlsx", overwrite = TRUE)


