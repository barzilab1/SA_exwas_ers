
source("config.R")
source("utility_fun.R")


anthropometrics = load_instrument("abcd_ant01",abcd_files_path)

anthropometrics = anthropometrics[,grepl("src|inter|sex|event|calc|cm|weight[1-3]", colnames(anthropometrics))] 

##### BMI ##### 

#get cdc percentiles table  
cdc_percentiles <- read_csv(paste0(additional_files_path, "bmiagerev.csv"))

#according to cdc, add 0.5 to the age https://www.cdc.gov/nccdphp/dnpao/growthcharts/resources/sas.htm
anthropometrics$bmi_age_months = anthropometrics$interview_age + 0.5
anthropometrics$sex_bmi = ifelse(anthropometrics$sex == "F",2,1)


#in release 4.0, the total weight is missing 
# remove outliers 
anthropometrics$anthroweight2lb[anthropometrics$anthroweight2lb > 1000] = NA

anthropometrics$anthroweightcalc = rowMeans(anthropometrics[,grep("weight[1-3]", colnames(anthropometrics))], na.rm = T)
anthropometrics[,grep("weight[1-3]", colnames(anthropometrics))] = NULL


#merge the 2 datasets so we will have a match between age and sex
BMI_dataset = merge(anthropometrics, cdc_percentiles, by.x = c("sex_bmi", "bmi_age_months"), by.y = c("Sex","Agemos"))


#calculate BMI
BMI_dataset$BMI = round( BMI_dataset$anthroweightcalc / (BMI_dataset$anthroheightcalc)^2 * 703 , digits = 1)

### Body-Mass index, remove outlier >36 or < 11 based on the recommendation from Rebecca Umbach, PhD. (ABCD official)
BMI_dataset$BMI[which(BMI_dataset$BMI>36 | BMI_dataset$BMI < 11)] = NA; 


P3_ind = which(colnames(BMI_dataset) == "P3")
P97_ind = which(colnames(BMI_dataset) == "P97")
#get percentiles
BMI_dataset$percentiles_tag = colnames(BMI_dataset)[apply(BMI_dataset, 1, function(r){ findInterval(r["BMI"], r[P3_ind:P97_ind]) + (P3_ind - 1) })]

#fix for below P3
BMI_dataset$percentiles_tag[BMI_dataset$percentiles_tag == colnames(BMI_dataset)[P3_ind-1]] = "<P3"

#numeric percentiles
BMI_dataset$bmi_percentiles = sub("P","",BMI_dataset$percentiles_tag)
BMI_dataset$bmi_percentiles = sub("<3","2",BMI_dataset$bmi_percentiles)

BMI_dataset$bmi_above_95p = (as.numeric(BMI_dataset$bmi_percentiles) >= 95)*1
BMI_dataset$bmi_above_85p = (as.numeric(BMI_dataset$bmi_percentiles) >= 85)*1


## https://github.com/carriedaymont/growthcleanr
## get exact percentile 
# BMI_dataset$bz = (((BMI_dataset$BMI / BMI_dataset$M) ^ BMI_dataset$L) - 1) / (BMI_dataset$L * BMI_dataset$S)
# BMI_dataset$exact_percentile = round(100 * pnorm(BMI_dataset$bz),2)


write.csv(file = "outputs/ABCD_BMI.csv",x = BMI_dataset[,c("src_subject_id","interview_date","interview_age","sex","eventname",
                                                                          "anthroweightcalc", "anthroheightcalc", "anthro_waist_cm",
                                                                          "BMI","bmi_above_85p","bmi_above_95p", "bmi_percentiles")],
                                                      row.names=F, na = "")






