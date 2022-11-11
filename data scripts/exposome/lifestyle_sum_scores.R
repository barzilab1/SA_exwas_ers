source("config.R")
source("utility_fun.R")

########### Sum Scores Mobil Tech Youth ########### 
ssmty = load_instrument("abcd_ssmty01",abcd_files_path)
ssmty = ssmty[, !grepl("_(nm|nt)$", colnames(ssmty))]
summary(ssmty)


########### Summary Scores Sports Activity ########### 
spacss= load_instrument("abcd_spacss01",abcd_files_path)
spacss[spacss == 999] = NA
spacss = spacss[,grep("src|sex|event|interview|_nmonth_", colnames(spacss), value = T)]


########### Longitudinal Summary Scores Sports Activity ########### 
lsssa = load_instrument("abcd_lsssa01",abcd_files_path)
lsssa[lsssa == 999] = NA
lsssa = lsssa[,grep("src|sex|event|interview|_nmonth_", colnames(lsssa), value = T)]


### combine the 2 instruments 
colnames(lsssa) = sub("_l", "", colnames(lsssa))
sssa = rbind.fill(spacss, lsssa)
describe(sssa)


########## ABCD Sum Scores Physical Health Parent ###########
ssphp01 = load_instrument("abcd_ssphp01", abcd_files_path)

#select variables
ssphp01 = ssphp01[,grepl("src|event|interview|sex|cna.*_sum$",colnames(ssphp01))]


################### Summary Scores Substance Use ################### 
suss = load_instrument("abcd_suss01", abcd_files_path)
suss = suss[,!grepl("_(nt|nm)((_l)?)$",colnames(suss))]

describe(suss)

