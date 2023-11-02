library(readr)
library(data.table)
library(nbpMatching)
library(qgraph)
library(caret)
library(psych)
library(tableone)

setwd("~/sa_exwas")

#############################
#### 1. split the cohort ####
#############################

df = read_csv("data/exposome_full_clean.csv")
setDT(df)

cov = c("pat_id", "sex", "age_at_screen", "ethnicity_new")
df_rw = df[race == "white", ..cov]
df_rb = df[race == "black or african american", .SD, .SDcols = c("bhssu04",cov)] 
df_r_nb_nw = df[!(race %in% c("black or african american","white")) , .SD, .SDcols = c("race",cov)]

set.seed(131)
# create distances
df.dist1 <- gendistance(df_rw, idcol="pat_id")
df.dist2 <- gendistance(df_rb, idcol="pat_id")
df.dist3 <- gendistance(df_r_nb_nw, idcol="pat_id")

# create distancematrix object
df.mdm1 <- distancematrix(df.dist1)
df.mdm2 <- distancematrix(df.dist2)
df.mdm3 <- distancematrix(df.dist3)

# create matches
df.match1 <- nonbimatch(df.mdm1)
df.match2 <- nonbimatch(df.mdm2)
df.match3 <- nonbimatch(df.mdm3)

# review quality of matches
# df.qom <- qom(df.dist$cov, df.match$matches)

# some helper functions are available
# runner -- start with the covariate, run through the entire process
# df.1 <- runner(df, idcol=1)


# df.match$halves
#OR
# df.1$matches$halves

group1 = c(df.match1$halves$Group1.ID, df.match2$halves$Group1.ID, df.match3$halves$Group1.ID)
group2 = c(df.match1$halves$Group2.ID, df.match2$halves$Group2.ID, df.match3$halves$Group2.ID)

df[pat_id %in% group1, group := 1]
df[pat_id %in% group2, group := 2]
df[,table(group)]
#   1    2 
# 9909 9909 


### TABLE 1 ###
category_f = c( "sex", "race", "ethnicity_new", "bhssu04", "TRANS") 
# by group
tab <- CreateTableOne(vars = c(category_f, "age_at_screen", "gender_new"), data = df, factorVars = category_f , strata = "group", addOverall = T)
table1 <- print(tab, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)
table1

# by SA
tab2 <- CreateTableOne(vars = c(category_f, "age_at_screen", "gender_new"), data = df, factorVars = category_f , strata = "bhssu04", addOverall = T)
table2 <- print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)
table2

write.csv(df[,c("pat_id", "group")], "data/matched_groups.csv", na = "", row.names = F)

#######################################
#### 2. split the exposome dataset ####
#######################################

train_df = df[group == 1,]

# columns with more then 10% missing data
missing_data_columns = train_df[, names(which(colSums(is.na(.SD)) > 0.1*.N))] #0
# columns with  with less then 0.1 information 
vari_delete = train_df[, names(which(sapply(.SD,\(x) is.numeric(x) && (sum(x != 0, na.rm = T) / sum(!is.na(x))  < 0.01))) )]

# check correlation and remove above 0.9
set.seed(131)
corr_data = train_df[ ,.SD ,.SDcols = grep("bhs(?!su04)|medi", colnames(train_df), ignore.case = T, perl = T)]
corrs = cor_auto(corr_data)
corr_featuers = findCorrelation(corrs, cutoff = .9, exact = T, names = T, verbose = T) 
train_df[, (corr_featuers) := NULL] # 18 exposures

View(as.data.frame(describe(train_df)))
train_df[, bhssa01a_z := scale(bhssa01a)]
train_df[, bhssa01a := NULL]
train_df[, bhssa03a_z := scale(bhssa03a)]
train_df[, bhssa03a := NULL]

write.csv(train_df, "data/train_df.csv", na = "", row.names = F)


testing_df = df[group == 2,]
write.csv(testing_df, "data/testing_df.csv", na = "", row.names = F)




