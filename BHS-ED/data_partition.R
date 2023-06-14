library(nbpMatching)

#### split the data ####

#### TODO add zipcode for the partition? gender?

df = readRDS("exwas/data/dataset_individual_full.rds")
df = df[ visit_number == 1 ,]


cov = c("pat_id", "sex", "age_at_screen", "ethnicity_new")
df_rw = df[race == "White", ..cov]
df_rb = df[race == "Black/African American", ..cov]
df_r_nb_nw = df[!(race %in% c("Black/African American","White")) , .SD, .SDcols = c("race",cov)]

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


df.match$halves
#OR
df.1$matches$halves

group1 = c(df.match1$halves$Group1.ID, df.match2$halves$Group1.ID, df.match3$halves$Group1.ID)
group2 = c(df.match1$halves$Group2.ID, df.match2$halves$Group2.ID, df.match3$halves$Group2.ID)

df[pat_id %in% group1, group := 1]
df[pat_id %in% group2, group := 2]
df[,table(group)]


library(tableone)
category_f = c( "sex", "race", "ethnicity_new", "bhssu04") 
tab <- CreateTableOne(vars = c(category_f, "age_at_screen"), data = df, factorVars = category_f , strata = "group", addOverall = T)
table1 <- print(tab, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)
table1


saveRDS(df[,c("pat_id", "group")], "exwas/data/matched_groups.rds")

#### check corr #### 
train_df = df[group == 1,]
cor1 =cor(train_df[,13:21], use = "complete.obs")
cor2 =cor(train_df[,c(12,14:21)], use = "complete.obs")

library(caret)
findCorrelation(cor1, cutoff = .9, exact = T, names = T, verbose = T) 
findCorrelation(cor2, cutoff = .9, exact = T, names = T, verbose = T) 

