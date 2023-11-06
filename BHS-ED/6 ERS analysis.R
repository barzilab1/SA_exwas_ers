library(data.table)
library(sjPlot)
library(readr)
library(tableone)

setwd("~/sa_exwas")

testing_df = read_csv("data/df_testing_imputed.csv")
setDT(testing_df)


#########################
#### 1. ERS Analysis ####
#########################
covariates = c("age_at_screen", "sex", "race_black" ,"race_white", "ethnicity_new" )
outcome = "bhssu04"


mod1 = glm(paste0(outcome, " ~ ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod2 = glm(paste0(outcome, " ~ ers_z + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)

t1 = tab_model(mod1, mod2, show.intercept = F, show.ngroups = T, show.aic = T) 
t1

anova(mod1, mod2, test="Chisq")

r2_nagelkerke(mod1) # 0.02488547 
r2_nagelkerke(mod2) # 0.1859619



#############################
#### 2. ERS by subgroups ####
#############################

testing_df[, wilcox.test(ers_z ~ TRANS )]
testing_df[, kruskal.test(ers_z ~ TRANS )]
CreateTableOne(var = "ers_z", strata = "TRANS", data = testing_df, addOverall = T)

testing_df[, kruskal.test(ers_z ,race_eth, p.adj='fdr')]
testing_df[, pairwise.wilcox.test(ers_z ,race_eth, p.adj='fdr')]
CreateTableOne(var = "ers_z", strata = "race_eth", data = testing_df, addOverall = T)


