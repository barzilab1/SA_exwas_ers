library(data.table)
library(sjPlot)
library(readr)
library(tableone)
library(performance)

setwd("~/sa_exwas")

testing_df = read_csv("data/df_testing_imputed.csv")
setDT(testing_df)
covariates = c("age_at_screen", "sex", "race_black" ,"race_white", "ethnicity_new" )
outcome = "bhssu04"

#########################
#### 1. ERS Analysis ####
#########################
mod1 = glm(paste0(outcome, " ~ ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod2 = glm(paste0(outcome, " ~ ers_z + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)

tab_model(mod1, mod2, show.intercept = F, show.ngroups = T, show.aic = T, file = "outputs/main_models.xls") 

anova(mod1, mod2, test="Chisq")

r2_nagelkerke(mod1) # 0.02547021 
r2_nagelkerke(mod2) # 0.193269



#############################
#### 2. ERS by subgroups ####
#############################

testing_df[, hist(ers_z )]

testing_df[, wilcox.test(ers_z ~ TRANS )]
testing_df[, kruskal.test(ers_z ~ TRANS )]
CreateTableOne(var = "ers_z", strata = "TRANS", data = testing_df, addOverall = T)

testing_df[, kruskal.test(ers_z ,race_eth, p.adj='fdr')]
testing_df[, pairwise.wilcox.test(ers_z ,race_eth, p.adj='fdr')]
CreateTableOne(var = "ers_z", strata = "race_eth", data = testing_df, addOverall = T)



######################
#### 3. Quintiles ####
######################
testing_df$q_group = dplyr::ntile(testing_df$ers_z, 5)
testing_df$q_group = factor(testing_df$q_group, levels = c(3,1,2,4,5))

modq1 = glm(paste0(outcome," ~ q_group + ", paste(covariates, collapse = " + ")), family = binomial, data = testing_df)
summary(modq1)

tab_model(modq1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T)


CreateTableOne(var = "q_group", factorVars = "q_group", strata = outcome, data = testing_df, addOverall = T)

