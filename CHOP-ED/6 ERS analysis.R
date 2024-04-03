library(data.table)
library(sjPlot)
library(readr)
library(tableone)
library(performance)
library(pROC)

setwd("~/sa_exwas")


get_marginal_AUC <- function(mod){
  
  y_predicted <- predict(mod, type ="response")
  y_test = model.frame(mod)[[outcome]]
  roc_object <- roc( y_test, y_predicted)
  print(round(auc(roc_object), digits = 4))
  return(roc_object)
  
}


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

roc1 = get_marginal_AUC(mod1) # 0.6058
roc2 = get_marginal_AUC(mod2) # 0.7813
roc.test(roc1, roc2)


####### Top 5 risk features #######
# supp table 5
Top5_risk = c("bhst03a", "bhssa04", "bhst04", "bhsb03_br", "bhsb02_br")
models_list = list()
for(exposure in Top5_risk){
  models_list[[exposure]] = glm(paste0(outcome, " ~ ", paste(c(exposure, covariates), collapse = " + ") ), family = binomial, data = testing_df)
  print(exposure)
  print(r2_nagelkerke(models_list[[exposure]])*100)
}

tab_model(list(mod2, models_list), 
          show.intercept = F, digits.p = 4,  show.r2 = T, file = "outputs/top5_risk_protective.xls")


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


#######  run models by subgroup ####### 
setDF(testing_df)

# supp table 8
testing_df$race_eth = factor(testing_df$race_eth, levels = c("NH-White", "NH-Black", "Hispanic"))
mod1_int = glm(paste0(outcome, " ~ ers_z * race_black + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)
mod2_int = glm(paste0(outcome, " ~ ers_z * ethnicity_new + ", paste(covariates, collapse = " + ") ), family = binomial, data = testing_df)

tab_model(mod2, mod1_int, mod2_int,
          show.intercept = F, show.ngroups = T, show.aic = T)


races = na.omit(levels(testing_df$race_eth))
for (race in races) {
  mod2r = glm(paste0(outcome, " ~ ers_z + ", paste(c("age_at_screen", "sex"), collapse = " + ") ), family = binomial, data = testing_df[testing_df$race_eth %in% race ,])
  
  print(tab_model(mod2r, show.intercept = F, show.ngroups = T, show.aic = T, file = paste0("outputs/chop_ers_",paste0(race, collapse = ""),".xls")))
  
  print(race)
  print(r2_nagelkerke(mod2r)*100)
}


######################
#### 3. Quintiles ####
######################
testing_df$q_group = dplyr::ntile(testing_df$ers_z, 5)
testing_df$q_group = factor(testing_df$q_group, levels = c(3,1,2,4,5))

modq1 = glm(paste0(outcome," ~ q_group + ", paste(covariates, collapse = " + ")), family = binomial, data = testing_df)
summary(modq1)

tab_model(modq1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T)


# q group [1]	0.46	0.33 – 0.62	<0.001
# q group [2]	0.44	0.32 – 0.61	<0.001
# q group [4]	1.98	1.58 – 2.49	<0.001
# q group [5]	5.77	4.70 – 7.12	<0.001


CreateTableOne(var = "q_group", factorVars = "q_group", strata = outcome, data = testing_df, addOverall = T)

