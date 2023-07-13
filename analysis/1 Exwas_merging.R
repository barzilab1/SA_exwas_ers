library(readr)
library(janitor)
library(qgraph)
library(caret)

source("config.R")
source("utility_fun.R")

################################### 
#### 1. load and merge suicide ####
################################### 

family_relationship <- read_csv("data/family_relationship.csv")
site <- read_csv("data/site.csv")
suicide <- read_csv("data/suicide_long.csv")

suicide$interview_date = NULL

suicide_site = merge(suicide, site)
suicide_site = suicide_site[complete.cases(suicide_site$SA_y) ,]
suicide_site = merge(suicide_site, family_relationship[,c("src_subject_id","sex","rel_family_id")])


participants = read.table(file = file.path(abcd_partition_path, "participants.tsv"), sep = '\t', header = TRUE)
participants$src_subject_id = sub("sub-NDAR", "NDAR_", participants$participant_id)
participants = unique(participants[, c("src_subject_id", "matched_group")]) 


# merge participants
suicide_site = merge(suicide_site, participants)

DV_suicide_train = suicide_site[suicide_site$matched_group == 1,]
DV_suicide_test = suicide_site[suicide_site$matched_group == 2,]

write.csv(file = "data/DV_suicide_train.csv", x = DV_suicide_train, row.names=F, na = "")
write.csv(file = "data/DV_suicide_test.csv", x = DV_suicide_test, row.names=F, na = "")


######################################### 
#### 2. load and merge exposome data ####
######################################### 

demographics <- read_csv("data/demographics_all.csv")
exposome_sum <- read_csv("data/exposome_sum_set.csv")
exposome_item <- read_csv("data/exposome_set_item.csv")
lifestyle <- read_csv("data/lifestyle_item.csv")

# structural_level <- read_csv("data/geo_data.csv") 

# merge all exposome features
individual_level = merge(demographics, exposome_sum)
individual_level = merge(individual_level, exposome_item)
individual_level = merge(individual_level, lifestyle)

# remove observations with no DV
individual_level = merge(individual_level, suicide_site[,c("src_subject_id", "eventname")])
# structural_level = merge(structural_level, suicide_site[,c("src_subject_id", "eventname")])


# remove features with more than 10% missing data 
remove_cols_with_na = function(df){
  
  for (timepoint in unique(df$eventname)) {
    sub_dataset = df[df$eventname == timepoint,]
    vari_delete = colnames(sub_dataset)[which( colSums(is.na(sub_dataset)) >= 0.10*nrow(sub_dataset))]
    df[df$eventname == timepoint, vari_delete] = NA
  }

  df = remove_empty(df, which = "cols")

  return(df)
}

individual_level = remove_cols_with_na(individual_level)
# structural_level = remove_cols_with_na(structural_level)


# remove columns with sd = 0
zero_sd_cols = sapply(individual_level, \(x) is.numeric(x) && sd(x, na.rm = T) == 0)
individual_level = individual_level[, !zero_sd_cols]



# check correlation and remove above 0.9
corr_data = individual_level[,grep("src|^sex|interview|event", colnames(individual_level), invert = T)]

# 563 - has issue to correlate with  others  
corrs = cor_auto(corr_data[,c(1:562,564:603)])
saveRDS(corrs, file = "outputs/corr_all_except_563.rds")

df_temp = data.frame( matrix(0,nrow = 1, ncol = ncol(corrs)), row.names = "su_tlfb_ecig_lt_calc")
colnames(df_temp) = colnames(corrs)

#TODO add column for su_tlfb_ecig_lt_calc with value =1 



corr_featuers = findCorrelation(corrs, cutoff = .9, exact = T, names = T, verbose = T) 
individual_level[,corr_featuers] = NULL




# Compare row 587  and column  596 with corr  1 
# Means:  0.621 vs 0.122 so flagging column 587 
colnames(corrs)[c(587,596)]
# su_tlfb_mj_drink_use_calc Marijuana infused alcohol drinks; 0 replaces blank answers in tlfb_mj_drink_use when tlfb_mj=0
# su_tlfb_cbd_follow_up___4 What type of CBD product did they use? - Edible, capsule, or pill 


# Compare row 535  and column  596 with corr  1 
# Compare row 539  and column  596 with corr  1 
# Compare row 519  and column  514 with corr  1 
# Compare row 540  and column  593 with corr  1 
# Compare row 542  and column  514 with corr  1 
# Compare row 517  and column  512 with corr  1 
# Compare row 541  and column  514 with corr  1 
# Compare row 552  and column  591 with corr  0.995 
# Compare row 596  and column  545 with corr  0.95 
# Compare row 551  and column  553 with corr  1 
# Compare row 553  and column  512 with corr  1 
# Compare row 597  and column  545 with corr  0.95 
# Compare row 520  and column  514 with corr  1 
# Compare row 545  and column  531 with corr  0.947 
# Compare row 528  and column  583 with corr  0.99 
# Compare row 546  and column  531 with corr  0.947 
# Compare row 531  and column  595 with corr  0.929 
# Compare row 544  and column  570 with corr  0.995 
# Compare row 547  and column  395 with corr  1 
# Compare row 548  and column  512 with corr  1 
# Compare row 516  and column  536 with corr  0.934 
# Compare row 595  and column  575 with corr  0.995 
# Compare row 538  and column  395 with corr  1 
# Compare row 515  and column  518 with corr  0.977 
# Compare row 518  and column  536 with corr  0.966 
# Compare row 554  and column  591 with corr  0.926 
# Compare row 537  and column  576 with corr  0.995 
# Compare row 588  and column  534 with corr  1 
# Compare row 534  and column  602 with corr  1 
# Compare row 549  and column  593 with corr  0.92 
# Compare row 584  and column  573 with corr  0.999 
# Compare row 583  and column  564 with corr  0.999 
# Compare row 543  and column  532 with corr  0.931 
# Compare row 530  and column  550 with corr  0.938 
# Compare row 533  and column  586 with corr  0.971 
# Compare row 536  and column  570 with corr  0.995 
# Compare row 526  and column  509 with corr  0.97 
# Compare row 532  and column  591 with corr  0.995 
# Compare row 550  and column  514 with corr  0.999 
# Compare row 524  and column  593 with corr  0.973 
# Compare row 593  and column  576 with corr  0.995 
# Compare row 573  and column  566 with corr  0.999 
# Compare row 529  and column  513 with corr  0.999 
# Compare row 527  and column  590 with corr  0.999 
# Compare row 586  and column  570 with corr  0.995 
# Compare row 591  and column  513 with corr  0.995 
# Compare row 590  and column  510 with corr  1 
# Compare row 379  and column  395 with corr  1 
# Compare row 566  and column  512 with corr  0.999 
# Compare row 509  and column  589 with corr  1 
# Compare row 589  and column  572 with corr  0.986 
# Compare row 513  and column  585 with corr  1 
# Compare row 585  and column  567 with corr  0.999 
# Compare row 514  and column  575 with corr  0.966 
# Compare row 499  and column  503 with corr  0.92 
# Compare row 512  and column  565 with corr  0.995 
# Compare row 567  and column  574 with corr  0.999 
# Compare row 525  and column  594 with corr  0.991 
# Compare row 503  and column  569 with corr  0.995 
# Compare row 562  and column  508 with corr  0.999 
# Compare row 564  and column  571 with corr  0.999 
# Compare row 571  and column  511 with corr  0.999 
# Compare row 523  and column  570 with corr  0.995 
# Compare row 594  and column  508 with corr  0.999 
# Compare row 498  and column  570 with corr  0.995 
# Compare row 36  and column  37 with corr  0.905 
# Compare row 510  and column  570 with corr  0.995 
# Compare row 572  and column  565 with corr  0.995 
# Compare row 570  and column  565 with corr  0.999 
# Compare row 574  and column  474 with corr  1 
# Compare row 508  and column  486 with corr  1 
# Compare row 511  and column  474 with corr  1 
# Compare row 565  and column  568 with corr  0.999 
# Compare row 568  and column  569 with corr  0.999 
# Compare row 569  and column  563 with corr  0.999 
# Compare row 563  and column  474 with corr  0.995 
# Compare row 301  and column  300 with corr  0.95 
# Compare row 269  and column  240 with corr  0.932 
# Compare row 474  and column  602 with corr  1 
# Compare row 237  and column  555 with corr  0.993 
# Compare row 238  and column  522 with corr  0.995 
# Compare row 470  and column  476 with corr  0.973 
# Compare row 292  and column  284 with corr  0.988 
# Compare row 476  and column  602 with corr  1 
# Compare row 41  and column  575 with corr  0.995 
# Compare row 371  and column  4 with corr  0.995 
# Compare row 235  and column  576 with corr  0.995 
# Compare row 284  and column  293 with corr  0.924 
# Compare row 239  and column  486 with corr  1 
# Compare row 209  and column  16 with corr  0.999 
# Compare row 454  and column  575 with corr  0.995 
# Compare row 293  and column  285 with corr  0.977 
# Compare row 475  and column  602 with corr  1 
# Compare row 576  and column  236 with corr  0.995 
# Compare row 479  and column  602 with corr  1 
# Compare row 480  and column  602 with corr  1 
# Compare row 461  and column  466 with corr  0.926 
# Compare row 233  and column  486 with corr  1 
# Compare row 469  and column  602 with corr  1 
# Compare row 575  and column  502 with corr  0.995 
# Compare row 472  and column  602 with corr  1 
# Compare row 291  and column  283 with corr  0.999 
# Compare row 285  and column  319 with corr  0.964 
# Compare row 478  and column  602 with corr  1 
# Compare row 468  and column  602 with corr  1 
# Compare row 449  and column  446 with corr  0.908 
# Compare row 473  and column  602 with corr  1 
# Compare row 471  and column  477 with corr  0.905 
# Compare row 477  and column  602 with corr  1 
# Compare row 294  and column  319 with corr  0.907 
# Compare row 2  and column  3 with corr  0.998 
# Compare row 288  and column  280 with corr  0.956 
# Compare row 290  and column  282 with corr  0.958 
# Compare row 561  and column  507 with corr  0.942 
# Compare row 234  and column  485 with corr  1 
# Compare row 283  and column  319 with corr  0.995 
# Compare row 145  and column  230 with corr  0.995 
# Compare row 485  and column  556 with corr  0.995 
# Compare row 485  and column  507 with corr  1 
# Compare row 15  and column  210 with corr  0.999 
# Compare row 501  and column  493 with corr  0.966 
# Compare row 559  and column  522 with corr  0.999 
# Compare row 522  and column  581 with corr  0.999 
# Compare row 456  and column  467 with corr  0.905 
# Compare row 95  and column  578 with corr  0.994 
# Compare row 490  and column  558 with corr  0.99 
# Compare row 490  and column  555 with corr  0.931 
# Compare row 490  and column  557 with corr  0.987 
# Compare row 441  and column  577 with corr  0.995 
# Compare row 175  and column  172 with corr  0.928 
# Compare row 273  and column  245 with corr  0.92 
# Compare row 581  and column  521 with corr  0.998 
# Compare row 3  and column  1 with corr  0.999 
# Compare row 488  and column  577 with corr  0.994 
# Compare row 459  and column  482 with corr  0.995 
# Compare row 521  and column  341 with corr  0.995 
# Compare row 270  and column  242 with corr  0.965 
# Compare row 496  and column  493 with corr  0.999 
# Compare row 580  and column  601 with corr  0.995 
# Compare row 59  and column  579 with corr  0.995 
# Compare row 493  and column  500 with corr  0.999 
# Compare row 249  and column  248 with corr  0.999 
# Compare row 592  and column  582 with corr  0.985 
# Compare row 327  and column  579 with corr  0.994 
# Compare row 601  and column  600 with corr  0.999 
# Compare row 601  and column  560 with corr  0.994 
# Compare row 377  and column  4 with corr  0.995 
# Compare row 258  and column  319 with corr  0.992 
# Compare row 579  and column  63 with corr  0.991 
# Compare row 159  and column  320 with corr  0.995 
# Compare row 104  and column  320 with corr  0.995 
# Compare row 122  and column  230 with corr  0.995 
# Compare row 492  and column  231 with corr  0.993 
# Compare row 4  and column  368 with corr  0.995 
# Compare row 182  and column  183 with corr  0.911 
# Compare row 230  and column  297 with corr  0.995 
# Compare row 199  and column  200 with corr  0.906 
# Compare row 7  and column  9 with corr  0.922 











# scale and check corr
scale_features = function(df){
  
  range = sapply(df[,grep("src|^sex|event|inter", colnames(df), invert = T)], range, na.rm = T)
  cols_to_scale = names(which(range[2,]-range[1,] > 1)) # 1 = binary, <1 = already scaled 
  cols_to_scale_z = paste0(cols_to_scale, "_z")
  df[,cols_to_scale_z] = scale(df[,cols_to_scale])
  df[,cols_to_scale] = NULL
  return(df)
  
}

individual_level = scale_features(individual_level)

individual_level_train = merge(individual_level, DV_suicide_train[,c("src_subject_id", "eventname")])
write.csv(file = "data/individual_level_train.csv", x = individual_level_train, row.names=F, na = "")


individual_level_test = merge(individual_level, DV_suicide_test[,c("src_subject_id", "eventname")])
write.csv(file = "data/individual_level_test.csv", x = individual_level_test, row.names=F, na = "")







