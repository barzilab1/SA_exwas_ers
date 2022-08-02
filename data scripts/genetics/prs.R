library(plyr)

source("config.R")


load_psr <- function(input_list) {
  
  tables = list()
  for(file_name in input_list){
    print(file_name)
    
    instrument = read.csv(file = file_name, sep = '\t',header = TRUE, 	row.names=NULL, na.string = c("","NA"), check.names=FALSE)
    
    #get the new name for Residual_PRS from the file name 
    new_col_name = regmatches(file_name, gregexpr("(?<=_ABCD_(AFR|EUR)_).*?(?=.txt)", file_name, perl=T))[[1]]
    # top_10_col_name = paste0(new_col_name,"_top10")
    # quantiles_col_name = paste0(new_col_name,"_quantiles")
    # 
    # #create binary variable for top 10%
    # instrument[[quantiles_col_name]] <-  (as.numeric(cut(instrument$Residual_PRS, 
    #                                         breaks=quantile(instrument$Residual_PRS, probs=seq(0,1, by=0.10)), 
    #                                         include.lowest=T, right = F, labels = 1:10)) -1)*10
    # 
    # instrument[[top_10_col_name]] = ifelse(instrument[[quantiles_col_name]] >= 90, 1,0)
    
    
    
    #rename Residual_PRS col
    colnames(instrument)[grep("Residual_PRS" ,colnames(instrument))] = new_col_name
    
    #fix sex range
    instrument$SEX = instrument$SEX - 1
    #fix ids
    instrument$src_subject_id = paste0("NDAR_" , instrument$IID)
    
    # select relevant features
    required_features  = c("src_subject_id", "SEX", new_col_name)
    #check if FID exists
    if("FID" %in% colnames(instrument)){
      required_features = c(required_features,"FID")
    }
    
    tables[[file_name]] = instrument[,required_features]
  }
  
  for(i in 2:length(tables)){
    tables[[1]] = merge(tables[[1]], tables[[i]])
  }
  
  return(tables[[1]])
}


#collect all of the ABCD txt files
input_list_afr = Sys.glob(paths = c(paste0(prs_files_path,"*ABCD_AFR*.txt"),
                                    paste0(prs_files_path,"Suicide/ABCD_AFR_PRSice/","*ABCD_AFR*[0-9].txt"),
                                    paste0(prs_files_path,"ADHD/ABCD_AFR_PRSice/","*ABCD_AFR*.txt")))
input_list_eur = Sys.glob(paths = c(paste0(prs_files_path,"*ABCD_EUR*.txt"),
                                    paste0(prs_files_path,"Suicide/ABCD_EUR_PRSice/","*ABCD_EUR*[0-9].txt"),
                                    paste0(prs_files_path,"ADHD/ABCD_EUR_PRSice/","*ABCD_EUR*[0-9].txt")))


afr_data = load_psr(input_list_afr)
eur_data = load_psr(input_list_eur)


afr_data$genetic_afr = 1
eur_data$genetic_afr = 0


genetic = rbind.fill(eur_data,afr_data)

ids = which(colnames(genetic) %in% c("FID", "SEX"))
colnames(genetic)[ids] = paste0(colnames(genetic)[ids], "_genetic")


# set.seed(131)
# library(data.table)
# one_family_member = setDT(genetic)[, one_family_member_genetic := {
#   fcase(
#     age == max(.N), 1,
#     default = 0
#   )
# } ,by = FID_genetic]


write.csv(file = "outputs/genetic.csv",x = genetic, row.names=F, na = "")





