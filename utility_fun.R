library(readr)

load_instrument <- function(file_name, file_path) {
  
  instrument = read.csv(file = paste0(file_path,file_name,".txt"), sep = '\t',header = TRUE,
                        row.names=NULL, na.string = c("","NA"), check.names=FALSE)
  
  #remove details line
  instrument=instrument[-1,]
  
  #drop columns introduced by NDA, they are not required in the instruments.
  instrument = instrument[,!(names(instrument) %in% c(paste0(file_name,"_id"), "collection_id", "collection_title", "promoted_subjectkey","subjectkey" ,"study_cohort_name", "dataset_id"))]
  
  #if visit was used instead of eventname, rename
  if ("visit" %in% names(instrument) ){
    ind = which(names(instrument) == "visit")
    names(instrument)[ind] = "eventname"
    print("eventname replaced visit")
  }
  
  #remove empty columns (and print their names)
  instrument = instrument[,colSums(is.na(instrument)) != nrow(instrument)]
  
  instrument = droplevels(instrument)
  
  
  #convert to numeric
  for (i in 1:ncol(instrument)) {
    
    tryCatch({
      if(typeof(instrument[,i]) == "character"){
        instrument[,i] = as.numeric(instrument[,i])
      }else if (typeof(instrument[,i]) == "factor"){
        instrument[,i] = as.numeric(as.character(instrument[,i]))
      }
    }, error = function(e) {
      print(colnames(instrument)[i])
      print(e)
    }, warning = function(e){
      print(colnames(instrument)[i])
      print(e)
    })
    
  }
  
  
  return(instrument)
}


get_wide_data = function(data, max_features = NULL){
  #### TODO: delete
  data = fix_release_4_sex(data)
  
  
  # remove 3 year follow up and empty columns 
  data = data[data$eventname != "3_year_follow_up_y_arm_1", ]
  data = data[, colSums(is.na(data)) != nrow(data)]
  
  # get the columns to work with 
  colnames_to_clean = grep("src|interview|sex|event",colnames(data), value = T, invert = T)
  
  # create timepoint feature for the wide dataset
  data$timepoint = regmatches(data$eventname, regexpr(".*_year", data$eventname))
  data[, c("interview_age", "interview_date", "eventname")] = NULL
  
  data_wide = reshape(data, direction = "wide", idvar = c("src_subject_id", "sex"), timevar = "timepoint", sep = "_")
  data_wide = data_wide[,colSums(is.na(data_wide)) != nrow(data_wide)]
  
  
  for (col_name in colnames_to_clean) {
    
    # get relevant columns from wide 
    cols_wide = grep(paste0(col_name, "_"), colnames(data_wide), value = T)
    
    # if there is only one column, no need to update it
    if(length(cols_wide) == 1) {
      print(paste0("one timepoint: ", col_name))
      next
    }
    
    # create the summary variable
    if(!is.null(max_features) & col_name %in% max_features){
      # max
      print(paste0("max:       ", col_name, "     vari: ",paste(cols_wide, collapse = " | ")))
      new_col_name = paste0(col_name, "_max")
      data_wide[,new_col_name] = apply(data_wide[,cols_wide], 1, function(r){
        if(all(is.na(r))) {return(NA)}                            
        max(r, na.rm = T)
      })
    }else{
      # check range
      col_range = range(data[,col_name], na.rm = T)
      if(col_range[2]-col_range[1] == 1 ){
        # binary
        print(paste0("binary:    ", col_name, "     vari: ",paste(cols_wide, collapse = " | ")))
        new_col_name = paste0(col_name, "_ever")
        data_wide[,new_col_name] = apply(data_wide[,cols_wide], 1, function(r){
          if(all(is.na(r))) {return(NA)}                            
          any(r == col_range[2], na.rm = T)*1
          })
      }else{
        # continues 
        print(paste0("continues: ", col_name, "     vari: ",paste(cols_wide, collapse = " | ")))
        new_col_name = paste0(col_name, "_mean")
        data_wide[,new_col_name] = rowMeans(data_wide[,cols_wide], na.rm = T)
      }
    }
    
    data_wide[,cols_wide] = NULL
  }
  
  # keep only features with at least 80% data
  data_wide = data_wide[, colSums(is.na(data_wide)) <= .2*nrow(data_wide)]
  data_wide = data_wide[, sapply(data_wide, function(x){ is.na(sd(x)) | sd(x)> 0 })]
  print(paste0("# of cols: " , length(colnames(data_wide))-2))
  
  return(data_wide)
  
}
