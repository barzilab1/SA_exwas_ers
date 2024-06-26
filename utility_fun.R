library(readr)

load_instrument <- function(file_name, file_path) {
  
  instrument = read.csv(file = file.path(file_path,paste0(file_name,".txt")), sep = '\t',header = TRUE,
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

# remove columns with low information/signal
remove_low_signal_cols = function(df){
  
  for (timepoint in unique(df$eventname)) {
    sub_dataset = df[df$eventname == timepoint,] 
    vari_delete = sapply(sub_dataset, \(x) is.numeric(x) && (sum(x != 0, na.rm = T) / sum(!is.na(x))  < 0.01) )
    df[df$eventname == timepoint, names(which(vari_delete))] = NA
  }
  
  df = remove_empty(df, which = "cols")
}


scale_features <- function(df){
  
  ranges = sapply(df[,grep("src|^sex|event|inter", colnames(df), invert = T)], range, na.rm = T)
  cols_to_scale = names(which(ranges[2,]-ranges[1,] > 1)) # 1 = binary, <1 = already scaled 
  cols_to_scale_z = paste0(cols_to_scale, "_z")
  print(cols_to_scale)
  # lapply(cols_to_scale, \(x) hist(df[[x]] , main = x))
  df[,cols_to_scale_z] = scale(df[,cols_to_scale])
  # lapply(cols_to_scale_z, \(x) hist(df[[x]] , main = x))
  df[,cols_to_scale] = NULL
  return(df)
  
}

# remove features with more than 10% missing data 
remove_cols_with_na = function(df){
  
  for (timepoint in unique(df$eventname)) {
    sub_dataset = df[df$eventname == timepoint,]
    N_rows = nrow(sub_dataset)
    vari_delete = colnames(sub_dataset)[which( colSums(is.na(sub_dataset)) >= 0.10*N_rows)]
    df[df$eventname == timepoint, vari_delete] = NA
  }
  
  df = remove_empty(df, which = "cols")
  
  return(df)
}


remove_outliers <- function(df) {
  
  cols_range = sapply(df[,grep("src|^sex|event|inter", colnames(df), invert = T)], range, na.rm = T)
  cols_to_check_outliers = names(which(cols_range[2,]-cols_range[1,] >= 6)) 
  
  timepoints = unique(df$eventname)
  for (col_name in cols_to_check_outliers) {
    for (timepoint in timepoints) {
      # if no data in the current time point, skip 
      if (sum(!is.na(df[df$eventname == timepoint, col_name])) == 0) {next} 
      # boxplot(df[df$eventname == timepoint, col_name], main = paste0(col_name, " " , timepoint))
      df[df$eventname == timepoint, col_name] = winsor(df[df$eventname == timepoint, col_name],trim=0.005)
      # boxplot(df[df$eventname == timepoint, col_name], main = paste0(col_name, " (winsor)"))
    }
  }
  
  return(df)
}


