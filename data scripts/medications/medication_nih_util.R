library(httr)
library(jsonlite)
library(plyr)

# https://lhncbc.nlm.nih.gov/RxNav/
# https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=1000492
# https://lhncbc.nlm.nih.gov/RxNav/APIs/api-RxClass.getClassByRxNormDrugId.html
# https://lhncbc.nlm.nih.gov/RxNav/applications/RxClassIntro.html



get_first_level = function(med_number,relaSource){
 
  url = paste0("https://rxnav.nlm.nih.gov/REST/rxclass/class/byRxcui.json?relaSource=",relaSource,"&rxcui=",med_number)
  res_get <- GET(url)
  if(res_get$status_code != 200){
    print(paste0("get first level failed with status code: ", res_get$status_code))
    print(paste0("relaSource: ", relaSource," medication: ",med_number ))
    return()
  }
  
  res_json = fromJSON(rawToChar(res_get$content))$rxclassDrugInfoList$rxclassDrugInfo
  return(data.frame(activeIngredient = res_json$minConcept$name, classId = res_json$rxclassMinConceptItem$classId))
  
}


get_all_classes = function(class_id, relaSource){
  
  url = paste0("https://rxnav.nlm.nih.gov/REST/rxclass/classContext.json?relaSource=",relaSource,"&classId=",class_id)
  res_get <- GET(url)
  if(res_get$status_code != 200){
    print(paste0("get all levels failed with status code: ", res_get$status_code))
    print(paste0("relaSource: ", relaSource," class: ",class_id ))
    return()
  }
  
  return(fromJSON(rawToChar(res_get$content))$classPathList$classPath$rxclassMinConcept)
  
}

get_ATC_levels = function(class_id){
  
  res = get_all_classes(class_id, "ATC")
  if (length(res) > 1){
    print(paste0("ATC has more than 1 path ", class_id))
  }
  
  res = res[[1]]
  res$atcClass = paste0("ATC",nrow(res) - as.numeric(rownames(res)))
  res = reshape(res, direction = "wide", idvar = "classType", timevar = "atcClass", sep = "_")
  return(res)
}


get_diseases_levels = function(class_id){
  
  res = get_all_classes(class_id, "MEDRT")
  res = rbind.fill(res)
  
  # remove 2 top levels 
  res = res[!res$classId %in% c("X1", "X3"),]
  disease = unique(unlist(res$className))
  return(paste(disease,collapse = " / "))
}


get_ATC_classes = function(meds_df) {
  
  new_meds_df = data.frame()
  
  for (i in 1:nrow(meds_df)) {
    first_level = get_first_level(meds_df$number[i], "ATC")
    # no result - skip to next med
    if(is.null(first_level) || 0 == nrow(first_level)){
      new_meds_df = rbind.fill(new_meds_df, meds_df[i,])
      next
    }
    
    # get hierarchy for each first level 
    for (j in 1:nrow(first_level)){
      atc_levels = get_ATC_levels(first_level$classId[j])
      temp_row = cbind(meds_df[i,], activeIngredient=first_level$activeIngredient[j],atc_levels)
      new_meds_df = rbind.fill(new_meds_df,temp_row)
    }
  }
  
  return(new_meds_df)
}


get_diseases = function(meds_df) {
  
  new_meds_df = data.frame()
  
  for (i in 1:nrow(meds_df)) {
    first_level = get_first_level(meds_df$number[i],"MEDRT&relas=may_treat&relas=may_prevent&relas=may_diagnose&relas=induces&relas=CI_with")
    # no result - skip to next med
    if(is.null(first_level) || 0 == nrow(first_level)){
      new_meds_df = rbind.fill(new_meds_df, meds_df[i,])
      next
    }
    
    diseases = list()
    # get hierarchy for each first level 
    for (j in 1:nrow(first_level)){
      diseases = append(diseases,get_diseases_levels(first_level$classId[j]))
    }
    diseases = unique(diseases)
    diseases = paste(diseases,collapse = " || ")
    temp_row = cbind(meds_df[i,], diseases)
    new_meds_df = rbind.fill(new_meds_df,temp_row)
  }
  
  return(new_meds_df)
  
}





