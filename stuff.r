# re-run for number of columns....
for(i in 1:nrow(synonym_all)){
  if(!is.na(synonym_all$syn3[i])){
    synonyms_append$acceptedName <- synonym_all$scientificName[i] 
    synonyms_append$scientificName <- synonym_all$syn3[i]
    synonyms <- rbind(synonyms, synonyms_append)
  } else {
    # nothing to do here
  }
}
