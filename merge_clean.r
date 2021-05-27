# cleaning of merged data
df <- read.csv("~/GitHub/tpt-siphonaptera/output/Flea_merged.csv", na = "NA") # read in merged taxottols file

# remove duplicates for Levenschtein check
duplicates <- df[which(duplicated(df$canonical)),] # duplicates here
unique_names <- df[which(!duplicated(df$canonical)),] # deduplicated list

unique_names$canonical <- ifelse(is.na(unique_names$canonical),paste(unique_names$genus,unique_names$species,sep = " "),unique_names$canonical) # make sure canonical has a value

# check Levenshtein's Distance (e.g., misspellings)
library(stringdist)
temp <- c()
similar_name <-c()
name <- c()
cutoff_distance <- 2
df2 <- c()
io <- FALSE
for(i in 1:length(unique_names$canonical)){
  if(!(unique_names$canonical[i] %in% similar_name)){ # testing
    for(j in 1:length(unique_names$canonical)){
      score <- stringdist(unique_names$canonical[i], unique_names$canonical[j], "dl")
      temp <- c(temp, score)
    }
    if(any(temp %in% c(1:cutoff_distance))){
      if(io){
        df2 <- cbind(df2, temp)
        wc = wc + 1
      } else {
        df2 <- as.data.frame(temp)
        rownames(df2) <- unique_names$canonical
        io <- TRUE
        wc <- 1
      }
      colnames(df2)[which(colnames(df2) == "temp")] <- unique_names$canonical[i]
      similar <- rownames(df2)[which(df2[,wc]==min(df2[,wc][which(df2[,wc]>0)]))]
      comp_name <- rep(unique_names$canonical[i], length(similar))
      similar_name <- c(similar_name, similar)
      name <- c(name, comp_name)
    }
    temp <- c()
  }
  if(i %% 10 == 0){
    print(paste('Completed iteration:', i, 'out of', length(unique_names$canonical), 'iterations (', round(i/length(unique_names$canonical),2)*100,'% DONE)'))
  } else {
    check_mat <- as.data.frame(cbind(name, similar_name))
    print('FINISHED!')
  }
}

Lewis <- df[which(df$source == "Lewis"),] # get all the Lewis names for comparison
check_mat$name_source <- ifelse(check_mat$compared_names %in% Lewis$canonical, "Lewis", "other") # add source of compared names
check_mat$similar_source <- ifelse(check_mat$similar_names %in% Lewis$canonical, "Lewis", "other") # add source of similar names

for (i in 1:nrow(check_mat)){
  diffc = diag(attr(adist(check_mat$compared_names[i],check_mat$similar_names[i], counts = TRUE), "trafos"))
  transform(check_mat$sim_diff[i] <- regmatches(similar_names[i],regexpr("[^M]",diffc)))
}

for (i in 1:nrow(check_mat)){
  diffc = diag(attr(adist(check_mat$similar_names[i],check_mat$compared_names[i], counts = TRUE), "trafos"))
  transform(check_mat$name_diff[i] <- regmatches(compared_names[i],regexpr("[^M]",diffc)))
}

check_mat <- check_mat[,c("name", 
                          "name_source", 
                          "name_diff",
                          "similar_name", 
                          "similar_source",
                          "sim_diff")] # change column order

write.csv(check_mat,"~/GitHub/tpt-siphonaptera/output/Flea_similar_names.csv", row.names = FALSE) # write out similar names for review

# get names that have been published since 2005 and are not in Lewis
df$year <- right(df$author,",") # get year from author text
df$year <- gsub("[^0-9]", "",df$year) # leave only numbers
new_names <- df[which(df$year > 2004),] # get all names published since 2004
new_GBIF <- new_names[which(new_names$source == "GBIF"),] # get only GBIF new names
new_Lewis <- new_names[which(new_names$source == "Lewis"),] # get only Lewis new names
new_names <- new_GBIF[which(new_GBIF$canonical %!in% new_Lewis$canonical),] # get GBIF new names not in Lewis for review
write.csv(new_names,"~/GitHub/tpt-siphonaptera/output/Flea_new_names.csv", row.names = FALSE) # write out new names for review

# remove per expert
Flea_removed <- df[which(df$family == "Archipsyllidae")] # add removed rows to removed file
df <- df[which(df$family != "Archipsyllidae"),] # Archipsyllidae is not a flea family

