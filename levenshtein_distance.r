# check Levenshtein's Distance (e.g., misspellings) [may need to do before canonical name generation]
# Watch for: Ornithodoros vunkeri; Ornithodoros yukeri; Ornithodoros yunkeri
library(stringdist)
temp <- c()
similar_names <-c()
compared_names <- c()
cutoff_distance <- 2
df2 <- c()
io <- FALSE
for(i in 1:length(df$canonicalName)){
  if(!(df$canonicalName[i] %in% similar_names)){ # testing
    for(j in 1:length(df$canonicalName)){
      score <- stringdist(df$canonicalName[i], df$canonicalName[j], "dl")
      temp <- c(temp, score)
    }
    if(any(temp %in% c(1:cutoff_distance))){
      if(io){
        df2 <- cbind(df2, temp)
        wc = wc + 1
      } else {
        df2 <- as.data.frame(temp)
        rownames(df2) <- df$canonicalName
        io <- TRUE
        wc <- 1
      }
      colnames(df2)[which(colnames(df2) == "temp")] <- df$canonicalName[i]
      similar <- rownames(df2)[which(df2[,wc]==min(df2[,wc][which(df2[,wc]>0)]))]
      comp_name <- rep(df$canonicalName[i], length(similar))
      similar_names <- c(similar_names, similar)
      compared_names <- c(compared_names, comp_name)
    }
    temp <- c()
  }
  if(i %% 10 == 0){
    print(paste('Completed iteration:', i, 'out of', length(df$canonicalName), 'iterations (', round(i/length(df$canonicalName),2)*100,'% DONE)'))
  }
}
print('FINISHED!')
check_mat <- as.data.frame(cbind(compared_names, similar_names)) # create matched names data frame
write.csv(check_mat,"~/GitHub/ixodes-tpt/output/matched_names_review.csv", row.names = FALSE) # write out matched names for review
print('proceed to missing higher taxa')