# Teresa's merge

Lewis <- read.csv("~/GitHub/tpt-siphonaptera/output/Lewis_DwC.csv", na = "NA") # read in cleaned Lewis review file
NMNH <- read.csv("~/GitHub/tpt-siphonaptera/output/NMNH_DwC.csv", na = "NA") # read in cleaned NMNH review file
FMNH <- read.csv("~/GitHub/tpt-siphonaptera/output/FMNH_DwC.csv", na = "NA") # read in cleaned FMNH review file
CoL <- read.csv("~/GitHub/tpt-siphonaptera/output/CoL_DwC.csv", na = "NA") # read in cleaned CoL review file
GBIF <- read.csv("~/GitHub/tpt-siphonaptera/output/GBIF_DwC.csv", na = "NA") # read in cleaned GBIF review file

# Names in sources other than Lewis
FMNH_not_Lewis <- FMNH[which(FMNH$canonicalName %!in% Lewis$canonicalName),]
NMNH_not_Lewis <- NMNH[which(NMNH$canonicalName %!in% Lewis$canonicalName),]
CoL_not_Lewis <- CoL[which(CoL$canonicalName %!in% Lewis$canonicalName),]
GBIF_not_Lewis <- GBIF[which(GBIF$canonicalName %!in% Lewis$canonicalName),]

df1 <- GBIF_not_Lewis # change dataframe name for ease of use

# if canonical is in CoL, add the source
for (i in 1:nrow(df1)){
  if (df1$canonicalName[i] %in% CoL$canonicalName){
    df1$source[i] <- paste(df1$source[i],"CoL", sep = ", ")
  }
}

df2 <- CoL_not_Lewis[which(CoL_not_Lewis$canonicalName %!in% df1$canonicalName),] # if CoL canonical is not in working file, get it
df1 <- rbind.fill(df1, df2) # add CoL not in GBIF to working file

# if canonical is in NMNH, add the source
for (i in 1:nrow(df1)){
  if (df1$canonicalName[i] %in% NMNH$canonicalName){
    df1$source[i] <- paste(df1$source[i],"NMNH", sep = ", ")
  }
}

df2 <- NMNH_not_Lewis[which(NMNH_not_Lewis$canonicalName %!in% df1$canonicalName),] # if NMNH canonical is not in working file, get it
df1 <- rbind.fill(df1, df2) # add NMNH not in GBIF to working file

# if canonical is in FMNH, add the source
for (i in 1:nrow(df1)){
  if (df1$canonicalName[i] %in% FMNH$canonicalName){
    df1$source[i] <- paste(df1$source[i],"FMNH", sep = ", ")
  }
}

df2 <- FMNH_not_Lewis[which(FMNH_not_Lewis$canonicalName %!in% df1$canonicalName),] # if FMNH canonical is not in working file, get it
df1 <- rbind.fill(df1, df2) # add FMNH not in GBIF to working file

write.csv(df1,"~/GitHub/tpt-siphonaptera/output/Not in Lewis.csv", row.names = FALSE) # names not in Lewis


# Lewis names in sources other than Lewis
FMNH_in_Lewis <- FMNH[which(FMNH$canonicalName %in% Lewis$canonicalName),]
NMNH_in_Lewis <- NMNH[which(NMNH$canonicalName %in% Lewis$canonicalName),]
CoL_in_Lewis <- CoL[which(CoL$canonicalName %in% Lewis$canonicalName),]
GBIF_in_Lewis <- GBIF[which(GBIF$canonicalName %in% Lewis$canonicalName),]

df <- Lewis # change dataframe name for ease of use

# if canonical is in CoL, add the source
for (i in 1:nrow(df)){
  if (df$canonicalName[i] %in% CoL$canonicalName){
    df$source[i] <- paste(df$source[i],"CoL", sep = ", ")
  }
}

# if canonical is in FMNH, add the source
for (i in 1:nrow(df)){
  if (df$canonicalName[i] %in% FMNH$canonicalName){
    df$source[i] <- paste(df$source[i],"FMNH", sep = ", ")
  }
}

# if canonical is in GBIF, add the source
for (i in 1:nrow(df)){
  if (df$canonicalName[i] %in% GBIF$canonicalName){
    df$source[i] <- paste(df$source[i],"GBIF", sep = ", ")
  }
}

# if canonical is in NMNH, add the source
for (i in 1:nrow(df)){
  if (df$canonicalName[i] %in% NMNH$canonicalName){
    df$source[i] <- paste(df$source[i],"NMNH", sep = ", ")
  }
}

write.csv(df,"~/GitHub/tpt-siphonaptera/output/in Lewis.csv", row.names = FALSE) # names in Lewis

# remove BOLD Names
GBIF_BOLD <- df1[which(startsWith(df1$scientificName, "BOLD:")),] # get the BOLD names
df1 <- df1[which(startsWith(df1$scientificName, "BOLD:") == FALSE),] # remove BOLD names

# df1$suggested <- NA # initialize suggestion column
# 
# # Euhoplopsyllus vs Euchoplopsyllus
# for (i in 1:nrow(df1)){
#   if (!is.na(df1$genus[i])){
#     if (df1$genus[i] == "Euchoplopsyllus"){
#       pick <- gsub("Euchoplopsyllus", "Euhoplopsyllus", df1$canonicalName[i])
#       df1$suggested[i] <- vlookup(df$canonicalName,pick,df$canonicalName)
#     }
#   }
# }
# 
# # Amalareus vs Amalaraeus
# for (i in 1:nrow(df1)){
#   if (!is.na(df1$genus[i])){
#     if (df1$genus[i] == "Amalareus"){
#       pick <- gsub("Amalareus", "Amalaraeus", df1$canonicalName[i])
#       df1$suggested[i] <- vlookup(df$canonicalName,pick,df$canonicalName)
#     }
#   }
# }
# 
# # match up species with subspecies
# dfsub <- df[which(df$specificEpithet == df$infraspecificEpithet),] # get the original subspecies
# dfsub$spec <- paste(dfsub$genus,dfsub$specificEpithet, sep = " ") # remove the infraspecific epithet
# 
# for (i in 1:nrow(df1)){
#   if (df1$canonicalName[i] %in% dfsub$spec){
#     df1$suggested[i] <- paste(df1$genus[i],df1$specificEpithet[i],df1$specificEpithet[i], sep = " ")    
#   } else {
#     df1$suggested[i] <- NA
#   }
# }
# 
# # match up subspecies with species
# for (i in 1:nrow(df1)){
#   if (is.na(df1$suggested[i])){
#     if (!is.na(df1$infraspecificEpithet[i])){
#       # if (df1$specificEpithet[i] == df1$infraspecificEpithet[i]){
#         pick <- paste(df1$genus[i],df1$infraspecificEpithet[i], sep = " ")
#         df1$suggested[i] <- vlookup(df$canonicalName,pick,df$canonicalName)
#       # }
#     }
#   }
# }
# 
# # Match genera
# df2 <- df1[which(is.na(df1$suggested)),] # Get stuff without suggestions
# df1 <- df1[which(!is.na(df1$suggested)),] # remove stuff without suggestions from working file
# 
# df$pick <- right(df$canonicalName," ")
# df$pickauth <- gsub("[()]","",df$scientificNameAuthorship)
# 
# for (i in 1:nrow(df2)){
#     pick <- right(df2$canonicalName[i]," ")
#     pickauth <- gsub("[()]","",df2$scientificNameAuthorship[i])
#     temp <- df[which(df$pick == pick),]
#     df2$suggested[i] <- vlookup(temp$canonicalName,gsub("[()]","",df2$scientificNameAuthorship[i]),temp$pickauth)
# }
# 
# df$pick <- NULL # remove temporary column
# df$pickauth <- NULL # remove temporary column
# 
# df1 <- rbind(df1,df2) # return suggested names

# get columns for Hastriter review
# review <- df1[c("source","family","subfamily","genus","specificEpithet","infraspecificEpithet","scientificNameAuthorship","taxonomicStatus","canonicalName","Hastriter_suggested","Hastriter_note")]
# review <- review[with(review, order(review$canonicalName)),]

# add Hastriter Comments to "not_in Lewis"
Hastriter <- read_excel("~/GitHub/tpt-siphonaptera/input/not_in_Lewis 12 July 2021.xlsx", col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text","text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))
df1$Hastriter_suggested <- vlookup(Hastriter$Hastriter_suggested,df1$canonicalName,Hastriter$canonicalName)
df1$Hastriter_note <- vlookup(Hastriter$Hastriter_note,df1$canonicalName,Hastriter$canonicalName)
df1$Hastriter_family <- vlookup(Hastriter$Hastriter_family,df1$canonicalName,Hastriter$canonicalName)

write.csv(df1,"~/GitHub/tpt-siphonaptera/output/not_in_Lewis.csv", row.names = FALSE) # write out names for review
# write.csv(all_names,"~/GitHub/tpt-siphonaptera/output/merged_names.csv", row.names = FALSE) # all names

