# Teresa's merge

Lewis <- read.csv("~/GitHub/tpt-siphonaptera/output/Lewis_DwC.csv", na = "NA") # read in cleaned Lewis review file
NMNH <- read.csv("~/GitHub/tpt-siphonaptera/output/NMNH_DwC.csv", na = "NA") # read in cleaned NMNH review file
FMNH <- read.csv("~/GitHub/tpt-siphonaptera/output/FMNH_DwC.csv", na = "NA") # read in cleaned FMNH review file
CoL <- read.csv("~/GitHub/tpt-siphonaptera/output/CoL_DwC.csv", na = "NA") # read in cleaned CoL review file
GBIF <- read.csv("~/GitHub/tpt-siphonaptera/output/GBIF_DwC.csv", na = "NA") # read in cleaned GBIF review file

# get row counts for sanity check
Lewisrow <- nrow(Lewis)
NMNHrow <- nrow(NMNH)
FMNHrow <- nrow(FMNH)
CoLrow <- nrow(CoL)
GBIFrow <- nrow(GBIF)

# Remove subgenera from source files as they are not recognized in Lewis
NMNH_subgen <- NMNH[which(NMNH$taxonRank == "subgenus"),]
NMNH <- NMNH[which(NMNH$taxonRank != "subgenus"),]
FMNH_subgen <- FMNH[which(FMNH$taxonRank == "subgenus"),]
FMNH <- FMNH[which(FMNH$taxonRank != "subgenus"),]
CoL_subgen <- CoL[which(CoL$taxonRank == "subgenus"),]
CoL <- CoL[which(CoL$taxonRank != "subgenus"),]
GBIF_subgen <- GBIF[which(GBIF$taxonRank == "subgenus"),]
GBIF <- GBIF[which(GBIF$taxonRank != "subgenus"),]

df1 <- GBIF_subgen # change dataframe name for ease of use
# add matches from other sources to GBIF_subgen not
# if canonical is in CoL, add the source
for (i in 1:nrow(df1)){
  if (df1$canonicalName[i] %in% CoL_subgen$canonicalName){
    df1$source[i] <- paste(df1$source[i],"CoL", sep = ", ")
  }
}

df2 <- CoL_subgen[which(CoL_subgen$canonicalName %!in% GBIF_subgen$canonicalName),] # if CoL canonical is not in working file, get it
df1 <- rbind.fill(df1, df2) # add CoL not in GBIF to working file

# if canonical is in NMNH, add the source
for (i in 1:nrow(df1)){
  if (df1$canonicalName[i] %in% NMNH_subgen$canonicalName){
    df1$source[i] <- paste(df1$source[i],"NMNH", sep = ", ")
  }
}

df2 <- NMNH_subgen[which(NMNH_subgen$canonicalName %!in% df1$canonicalName),] # if NMNH canonical is not in working file, get it
df1 <- rbind.fill(df1, df2) # add NMNH not in GBIF to working file

# if canonical is in FMNH, add the source
for (i in 1:nrow(df1)){
  if (df1$canonicalName[i] %in% FMNH_subgen$canonicalName){
    df1$source[i] <- paste(df1$source[i],"FMNH", sep = ", ")
  }
}

df2 <- FMNH_subgen[which(FMNH_subgen$canonicalName %!in% df1$canonicalName),] # if FMNH canonical is not in working file, get it
df1 <- rbind.fill(df1, df2) # add FMNH not in GBIF to working file
subgenera <- df1 # save subgenera to add back later

# Names in sources other than Lewis
FMNH_not_Lewis <- FMNH[which(FMNH$canonicalName %!in% Lewis$canonicalName),]
# FMNH_not_Lewis <- rbind(FMNH_not_Lewis,FMNH_subgen) # add in subgenera
NMNH_not_Lewis <- NMNH[which(NMNH$canonicalName %!in% Lewis$canonicalName),]
# NMNH_not_Lewis <- rbind(NMNH_not_Lewis,NMNH_subgen) # add in subgenera
CoL_not_Lewis <- CoL[which(CoL$canonicalName %!in% Lewis$canonicalName),]
# CoL_not_Lewis <- rbind(CoL_not_Lewis,CoL_subgen) # add in subgenera
GBIF_not_Lewis <- GBIF[which(GBIF$canonicalName %!in% Lewis$canonicalName),]
# GBIF_not_Lewis <- rbind(GBIF_not_Lewis,GBIF_subgen) # add in subgenera

df1 <- GBIF_not_Lewis # change dataframe name for ease of use

# add matches from other sources to GBIF not
# if canonical is in CoL_not_Lewis, add the source
for (i in 1:nrow(df1)){
  if (df1$canonicalName[i] %in% CoL_not_Lewis$canonicalName){
    df1$source[i] <- paste(df1$source[i],"CoL", sep = ", ")
  }
}

df2 <- CoL_not_Lewis[which(CoL_not_Lewis$canonicalName %!in% df1$canonicalName),] # if CoL canonical is not in working file, get it
df1 <- rbind.fill(df1, df2) # add CoL not in GBIF to working file

# if canonical is in NMNH_not_Lewis, add the source
for (i in 1:nrow(df1)){
  if (df1$canonicalName[i] %in% NMNH_not_Lewis$canonicalName){
    df1$source[i] <- paste(df1$source[i],"NMNH", sep = ", ")
  }
}

df2 <- NMNH_not_Lewis[which(NMNH_not_Lewis$canonicalName %!in% df1$canonicalName),] # if NMNH canonical is not in working file, get it
df1 <- rbind.fill(df1, df2) # add NMNH not in GBIF to working file

# if canonical is in FMNH_not_Lewis, add the source
for (i in 1:nrow(df1)){
  if (df1$canonicalName[i] %in% FMNH_not_Lewis$canonicalName){
    df1$source[i] <- paste(df1$source[i],"FMNH", sep = ", ")
  }
}

df2 <- FMNH_not_Lewis[which(FMNH_not_Lewis$canonicalName %!in% df1$canonicalName),] # if FMNH canonical is not in working file, get it
df1 <- rbind.fill(df1, df2) # add FMNH not in GBIF to working file

df1 <- rbind(df1, subgenera) # add back subgenera

write.csv(df1,"~/GitHub/tpt-siphonaptera/output/Not in Lewis.csv", row.names = FALSE) # names not in Lewis


# Lewis names in sources other than Lewis
FMNH_in_Lewis <- FMNH[which(FMNH$canonicalName %in% Lewis$canonicalName),]
NMNH_in_Lewis <- NMNH[which(NMNH$canonicalName %in% Lewis$canonicalName),]
CoL_in_Lewis <- CoL[which(CoL$canonicalName %in% Lewis$canonicalName),]
GBIF_in_Lewis <- GBIF[which(GBIF$canonicalName %in% Lewis$canonicalName),]

df <- Lewis # change dataframe name for ease of use

# if canonical is in CoL_in_Lewis, add the source
for (i in 1:nrow(df)){
  if (df$canonicalName[i] %in% CoL_in_Lewis$canonicalName){
    df$source[i] <- paste(df$source[i],"CoL", sep = ", ")
  }
}

df2 <- CoL_in_Lewis[which(CoL_in_Lewis$canonicalName %!in% df$canonicalName),] # if CoL canonical is not in working file, get it
df <- rbind.fill(df, df2) # add FMNH not in Lewis to working file

# if canonical is in FMNH_in_Lewis, add the source
for (i in 1:nrow(df)){
  if (df$canonicalName[i] %in% FMNH_in_Lewis$canonicalName){
    df$source[i] <- paste(df$source[i],"FMNH", sep = ", ")
  }
}

df2 <- FMNH_in_Lewis[which(FMNH_in_Lewis$canonicalName %!in% df$canonicalName),] # if CoL canonical is not in working file, get it
df <- rbind.fill(df, df2) # add FMNH not in Lewis to working file

# if canonical is in GBIF_in_Lewis, add the source
for (i in 1:nrow(df)){
  if (df$canonicalName[i] %in% GBIF_in_Lewis$canonicalName){
    df$source[i] <- paste(df$source[i],"GBIF", sep = ", ")
  }
}

df2 <- GBIF_in_Lewis[which(GBIF_in_Lewis$canonicalName %!in% df$canonicalName),] # if GBIF canonical is not in working file, get it
df <- rbind.fill(df, df2) # add GBIF not in Lewis to working file

# if canonical is in NMNH_in_Lewis, add the source
for (i in 1:nrow(df)){
  if (df$canonicalName[i] %in% NMNH_in_Lewis$canonicalName){
    df$source[i] <- paste(df$source[i],"NMNH", sep = ", ")
  }
}

df2 <- NMNH_in_Lewis[which(NMNH_in_Lewis$canonicalName %!in% df$canonicalName),] # if NMNH canonical is not in working file, get it
df <- rbind.fill(df, df2) # add NMNH not in Lewis to working file

write.csv(df,"~/GitHub/tpt-siphonaptera/output/in Lewis.csv", row.names = FALSE) # names in Lewis

# remove BOLD Names
# GBIF_BOLD <- df1[which(startsWith(df1$scientificName, "BOLD:")),] # get the BOLD names
# df1 <- df1[which(startsWith(df1$scientificName, "BOLD:") == FALSE),] # remove BOLD names

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
# review <- review

# add Hastriter Comments to "not_in Lewis"
Hastriter <- read_excel("~/GitHub/tpt-siphonaptera/input/not_in_Lewis 14 July 2021.xlsx", col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text","text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))
df1$Hastriter_suggested <- vlookup(Hastriter$Hastriter_suggested,df1$canonicalName,Hastriter$canonicalName)
df1$Hastriter_note <- vlookup(Hastriter$Hastriter_note,df1$canonicalName,Hastriter$canonicalName)
df1$Hastriter_family <- vlookup(Hastriter$Hastriter_family,df1$canonicalName,Hastriter$canonicalName)

for (i in 1:nrow(df1)) {
  if (left(df1$scientificName[i],":") == "BOLD:"){
    df1$Hastriter_suggested[i] <- "none"
    df1$Hastriter_note[i] <- "BOLD ID not recognized in Lewis"
  }
}# add comments for BOLD names
  
for (i in 1:nrow(df1)) {
  if (df1$taxonRank[i] == "subgenus"){
    df1$Hastriter_suggested[i] <- "none"
    df1$Hastriter_note[i] <- "subgenera not recognized in Lewis"
  }
}# add comments for subgenera

for (i in 1:nrow(df1)) {
  if (df1$taxonRank[i] == "superfamily"){
    df1$Hastriter_suggested[i] <- "none"
    df1$Hastriter_note[i] <- "superfamilies not recognized in Lewis"
  }
}# add comments for superfamilies

df1$Hastriter_suggested <- ifelse(df1$scientificName == "Tarsopsylla octodecimdentata octodecimdentata", "Tarsopsylla octodecimdentata, subspecies not recognized in Lewis", df1$Hastriter_suggested) # fix unrecognized subspecies
df1$Hastriter_suggested <- ifelse(df1$scientificName == "Macropsyllidae", "Hystrichopsyllidae, Macropsyllidae no longer recognized by Lewis", df1$Hastriter_suggested) # fix unrecognized family

test <- df1[which(is.na(df1$Hastriter_suggested)),] # check to see that all not in Lewis names have a recommendation
write.csv(df1,"~/GitHub/tpt-siphonaptera/output/not_in_Lewis.csv", row.names = FALSE) # write out names for review

# sanity check

GBIFLewis <- nrow(df[grep("GBIF", df$source),])
GBIFnotLewis <- nrow(df1[grep("GBIF", df1$source),])
ifelse(GBIFrow == (GBIFLewis + GBIFnotLewis), print("Yay"),print("Wah"))

CoLLewis <- nrow(df[grep("CoL", df$source),])
CoLnotLewis <- nrow(df1[grep("CoL", df1$source),])
ifelse(CoLrow == (CoLLewis + CoLnotLewis), print("Yay"),print("Wah"))

NMNHLewis <- df[grep("NMNH", df$source),]
NMNHnotLewis <- df1[grep("NMNH", df1$source),]
check <- NMNH_not_Lewis[which(NMNH_not_Lewis$canonicalName %!in% NMNHnotLewis$canonicalName),]
NMNH_in_Lewis$reason <- c(ifelse(duplicated(NMNH_in_Lewis$canonicalName, fromLast = TRUE)  | duplicated(NMNH_in_Lewis$canonicalName),
                                 "dupe", NA)) # Flag internal dupes
test <- df1[grep("NMNH", df1$source),]

test <- df[grep("FMNH", df$source),]
test <- df1[grep("FMNH", df1$source),]

GBIFLewis <- df[grep("GBIF", df$source),]
check <- GBIF_in_Lewis[which(GBIF_in_Lewis$canonicalName %!in% GBIFLewis$canonicalName),]
check <- GBIFLewis[which(GBIFLewis$canonicalName %!in% GBIF_in_Lewis$canonicalName),]

GBIF_in_Lewis$reason <- c(ifelse(duplicated(GBIF_in_Lewis$canonicalName, fromLast = TRUE)  | duplicated(GBIF_in_Lewis$canonicalName),
                                 "dupe", NA)) # Flag internal dupes
