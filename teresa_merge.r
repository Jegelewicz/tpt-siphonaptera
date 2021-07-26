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

# Names in sources other than Lewis
FMNH_not_Lewis <- FMNH[which(FMNH$canonicalName %!in% Lewis$canonicalName),]
NMNH_not_Lewis <- NMNH[which(NMNH$canonicalName %!in% Lewis$canonicalName),]
CoL_not_Lewis <- CoL[which(CoL$canonicalName %!in% Lewis$canonicalName),]
GBIF_not_Lewis <- GBIF[which(GBIF$canonicalName %!in% Lewis$canonicalName),]
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

# sanity check

LewisGBIF <- df[grep("GBIF", df$source),] # Get rows in final that include "GBIF" in source
LewisCoL <- df[grep("CoL", df$source),] # Get rows in final that include "CoL" in source
LewisNMNH <- df[grep("NMNH", df$source),] # Get rows in final that include "NMNH" in source
LewisFMNH <- df[grep("FMNH", df$source),] # Get rows in final that include "FNMH" in source
notLewisGBIF <- df1[grep("GBIF", df1$source),] # get number of rows in "not in Lewis" final that include GBIF in source
notLewisCoL <- df1[grep("CoL", df1$source),] # get number of rows in "not in Lewis" final that include CoL in source
notLewisNMNH <- df1[grep("NMNH", df1$source),] # get number of rows in "not in Lewis" final that include NMNH in source
notLewisFMNH <- df1[grep("FMNH", df1$source),] # get number of rows in "not in Lewis" final that include FMNH in source

# Get number of duplicates in in_Lewis files
GBIF_in_Lewis_dupes <- GBIF_in_Lewis[which(duplicated(GBIF_in_Lewis$canonicalName, fromLast = TRUE)  | duplicated(GBIF_in_Lewis$canonicalName)),] # get dupes
GBIFinLewisdupecount <- length(unique(GBIF_in_Lewis_dupes[["canonicalName"]])) # count number of unique names in dupes
CoL_in_Lewis_dupes <- CoL_in_Lewis[which(duplicated(CoL_in_Lewis$canonicalName, fromLast = TRUE)  | duplicated(CoL_in_Lewis$canonicalName)),] # get dupes
CoLinLewisdupecount <- length(unique(CoL_in_Lewis_dupes[["canonicalName"]])) # count number of unique names in dupes
NMNH_in_Lewis_dupes <- NMNH_in_Lewis[which(duplicated(NMNH_in_Lewis$canonicalName, fromLast = TRUE)  | duplicated(NMNH_in_Lewis$canonicalName)),] # get dupes
NMNHinLewisdupecount <- length(unique(NMNH_in_Lewis_dupes[["canonicalName"]])) # count number of unique names in dupes
FMNH_in_Lewis_dupes <- FMNH_in_Lewis[which(duplicated(FMNH_in_Lewis$canonicalName, fromLast = TRUE)  | duplicated(FMNH_in_Lewis$canonicalName)),] # get dupes
FMNHinLewisdupecount <- length(unique(FMNH_in_Lewis_dupes[["canonicalName"]])) # count number of unique names in dupes

# Get number of duplicates in Lewis files
LewisGBIF_dupes <- LewisGBIF[which(duplicated(LewisGBIF$canonicalName, fromLast = TRUE)  | duplicated(LewisGBIF$canonicalName)),] # get dupes
LewisGBIFdupecount <- length(unique(LewisGBIF_dupes[["canonicalName"]])) # count number of unique names in dupes
LewisCoL_dupes <- LewisCoL[which(duplicated(LewisCoL$canonicalName, fromLast = TRUE)  | duplicated(LewisCoL$canonicalName)),] # get dupes
LewisCoLdupecount <- length(unique(LewisCoL_dupes[["canonicalName"]])) # count number of unique names in dupes
LewisNMNH_dupes <- LewisNMNH[which(duplicated(LewisNMNH$canonicalName, fromLast = TRUE)  | duplicated(LewisNMNH$canonicalName)),] # get dupes
LewisNMNHdupecount <- length(unique(LewisNMNH_dupes[["canonicalName"]])) # count number of unique names in dupes
LewisFMNH_dupes <- LewisFMNH[which(duplicated(LewisFMNH$canonicalName, fromLast = TRUE)  | duplicated(LewisFMNH$canonicalName)),] # get dupes
LewisFMNHdupecount <- length(unique(LewisFMNH_dupes[["canonicalName"]])) # count number of unique names in dupes
 
# Get number of duplicates in not_Lewis files
GBIF_not_Lewis_dupes <- GBIF_not_Lewis[which(duplicated(GBIF_not_Lewis$canonicalName, fromLast = TRUE)  | duplicated(GBIF_not_Lewis$canonicalName)),] # get dupes
GBIFnotinLewisdupecount <- length(unique(GBIF_not_Lewis_dupes[["canonicalName"]])) # count number of unique names in dupes
CoL_not_Lewis_dupes <- CoL_not_Lewis[which(duplicated(CoL_not_Lewis$canonicalName, fromLast = TRUE)  | duplicated(CoL_not_Lewis$canonicalName)),] # get dupes
CoLnotinLewisdupecount <- length(unique(CoL_not_Lewis_dupes[["canonicalName"]])) # count number of unique names in dupes
NMNH_not_Lewis_dupes <- NMNH_not_Lewis[which(duplicated(NMNH_not_Lewis$canonicalName, fromLast = TRUE)  | duplicated(NMNH_not_Lewis$canonicalName)),] # get dupes
NMNHnotinLewisdupecount <- length(unique(NMNH_not_Lewis_dupes[["canonicalName"]])) # count number of unique names in dupes
FMNH_not_Lewis_dupes <- FMNH_not_Lewis[which(duplicated(FMNH_not_Lewis$canonicalName, fromLast = TRUE)  | duplicated(FMNH_not_Lewis$canonicalName)),] # get dupes
FMNHnotinLewisdupecount <- length(unique(FMNH_not_Lewis_dupes[["canonicalName"]])) # count number of unique names in dupes

# Get number of duplicates in notLewis files
notLewisGBIF_dupes <- notLewisGBIF[which(duplicated(notLewisGBIF$canonicalName, fromLast = TRUE)  | duplicated(notLewisGBIF$canonicalName)),] # get dupes
notLewisGBIFdupecount <- length(unique(notLewisGBIF_dupes[["canonicalName"]])) # count number of unique names in dupes
notLewisCoL_dupes <- notLewisCoL[which(duplicated(notLewisCoL$canonicalName, fromLast = TRUE)  | duplicated(notLewisCoL$canonicalName)),] # get dupes
notLewisCoLdupecount <- length(unique(notLewisCoL_dupes[["canonicalName"]])) # count number of unique names in dupes
notLewisNMNH_dupes <- LewisNMNH[which(duplicated(notLewisNMNH$canonicalName, fromLast = TRUE)  | duplicated(notLewisNMNH$canonicalName)),] # get dupes
notLewisNMNHdupecount <- length(unique(notLewisNMNH_dupes[["canonicalName"]])) # count number of unique names in dupes
notLewisFMNH_dupes <- LewisFMNH[which(duplicated(notLewisFMNH$canonicalName, fromLast = TRUE)  | duplicated(notLewisFMNH$canonicalName)),] # get dupes
notLewisFMNHdupecount <- length(unique(notLewisFMNH_dupes[["canonicalName"]])) # count number of unique names in dupes

#sanity check - all should be zero
GBIF_check <- nrow(LewisGBIF) + nrow(notLewisGBIF) - LewisGBIFdupecount - notLewisGBIFdupecount - GBIFrow + GBIFinLewisdupecount + GBIFnotinLewisdupecount
CoL_check <- nrow(LewisCoL) + nrow(notLewisCoL) - LewisCoLdupecount - notLewisCoLdupecount - CoLrow + CoLinLewisdupecount + CoLnotinLewisdupecount
NMNH_check <- nrow(LewisNMNH) + nrow(notLewisNMNH) - LewisNMNHdupecount - notLewisNMNHdupecount - NMNHrow + NMNHinLewisdupecount + NMNHnotinLewisdupecount
FMNH_check <- nrow(LewisFMNH) + nrow(notLewisFMNH) - LewisFMNHdupecount - notLewisFMNHdupecount - FMNHrow + FMNHinLewisdupecount + FMNHnotinLewisdupecount


