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

all_names <- rbind(df,df1)

write.csv(all_names,"~/GitHub/tpt-siphonaptera/output/merged_names.csv", row.names = FALSE) # all names
