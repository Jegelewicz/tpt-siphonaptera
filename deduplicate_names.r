library(readr)

# TPT_Siphonaptera_treasure_clean <- read_csv("~/GitHub/ixodes-tpt/output/TPT_Siphonaptera_treasure_clean.csv")
# df <- TPT_Siphonaptera_treasure_clean
df$dupe_dataset <- NA # create the new column for dupe data set info

BYU_Siphonaptera <- df[which(grepl('^BYU', df$TPTdataset) == TRUE), ]
FMNH_Siphonaptera <- df[which(grepl('^FMNH', df$TPTdataset) == TRUE), ]
NMNH_Siphonaptera <- df[which(grepl('^NMNH', df$TPTdataset) == TRUE), ]

# Missing data check
if(TOTAL != nrow(BYU_Siphonaptera) + 
   nrow(FMNH_Siphonaptera) +
   nrow(NMNH_Siphonaptera)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
   print('all records accounted for')
} # End of Missing data check

# Review for internal duplication
FMNH_Siphonaptera$dupe_dataset <- c(ifelse(duplicated(FMNH_Siphonaptera$canonicalName, fromLast = TRUE)  | duplicated(FMNH_Siphonaptera$canonicalName),
                                           "FMNH", NA)) # Flag internal dupes
FMNH_dupes_review <- FMNH_Siphonaptera[which(grepl('FMNH',FMNH_Siphonaptera$dupe_dataset) == TRUE), ] 
FMNH_Siphonaptera <- FMNH_Siphonaptera[which(grepl('FMNH',FMNH_Siphonaptera$dupe_dataset) == FALSE), ] 

NMNH_Siphonaptera$dupe_dataset <- c(ifelse(duplicated(NMNH_Siphonaptera$canonicalName, fromLast = TRUE)  | duplicated(NMNH_Siphonaptera$canonicalName),
                                    "NMNH", NA)) # Flag internal dupes
NMNH_dupes_review <- NMNH_Siphonaptera[which(grepl('NMNH',NMNH_Siphonaptera$dupe_dataset) == TRUE), ] 
NMNH_Siphonaptera <- NMNH_Siphonaptera[which(grepl('NMNH',NMNH_Siphonaptera$dupe_dataset) == FALSE), ] 

BYU_Siphonaptera$dupe_dataset <- c(ifelse(duplicated(BYU_Siphonaptera$canonicalName, fromLast = TRUE)  | duplicated(BYU_Siphonaptera$canonicalName),
                                    "BYU", NA)) # Flag internal dupes
BYU_dupes_review <- BYU_Siphonaptera[which(grepl('^BYU',BYU_Siphonaptera$dupe_dataset) == TRUE), ] 
BYU_Siphonaptera <- BYU_Siphonaptera[which(grepl('BYU',BYU_Siphonaptera$dupe_dataset) == FALSE), ] 

internal_dupes <- rbind(BYU_dupes_review, FMNH_dupes_review, NMNH_dupes_review)
write.csv(internal_dupes,"~/GitHub/ixodes-tpt/output/review_dupes.csv", row.names = FALSE) # these need review

# If there are no internal dupes - proceed, if any dataset has internal dupes, review and return only one to the file. place the rest in the duplicates dataframe
# If there are dupes - review and save three sets of data as below, then read in and combine
duplicates <- read_csv("~/GitHub/ixodes-tpt/input/duplicates.csv")
dups_keep <- read_csv("~/GitHub/ixodes-tpt/input/review_dupes.csv")
need_expert_review <- read_csv("~/GitHub/ixodes-tpt/output/need_expert_review.csv")

BYU_dupes_review <- dups_keep[which(grepl('^BYU',dups_keep$dupe_dataset) == TRUE), ] 
BYU_Siphonaptera <- rbind(BYU_Siphonaptera, BYU_dupes_review)

NMNH_dupes_review <- dups_keep[which(grepl('^NMNH',dups_keep$dupe_dataset) == TRUE), ] 
NMNH_Siphonaptera <- rbind(NMNH_Siphonaptera, NMNH_dupes_review)

FMNH_dupes_review <- dups_keep[which(grepl('^FMNH',dups_keep$dupe_dataset) == TRUE), ] 
FMNH_Siphonaptera <- rbind(FMNH_Siphonaptera, FMNH_dupes_review)

df <- rbind(BYU_Siphonaptera, FMNH_Siphonaptera, NMNH_Siphonaptera)

#Sanity Check
if(TOTAL != nrow(df) + 
   nrow(duplicates) +
   nrow(need_expert_review)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
   print('all records accounted for')
} # end sanity check

# flag duplicate canonical names
df$reason <- c(duplicated(df$canonicalName, fromLast = TRUE)  | duplicated(df$canonicalName)) 

# extract duplicates from less-favored sources
# FMNH
duplicate_append <- df[which(grepl('^FMNH', df$TPTdataset) == TRUE &
                         df$reason == TRUE), ] # FMNH canonical names that are duplicated in other sources
FMNH_dupes <- nrow(duplicates)
duplicates["reason"] <- NA
duplicates <- rbind(duplicates, duplicate_append)
duplicates_not <- df[which(grepl('^FMNH', df$TPTdataset) == TRUE &
                             df$reason == FALSE), ] # FMNH canonical names that are not duplicated in other sources
df <- df[which(grepl('^FMNH', df$TPTdataset) == FALSE), ] # remove all FMNH rows
df <- rbind(df, duplicates_not) # return FMNH rows that are not dupes to working file
for(i in 1:nrow(df)){
   df$dupe_dataset[i] <- ifelse(df$canonicalName[i] %in% duplicates$canonicalName,
                                "FMNH",NA)
}
df$reason <- c(duplicated(df$canonicalName, fromLast = TRUE)  | duplicated(df$canonicalName)) # reflag duplicate canonical names

# Missing data check FMNH
if(TOTAL != nrow(df) + 
   nrow(duplicates) +
   nrow(need_expert_review)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check FMNH

# NMNH
duplicate_append <- df[which(grepl('^NMNH', df$TPTdataset) == TRUE &
                               df$reason == TRUE), ] # NMNH canonical names that are duplicated in other sources
NMNH_dupes <- nrow(duplicate_append)
duplicates <- rbind(duplicates, duplicate_append)
duplicates_not <- df[which(grepl('^NMNH', df$TPTdataset) == TRUE &
                                    df$reason == FALSE), ] # NMNH canonical names that are not duplicated in other sources
df <- df[which(grepl('^NMNH', df$TPTdataset) == FALSE), ] # remove all NMNH rows
df <- rbind(df, duplicates_not) # return NMNH rows that are not dupes to working file
for(i in 1:nrow(df)){
   df$dupe_dataset[i] <- ifelse(df$canonicalName[i] %in% duplicate_append$canonicalName,
                                ifelse(!is.na(df$dupe_dataset[i]),
                                       paste(df$dupe_dataset[i],'NMNH',sep=', '),'NMNH'),NA)
}
df$reason <- c(duplicated(df$canonicalName, fromLast = TRUE)  | duplicated(df$canonicalName)) # reflag duplicate canonical names

# Missing data check NMNH
if(TOTAL != nrow(df) + 
   nrow(duplicates) +
   nrow(need_expert_review)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check NMNH

# # GBIF
# duplicate_append <- df[which(grepl('^BYU', df$TPTdataset) == TRUE &
#                                 df$reason == TRUE), ] # BYU canonical names that are duplicated in other sources
# BYU_dupes <- nrow(duplicate_append)
# duplicates <- rbind(duplicates, duplicate_append)
# duplicates_not <- df[which(grepl('^BYU', df$TPTdataset) == TRUE &
#                               df$reason == FALSE), ] # BYU canonical names that are not duplicated in other sources
# df <- df[which(grepl('^BYU', df$TPTdataset) == FALSE), ] # remove all BYU rows
# df <- rbind(df, duplicates_not) # return BYU rows that are not dupes to working file
# for(i in 1:nrow(df)){
#    df$dupe_dataset[i] <- ifelse(df$canonicalName[i] %in% duplicate_append$canonicalName,
#                                 ifelse(!is.na(df$dupe_dataset[i]),
#                                        paste(df$dupe_dataset[i],'BYU',sep=', '),'BYU'), NA)
# }
# df$reason <- c(duplicated(df$canonicalName, fromLast = TRUE)  | duplicated(df$canonicalName)) # reflag duplicate canonical names

# # Missing data check NMNH
# if(TOTAL != nrow(df) + 
#    nrow(duplicates) +
#    nrow(df_removed)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
# } else {
#    print('all records accounted for')
# } # End of Missing data check NMNH

# BYU
duplicate_append <- df[which(duplicated(df$canonicalName)),] # GBIF canonical names that are duplicated in other sources
BYU_dupes <- nrow(duplicate_append)
duplicates <- rbind(duplicates, duplicate_append)
df <- df[which(!duplicated(df$canonicalName)),] # deduplicated list
for(i in 1:nrow(df)){
   df$dupe_dataset[i] <- ifelse(df$canonicalName[i] %in% duplicate_append$canonicalName,
                                ifelse(!is.na(df$dupe_dataset[i]),
                                       paste(df$dupe_dataset[i],'BYU',sep=', '),'BYU'),NA)
}

# Missing data check Final
if(TOTAL != nrow(df) + 
   nrow(duplicates) +
   nrow(need_expert_review)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check Final


# add duplicates and uniques to source summary dataframe
BYU <- nrow(df[grep("BYU*", df$TPTdataset), ])
FMNH <- nrow(df[grep("FMNH*", df$TPTdataset), ]) # number of deduplicated records in FMNH Source
# GBIF <- nrow(df[grep("GBIF*", df$TPTdataset), ]) # number of deduplicated records in GBIF Source
NMNH <- nrow(df[grep("NMNH*", df$TPTdataset), ]) # number of deduplicated records in NMNH Source
TOTAL <- BYU + FMNH + NMNH # number of deduplicated records in all sources
TOTAL_Dupes <- BYU_dupes + FMNH_dupes + NMNH_dupes # number of duplicated records in all sources
source_summary$removed_names <- c(BYU_dupes, FMNH_dupes, NMNH_dupes, TOTAL_Dupes) # create column in sources data frame for number of duplicate names

source_summary$unique_names <- c(BYU, FMNH, NMNH, TOTAL)# create column in sources data frame for number of unique names


write.csv(df,"~/GitHub/ixodes-tpt/output/TPT_Siphonaptera.csv", row.names = FALSE) # write out final list
write.csv(duplicates,"~/GitHub/ixodes-tpt/output/TPT_Siphonaptera_duplicates.csv", row.names = FALSE) # write out file of duplicates removed
write.csv(need_expert_review,"~/GitHub/ixodes-tpt/output/need_expert_review.csv", row.names = FALSE) # write out file of removed for review
write.csv(source_summary,"~/GitHub/ixodes-tpt/output/Siphonaptera_sources.csv", row.names = FALSE) # write out reconciliation

print("proceed to levenshtein distance name matching")
