# import libraries
library(taxotools)

# read in cleaned review file
df_review <- read.csv("~/GitHub/ixodes-tpt/input/taxa_need_review.csv", encoding="UTF-8")
df <- rbind(df, df_review)

# read in names removed during review
df_removed <- read.csv("~/GitHub/ixodes-tpt/input/taxa_removed_review.csv", encoding="UTF-8")

# Missing data check 1
if(TOTAL != (nrow(df) +
             nrow(df_removed))){
  print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for, proceed to canonical name script')
} # End of Missing data check 1

# extract rows with canonicalName
canonical <- df[which(lapply(df$canonicalName, name_length) != 0),]
df <- df[which(lapply(df$canonicalName, name_length) == 0),]

# extract higher taxa for next set of review
higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),]

# generate canonical name for genus and below
df <- cast_canonical(df,
                     canonical="canonicalName", 
                     genus = "genus", 
                     species = "specificEpithet",
                     subspecies = "infraspecificEpithet")

# canonical names for taxa ranked subgenus and above - get the lowest ranking term and put it here!
for(i in 1:nrow(higher_taxa)){
  higher_taxa$canonicalName[i] <- ifelse(!is.na(higher_taxa$subgenus[i]), paste(higher_taxa$subgenus[i]),
                                     ifelse(!is.na(higher_taxa$genus[i]), paste(higher_taxa$genus[i]),
                                            ifelse(!is.na(higher_taxa$subfamily[i]), paste(higher_taxa$subfamily[i]),
                                                   ifelse(!is.na(higher_taxa$family[i]), paste(higher_taxa$family[i]),
                                                          ifelse(!is.na(higher_taxa$superfamily[i]), paste(higher_taxa$superfamily[i]),
                                                                 ifelse(!is.na(higher_taxa$infraorder[i]), paste(higher_taxa$infraorder[i]),
                                                                        ifelse(!is.na(higher_taxa$order[i]), paste(higher_taxa$order[i]),
                                                                               ifelse(!is.na(higher_taxa$class[i]), paste(higher_taxa$class[i]),
                                                                                      ifelse(!is.na(higher_taxa$phylum[i]), paste(higher_taxa$phylum[i]),
                                                                                             ifelse(!is.na(higher_taxa$kingdom[i]), paste(higher_taxa$kingdom[i]), "review"))))))))))
}

# Extract rows from higher taxa that need review
flag <- c('review')
review_canonical <- higher_taxa[(higher_taxa$canonical %in% flag), ]
write.csv(review_canonical,"~/GitHub/ixodes-tpt/output/review_canonical.csv", row.names = FALSE) # these need review
higher_taxa <- higher_taxa[(higher_taxa$canonical %!in% flag), ] # extract review items from higher_taxa

if(nrow(review_canonical) == 0){
  print('No canonical names in higher_taxa have been flagged for review. Proceed to deduplication.')
  df <- rbind(higher_taxa, df) # add higher taxa back to df for remainder of de-duplication
} else{
  stop('Open the review_canonical file in the output folder, make adjustments as appropriate and save the revised file to input with the same name before proceeding')
}

review_canonical <- read.csv("~/GitHub/ixodes-tpt/input/review_canonical.csv", encoding="UTF-8") # read in cleaned review file
higher_taxa <- rbind(higher_taxa, review_canonical) # add reviewed higher_taxa back to the working file
df <- rbind(higher_taxa, df) # add higher taxa back to df for remainder of de-duplication

# Missing data check 2
if(TOTAL != nrow(df)){
  print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for, proceed to name deduplication name script')
} # End of Missing data check 2
