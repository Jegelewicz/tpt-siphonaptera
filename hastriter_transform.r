# read in new format file
Hastriter <- read_excel("~/GitHub/tpt-siphonaptera/input/Hastriter ADDED GENERA SORT-1.xlsx")
df <- Hastriter # change filename for ease of use

# clean up text
df <- char_fun(df,phrase_clean) # remove all \xa0 characters
df <- char_fun(df,trimws) # trim white space
df <- char_fun(df,space_clean) # replace double spaces with single space

# Find new names
df_new <- df[which(is.na(df$taxonID)),] # get rows without a taxonID
df_old <- df[which(!is.na(df$taxonID)),] # remove rows without a taxonID from working file
df <- df_new # change dataframe name for ease of operation
original <- nrow(df_old) # get last original TaxonID
add <- nrow(df) # get number of new names

# add taxonID for new names
df$taxonID <- seq.int(nrow(df)) # add numeric ID for each name
df$taxonID <- df$taxonID + original # add number of original rows to new ids

# add canonicalName
# extract higher taxa for next set of review
higher_taxa <- higher_taxa_epithet(df,df$specificEpithet,df$infraspecificEpithet) # create dataframe of higher taxa
df <- species_epithet(df,df$infraspecificEpithet, df$specificEpithet) # remove higher taxa from working file

# generate canonical name for species and below
df <- cast_canonical(df,
                     canonical="canonicalName", 
                     genus = "genus", 
                     species = "specificEpithet",
                     subspecies = "infraspecificEpithet")

# canonical names for taxa ranked subgenus and above - get the lowest ranking term and put it here!
for(i in 1:nrow(higher_taxa)){
  higher_taxa$canonicalName[i] <- ifelse(!is.na(higher_taxa$subgenus[i]), paste(higher_taxa$subgenus[i]),
                                         ifelse(!is.na(higher_taxa$genus[i]), paste(higher_taxa$genus[i]),
                                                ifelse(!is.na(higher_taxa$family[i]), paste(higher_taxa$family[i]),
                                                       ifelse(!is.na(higher_taxa$subfamily[i]), paste(higher_taxa$subfamily[i]),
                                                              "review"))))
}

# add higher classification
unique_names <- df_old[which(df_old$taxonRank == "species"),] # get only species
unique_names <- unique_names[which(!duplicated(unique_names$genus)),] # unique genera
for (i in 1:nrow(df)){
  df$family[i] <- vlookup(unique_names$family,df$genus[i],unique_names$genus) # get family names
  df$subfamily[i] <- vlookup(unique_names$subfamily,df$genus[i],unique_names$genus) # get subfamily names
}

df <- rbind(df, higher_taxa) # combine new higher and lower taxa

# generate scientificName for new names
for(i in 1:nrow(df)){
  df$scientificName[i] <- paste(df$canonicalName[i],df$scientificNameAuthorship[i],sep = " ")
}

df$kingdom <- "Animalia" # add kingdom
df$phylum <- "Arthropoda" # add phylum
df$class <- "Insecta" # add class
df$order <- "Siphonaptera" # add order

# Fill in DwC fields
df$nomenclaturalCode <- "ICZN" # set nomenclatural code
df$namePublishedInYear <- right(df$scientificNameAuthorship,",") # get year from author text
df$namePublishedInYear <- gsub("[^0-9]", "",df$namePublishedInYear) # leave only numbers


# df <- rbind(df, df_new) # add back rows with no author
