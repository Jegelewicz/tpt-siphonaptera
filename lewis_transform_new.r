# read in file
Lewis <- read_excel("~/GitHub/tpt-siphonaptera/input/Lewis World Species List 13 JULY 2021.xlsx", col_types = c("text", "text", "text", "text", "text", "text", "text", "text"))
Lewis_genera <- read_excel("~/GitHub/tpt-siphonaptera/input/Lewis World Genera List 13 JULY 2021.xlsx", col_types = c("text", "text", "text", "text", "text", "text"))
df <- rbind.fill(Lewis, Lewis_genera) # combine species and genus files
Lewis_original_rows <- nrow(df) # get initial number of rows
tpt_dwc_template <- read_excel("input/tpt_dwc_template.xlsx") # read in TPT DarwinCore template
tpt_dwc_template[] <- lapply(tpt_dwc_template, as.character) # set all columns in template to character

# transform column headers
colnames(df) <- tolower(colnames(df)) # lower case column names
colnames(df) <- convert2DwC(colnames(df)) # convert to DarwinCore terms

# text cleaning
df <- char_fun(df,phrase_clean) # remove all \xa0 characters
df <- char_fun(df,trimws) # trim white space
df <- char_fun(df,space_clean) # replace double spaces with single space

df <- rbindlist(list(df, tpt_dwc_template), fill = TRUE) # add all DwC columns

df$taxonID <- seq.int(nrow(df)) # add numeric ID for each name

is.na(df$subfamily) <- df$subfamily == "NONE" # remove "NONE" from subfamily
df$family <- toproper(df$family) # ensure all family names are proper case

# deal with "generic account" column
# for (i in 1:nrow(df)){
#   if (is.na(df$genus[i])){
#     df$genus[i] <- df$`generic account`[i]
#     df$`generic account`[i] <- NA
#   } else {
#     df$taxonRemarks[i] <- paste("generic account",df$`generic account`[i], sep = " ")
#     df$`generic account`[i] <- NA
#   }
# }

# deal with "fossil" column
df$taxonRemarks <- tolower(df$fossil)

# deal with parenthesis in species
# df$taxonRemarks <- inparens(df$species)# for species column put things in parentheses in taxonRemark
# df$taxonRemarks[ df$taxonRemarks == "" ] <- NA # set all blank taxonRemark to NA
# df$species <- outparens(df$species)# get things outside parenthesis for species

# split specificEpithet when it has two terms
multi_epithet <- df[which(lapply(df$species, name_length) > 1),] # extract rows with a multi-name species
df <- df[which(lapply(df$species, name_length) <= 1),] # extract rows with a multi-name species

for(i in 1:nrow(multi_epithet)){
  multi_epithet$specificEpithet[i] <- left(multi_epithet$species[i], " ") # place first term in specificEpithet
  multi_epithet$infraspecificEpithet[i] <- right(multi_epithet$species[i], " ") # place second term in infraspecificEpithet
}

multi_epithet <- char_fun(multi_epithet,trimws) # strip spaces from ends of strings
multi_epithet <- char_fun(multi_epithet,space_clean) # strip double spaces

df$specificEpithet <- df$species # place single term species names in specificEpithet

df <- rbind(df, multi_epithet) # return subspecies to working file

# create scientificNameAuthorship which meets DarwinCore standard for ICZN
for(i in 1:nrow(df)){
  df$scientificNameAuthorship[i] <- ifelse(is.na(df$namePublishedInYear[i]) &
                                             is.na(df$author[i]), NA, # if both author and year are blank, insert NA
                                           ifelse(is.na(df$namePublishedInYear[i]),df$author[i], # if author is not blank but year is, insert author
                                                  ifelse(is.na(df$author[i]), df$namePublishedInYear[i], # if author is blank, but year is not, insert year
                                                         paste(df$author[i], df$namePublishedInYear[i], sep = ', ')) # if both author and year are NOT blank merge and insert
                                           )
  )
}

df$scientificNameAuthorship <- fixAuth(df$scientificNameAuthorship) # apply fix for author parenthesis

# add canonical name and taxon rank
df$canonicalName <- NA # create column for canonicalName

# extract higher taxa for next set of review
higher_taxa <- higher_taxa_epithet(df,df$specificEpithet,df$infraspecificEpithet) # create dataframe of higher taxa
df <- species_epithet(df,df$infraspecificEpithet, df$specificEpithet) # remove higher taxa from working file

# generate canonical name for species and below
df <- cast_canonical(df,
                     canonical="canonicalName", 
                     genus = "genus", 
                     species = "specificEpithet",
                     subspecies = "infraspecificEpithet")

# generate taxonRank for species and below
for(i in 1:nrow(df)){
  df$taxonRank[i] <- 
    ifelse(!is.na(df$infraspecificEpithet[i]), "subspecies",
           ifelse(!is.na(df$specificEpithet[i]), "species",
                  "review"))
}

# cast scientific name for species and below
df$scientificName[i] <- for(i in 1:nrow(df)){
  if(!is.na(df$genus[i])){
    scn <- df$genus[i]
  }
  if(!is.na(df$subgenus[i])){
    scn <- paste(scn," (",df$subgenus[i],")",sep = "")
  }
  if(!is.na(df$specificEpithet[i])){
    scn <- paste(scn,df$specificEpithet[i], sep = " ")
  }
  if(!is.na(df$infraspecificEpithet[i])){
    scn <- paste(scn,df$infraspecificEpithet[i], sep = " ")
  }
  if(!is.na(df$scientificNameAuthorship[i])){
    scn <- paste(scn,trimws(df$scientificNameAuthorship[i]), sep = " ")
  }
  df$scientificName[i] <- scn
}

# canonical names for taxa ranked subgenus and above - get the lowest ranking term and put it here!
for(i in 1:nrow(higher_taxa)){
  higher_taxa$canonicalName[i] <- ifelse(!is.na(higher_taxa$subgenus[i]), paste(higher_taxa$subgenus[i]),
                                         ifelse(!is.na(higher_taxa$genus[i]), paste(higher_taxa$genus[i]),
                                                ifelse(!is.na(higher_taxa$family[i]), paste(higher_taxa$family[i]),
                                                       ifelse(!is.na(higher_taxa$subfamily[i]), paste(higher_taxa$subfamily[i]),
                                                              "review"))))
}

# generate taxonRank for genus and above
for(i in 1:nrow(higher_taxa)){
  higher_taxa$taxonRank[i] <- 
    ifelse(!is.na(higher_taxa$subgenus[i]), "subgenus",
           ifelse(!is.na(higher_taxa$genus[i]), "genus",
                  ifelse(!is.na(higher_taxa$family[i]), "family",
                         ifelse(!is.na(higher_taxa$subfamily[i]), "subfamily",
                                "review"))))
}

# cast scientific name for genus and above
higher_taxa$scientificName <- ifelse(is.na(higher_taxa$scientificNameAuthorship), higher_taxa$canonicalName, paste(higher_taxa$canonicalName, higher_taxa$scientificNameAuthorship, sep = " "))

df <- rbind(higher_taxa, df) # add higher taxa back to df for remainder of de-duplication

df <- char_fun(df,trimws) # trim white space
df <- char_fun(df,space_clean) # replace double spaces with single space

# synonyms
synonyms <- df[which(!is.na(df$`synonym(s)`)),] # get all the synonyms
df <- df[which(is.na(df$`synonym(s)`)),] # remove synonyms from working file

# transform synonyms
# synonyms$taxonID <- seq.int(Lewis_rows + 1, Lewis_rows + nrow(synonyms)) # add numeric ID for each synonym name
synonyms$acceptedNameUsage <- synonyms$canonicalName # create accepted name
synonyms$scientificName <- synonyms$`synonym(s)` # move synonym to scientificName
synonyms$infraspecificEpithet <- NA # clear subspecifc names of accepted name classification
synonyms$specificEpithet <- NA # clear specific names of accepted name classification
synonyms$canonicalName <- NA # clear canonical names of accepted name
synonyms$namePublishedInYear <- NA # clear publication year of accepted name
synonyms$genus <- NA # clear genus of accepted name
synonyms$scientificNameAuthorship <- NA # clear authorship of accepted name

# close unclosed parenthesis
# parens <- synonyms[which(grepl("\\(",synonyms$scientificName) == TRUE),] # get names with open parens
# no_parens <- parens[which(grepl(")",parens$scientificName) == FALSE),] # get names with unclosed parens
# synonyms <- synonyms[which(synonyms$taxonID %!in% no_parens$taxonID),] # remove missing parens rows
# no_parens$scientificName <- paste(no_parens$scientificName,")",sep = "") #add closing synonym
# synonyms <- rbind(synonyms,no_parens) # add fixes back to synonyms

Lewis_synonym_rows <- nrow(synonyms) # number of synonyms

# deal with parenthesis in synonym scientificName
synonyms$taxonRemarks <- inparens(synonyms$scientificName) # put things in parentheses in taxonRemark
synonyms$taxonRemarks <- ifelse(synonyms$taxonRemarks == "", NA, synonyms$taxonRemarks) # set all blank taxonRemark to NA
synonyms$scientificName <- outparens(synonyms$scientificName) # get things outside parenthesis for scientificName

synonyms$taxonomicStatus <- "synonym" # add taxonomicStatus of "synonym" to all synonym names
df$taxonomicStatus <- "accepted" # add taxonomicStatus of "accepted" to all non-synonym names

# deal with synonyms without accepted name
# review <- synonyms[which(synonyms$acceptedNameUsage == "review"),] # get review names
# synonyms <- synonyms[which(synonyms$acceptedNameUsage != "review"),] # remove review names from synonyms
# review$acceptedNameUsage <- NA
# review$taxonRank <- "species"
# review$namePublishedInYear <- right(review$scientificName,",") # get year from author text
# review$namePublishedInYear <- gsub("[^0-9]", "",review$namePublishedInYear) # leave only numbers
# synonyms <- rbind(synonyms,review)

# melt scientific name of synonyms
synonyms <- melt_scientificname(synonyms, 
                                sciname="scientificName", 
                                genus="genus",
                                subgenus="subgenus", 
                                species="specificEpithet", 
                                subspecies="infraspecificEpithet",
                                author="scientificNameAuthorship")

synonyms$scientificNameAuthorship <- lapply(synonyms$scientificNameAuthorship, trimws) # trim space left at beginning of scientifcNameAuthorship by function
synonyms$scientificNameAuthorship <- vapply(synonyms$scientificNameAuthorship, paste, collapse = ", ", character(1L)) # set scientific authorship to character

# cast canonicalName for synonyms
synonyms <- cast_canonical(synonyms,
                           canonical="canonicalName", 
                           genus = "genus", 
                           species = "specificEpithet",
                           subspecies = "infraspecificEpithet")

# generate taxonRank for synonyms
for(i in 1:nrow(synonyms)){
  synonyms$taxonRank[i] <- 
    ifelse(!is.na(synonyms$infraspecificEpithet[i]), "subspecies",
           ifelse(!is.na(synonyms$specificEpithet[i]), "species",
                  "genus"))
}

synonyms <- char_fun(synonyms,trimws) # strip spaces from ends of strings
synonyms <- char_fun(synonyms,space_clean) # strip double spaces
synonyms <- char_fun(synonyms,phrase_clean) # remove all \xa0 characters

# fix authorship in parens for synonyms
synonyms[synonyms == "" | synonyms == " "] <- NA
for (i in 1:nrow(synonyms)){
  if (is.na(synonyms$scientificNameAuthorship[i]) == TRUE){
    synonyms$scientificNameAuthorship[i] <- paste("(",synonyms$taxonRemarks[i],")",sep = "")
    synonyms$taxonRemarks[i] <- NA
  }
}

# add published year
synonyms$namePublishedInYear <- right(synonyms$scientificNameAuthorship,",") # get year from author text
synonyms$namePublishedInYear <- gsub("[^0-9]", "",synonyms$namePublishedInYear) # leave only numbers

# remove synonyms that need review and make corrections
# review_synonyms <- synonyms[which(synonyms$taxonRank == "review"),] # extract synonyms with taxonRemarks
# synonyms <- synonyms[which(synonyms$taxonRank != "review"),] # remove review names
# review_synonyms$taxonRank <- "genus"
# synonyms <- rbind(synonyms,review_synonyms) # add reviewed back to synonyms

# get accepted name ID
for (i in 1:nrow(synonyms)){
  synonyms$acceptedNameUsageID[i] <- vlookup(df$taxonID,synonyms$acceptedNameUsage[i],df$canonicalName) # get accceptedID
}

# look for missing accepted names
accepted <- synonyms[which(!duplicated(synonyms$acceptedNameUsage)),]
look <- synonyms[which(synonyms$acceptedNameUsage %!in% df$canonicalName),]

df <- rbind(df,synonyms) # add synonyms back to working file

df <- char_fun(df,trimws) # strip spaces from ends of strings
df <- char_fun(df,space_clean) # strip double spaces
df <- char_fun(df,phrase_clean) # remove all \xa0 characters

df$kingdom <- "Animalia" # add kingdom
df$phylum <- "Arthropoda" # add phylum
df$class <- "Insecta" # add class
df$order <- "Siphonaptera" # add order
df$source <- "Lewis"

df$specificEpithet <- ifelse(df$taxonRank == "genus", NA, df$specificEpithet) # ensure genera don't have specific epithet
df$infraspecificEpithet <- ifelse(df$taxonRank == "genus", NA, df$infraspecificEpithet) # ensure genera don't have infraspecific epithet
df$scientificNameAuthorship <- ifelse(df$scientificNameAuthorship == "", NA, df$scientificNameAuthorship) # replace blank with NA

# clean up taxon remarks
df$taxonRemarks <- gsub("Female","female",df$taxonRemarks)
df$taxonRemarks <- gsub("Male","male",df$taxonRemarks)
df$taxonRemarks <- gsub("in:","in",df$taxonRemarks)

# update appropriate fields for subgenera remark
for (i in 1:nrow(df)){
  if (grepl("subgenus",df$taxonRemarks[i]) == TRUE){ # if remarks contain subgenus
    df$taxonRank[i] <- "subgenus" # make taxon rank subgenus
    df$subgenus[i] <- df$canonicalName[i] # make subgenus equal to canonical
    df$genus[i] <- df$acceptedNameUsage[i] # make genus accepted name
  }
}

# Fill in DwC fields
df$nomenclaturalCode <- "ICZN" # set nomenclatural code
for(i in 1:nrow(df)){
  if (df$taxonRank[i] == "subspecies"){
    df$parentNameUsage[i] <- paste(df$genus[i],df$specificEpithet[i],sep = " ")
  }else {
    if (df$taxonRank[i] == "species"){
      df$parentNameUsage[i] <- df$genus[i]
    } else {
      if (df$taxonRank[i] == "genus"){
        ifelse(is.na(df$subfamily[i]),
               df$parentNameUsage[i] <- df$family[i],
               df$parentNameUsage[i] <- df$subfamily[i])
        
        }
      }
    }
}

# look for missing genera
df <- char_fun(df,trimws) # strip spaces from ends of strings
df <- char_fun(df,space_clean) # strip double spaces
df <- char_fun(df,phrase_clean) # remove all \xa0 characters

generic_names <- df[which(!duplicated(df$genus)),] # deduplicated list by genus
genera <- df[which(df$taxonRank == "genus"),] # all generic names in the list
missing_genera <- generic_names[which(generic_names$genus %!in% genera$genus),] # get names used as genus but not in list
missing_generic <- genera[which(genera$genus %!in% generic_names$genus),] # get names used as genus but not in list
write.csv(missing_genera,"~/GitHub/tpt-siphonaptera/output/Lewis_review_genera.csv", row.names = FALSE)

# get parent name ID - doesn't work because some parents aren't in the file
# lookup <- df # create lookup dataframe
# for (i in 1:nrow(df)){
#   df$parentNameUsageID[i] <- vlookup(lookup$taxonID,df$parentNameUsage[i],lookup$canonicalName) # get parentNameID
# }

# check for problems
df$reason <- c(ifelse(duplicated(df$canonicalName, fromLast = TRUE)  | duplicated(df$canonicalName),
                                           "dupe", NA)) # Flag internal dupes
dupes <- df[which(grepl('dupe',df$reason) == TRUE), ] 
bad_year <- df[which(df$namePublishedInYear < 1700 | is.na(df$namePublishedInYear) | df$namePublishedInYear > 2100),]
bad_year$reason <- "bad year"
missing_author <- df[which(is.na(df$scientificNameAuthorship)),]
missing_author$reason <- "no author"
multi_genera <- df[which(lapply(df$genus, name_length) > 1),] # extract rows with a multi-name genus
multi_specific <- df[which(lapply(df$specificEpithet, name_length) > 1),] # extract rows with a multi-name species
multi_infra <- df[which(lapply(df$infraspecificEpithet, name_length) > 1),] # extract rows with a multi-name species
review <- rbind(dupes,bad_year,missing_author,multi_genera,multi_specific,multi_infra)
write.csv(review,"~/GitHub/tpt-siphonaptera/output/Lewis_review.csv", row.names = FALSE) # write out duplicate names for review

# df <- df[which(df$taxonID %!in% review$taxonID)] # no review items

# Do this after final review...
Lewis_non_dwc <- subset(df, select = c(source, taxonID, species, author, `synonym(s)`, fossil, reason)) # get all columns that are not DwC
# remove non DwC columns from working file
df$species <- NULL
df$author <- NULL
df$`synonym(s)` <- NULL
df$fossil <- NULL
df$reason <- NULL

# order column names
# df[,c(1,2,3,4)]. Note the first comma means keep all the rows, and the 1,2,3,4 refers to the columns.
df <- df[,c("source",
            "taxonID",
            "scientificNameID",
            "acceptedNameUsageID",
            "parentNameUsageID",
            "originalNameUsageID",
            "nameAccordingToID",
            "namePublishedInID",
            "taxonConceptID",
            "scientificName",
            "acceptedNameUsage",
            "parentNameUsage",
            "originalNameUsage",
            "nameAccordingTo",
            "namePublishedIn",
            "namePublishedInYear",
            "higherClassification",
            "kingdom",
            "phylum",
            "class",
            "order",
            "family",
            "subfamily",
            "genus",
            "subgenus",
            "specificEpithet",
            "infraspecificEpithet",
            "taxonRank",
            "verbatimTaxonRank",
            "scientificNameAuthorship",
            "vernacularName",
            "nomenclaturalCode",
            "taxonomicStatus",
            "nomenclaturalStatus",
            "taxonRemarks",
            "canonicalName"
)]

# add family names
families <- df[which(!duplicated(df$family)),] # deduplicated list by family
families <- families[which(!is.na(families$family)),] # remove the NA family row
families$canonicalName <- families$family
families$scientificName <- families$family
families$acceptedNameUsage <- NA
families$parentNameUsage <- families$order
families$namePublishedInYear <- NA
families$subfamily <- NA
families$genus <- NA
families$scientificNameAuthorship <- NA
families$subgenus <- NA
families$specificEpithet <- NA
families$infraspecificEpithet <- NA
families$taxonomicStatus <- "accepted"
families$taxonRank <- "family"

# add subfamily names
subfamilies <- df[which(!duplicated(df$subfamily)),] # deduplicated list by subfamily
subfamilies <- subfamilies[which(!is.na(subfamilies$subfamily)),] # remove the NA subfamily row
subfamilies$canonicalName <- subfamilies$subfamily
subfamilies$scientificName <- subfamilies$subfamily
subfamilies$acceptedNameUsage <- NA
subfamilies$parentNameUsage <- subfamilies$family
subfamilies$namePublishedInYear <- NA
subfamilies$genus <- NA
subfamilies$scientificNameAuthorship <- NA
subfamilies$subgenus <- NA
subfamilies$specificEpithet <- NA
subfamilies$infraspecificEpithet <- NA
subfamilies$taxonomicStatus <- "accepted"
subfamilies$taxonRank <- "subfamily"

# add order names
orders <- df[which(!duplicated(df$order)),] # deduplicated list by order
orders <- orders[which(!is.na(orders$order)),] # remove the NA order row
orders$canonicalName <- orders$order
orders$scientificName <- orders$order
orders$acceptedNameUsage <- NA
orders$parentNameUsage <- orders$class
orders$namePublishedInYear <- NA
orders$genus <- NA
orders$scientificNameAuthorship <- NA
orders$family <-NA
orders$subgenus <- NA
orders$specificEpithet <- NA
orders$infraspecificEpithet <- NA
orders$taxonomicStatus <- "accepted"
orders$taxonRank <- "order"

# add class names
classes <- df[which(!duplicated(df$class)),] # deduplicated list by class
classes <- classes[which(!is.na(classes$class)),] # remove the NA order row
classes$canonicalName <- classes$class
classes$scientificName <- classes$class
classes$acceptedNameUsage <- NA
classes$parentNameUsage <- classes$phylum
classes$namePublishedInYear <- NA
classes$genus <- NA
classes$scientificNameAuthorship <- NA
classes$order <-NA
classes$family <-NA
classes$subgenus <- NA
classes$specificEpithet <- NA
classes$infraspecificEpithet <- NA
classes$taxonomicStatus <- "accepted"
classes$taxonRank <- "class"

# add phyla names
phyla <- df[which(!duplicated(df$phylum)),] # deduplicated list by class
phyla <- phyla[which(!is.na(phyla$phylum)),] # remove the NA order row
phyla$canonicalName <- phyla$phylum
phyla$scientificName <- phyla$phylum
phyla$acceptedNameUsage <- NA
phyla$parentNameUsage <- phyla$kingdom
phyla$namePublishedInYear <- NA
phyla$genus <- NA
phyla$scientificNameAuthorship <- NA
phyla$class <- NA
phyla$order <-NA
phyla$family <-NA
phyla$subgenus <- NA
phyla$specificEpithet <- NA
phyla$infraspecificEpithet <- NA
phyla$taxonomicStatus <- "accepted"
phyla$taxonRank <- "phylum"

# add kingdom
kingdom <- df[which(!duplicated(df$kingdom)),] # deduplicated list by class
kingdom <- kingdom[which(!is.na(kingdom$kingdom)),] # remove the NA order row
kingdom$canonicalName <- kingdom$kingdom
kingdom$scientificName <- kingdom$kingdom
kingdom$acceptedNameUsage <- NA
kingdom$namePublishedInYear <- NA
kingdom$genus <- NA
kingdom$scientificNameAuthorship <- NA
kingdom$phylum <- NA
kingdom$class <- NA
kingdom$order <-NA
kingdom$family <-NA
kingdom$subgenus <- NA
kingdom$specificEpithet <- NA
kingdom$infraspecificEpithet <- NA
kingdom$taxonomicStatus <- "accepted"
kingdom$taxonRank <- "kingdom"

higher_taxa <- rbind(kingdom, phyla, classes, orders, families, subfamilies)
higher_taxa$taxonID <- seq.int(Lewis_original_rows + 1, Lewis_original_rows + nrow(higher_taxa)) # add numeric ID for each higher taxon name

df <- rbind(df, higher_taxa)

write.csv(Lewis_non_dwc,"~/GitHub/tpt-siphonaptera/output/Lewis_non_DwC.csv", row.names = FALSE) # removed fields
write.csv(df,"~/GitHub/tpt-siphonaptera/output/Lewis_DwC.csv", row.names = FALSE) # ready for analysis


# review for classification consistency

accepted <- df[which(df$taxonomicStatus == "accepted"),] # only look at accepted names
accepted[ is.na(accepted) ] <- "none"

# look for genera in more than one subfamily by selecting unique combinations of subfamily and genus
df3 <- accepted %>%
  group_by(grp = paste0(pmin(accepted$genus, accepted$subfamily), pmax(accepted$genus, accepted$subfamily))) %>%
  slice(1) %>%
  ungroup() %>%
  select(-grp)

df3$reason <- c(ifelse(duplicated(df3$genus, fromLast = TRUE)  | duplicated(df3$genus),
                      "subfamily mismatch", NA)) # Flag internal dupes

mismatch <- df3[which(df3$reason == "subfamily mismatch"),] # create a mismatch file

# look for subfamilies in more than one family by selecting unique combinations of subfamily and genus
df3 <- accepted %>%
  group_by(grp = paste0(pmin(accepted$subfamily, accepted$family), pmax(accepted$subfamily, accepted$family))) %>%
  slice(1) %>%
  ungroup() %>%
  select(-grp)

df3$reason <- c(ifelse(duplicated(df3$subfamily, fromLast = TRUE)  | duplicated(df3$subfamily),
                       "family mismatch", NA)) # Flag internal dupes

df3 <- df3[which(df3$reason == "family mismatch"),] # keep only dupes
mismatch <- rbind(mismatch,df3) # add family mismatch to mismatch file

# look for genus in more than one family by selecting unique combinations of subfamily and genus
df3 <- accepted %>%
  group_by(grp = paste0(pmin(accepted$genus, accepted$family), pmax(accepted$genus, accepted$family))) %>%
  slice(1) %>%
  ungroup() %>%
  select(-grp)

df3$reason <- c(ifelse(duplicated(df3$genus, fromLast = TRUE)  | duplicated(df3$genus),
                       "family mismatch", NA)) # Flag internal dupes

df3 <- df3[which(df3$reason == "family mismatch"),] # keep only dupes
mismatch <- rbind(mismatch,df3) # add family mismatch to mismatch file

write.csv(mismatch,"~/GitHub/tpt-siphonaptera/output/mismatched_classifications.csv", row.names = FALSE) # names that do not all share higher classification
