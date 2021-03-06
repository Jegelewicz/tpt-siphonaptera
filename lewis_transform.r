# read in file
Lewis_World_Species_List <- read_excel("~/GitHub/tpt-siphonaptera/input/Lewis World Species List 18 MAY 2021.xlsx")
df <- Lewis_World_Species_List # change filename for ease of use
tpt_dwc_template <- read_excel("input/tpt_dwc_template.xlsx") # read in TPT DarwinCore template
tpt_dwc_template[] <- lapply(tpt_dwc_template, as.character) # set all columns in template to character

# transform column headers
colnames(df) <- tolower(colnames(df)) # lower case column names

colnames(df) <- convert2DwC(colnames(df)) # convert to DarwinCore terms

df <- rbindlist(list(df, tpt_dwc_template), fill = TRUE) # add all DwC columns

df$source <- "Lewis" # add dataset name

no_name <- df[which(lapply(df$author, name_length) == 0),] # extract rows with no author
df <- df[which(lapply(df$author, name_length) != 0),] # remove rows with no author
Lewis_original_rows <- nrow(df) # number of accepted names
df$taxonID <- seq.int(nrow(df)) # add numeric ID for each name
df <- rbind(df, no_name) # add back rows with no author

df$kingdom <- "Animalia" # add kingdom
df$phylum <- "Arthropoda" # add phylum
df$class <- "Insecta" # add class
df$order <- "Siphonaptera" # add order

is.na(df$subfamily) <- df$subfamily == "NONE" # remove "NONE" from subfamily
df$family <- toproper(df$family) # ensure all family names are proper case

# deal with parenthesis in species
df$taxonRemarks <- inparens(df$species)# for species column put things in parentheses in taxonRemark
df$taxonRemarks[ df$taxonRemarks == "" ] <- NA # set all blank taxonRemark to NA
df$species <- outparens(df$species)# get things outside parenthesis for species


df <- char_fun(df,phrase_clean) # remove all \xa0 characters
df <- char_fun(df,trimws) # trim white space
df <- char_fun(df,space_clean) # replace double spaces with single space

# split specificEpithet when it has two terms
multi_epithet <- df[which(lapply(df$species, name_length) > 1),] # extract rows with a multi-name specifies
df <- df[which(lapply(df$species, name_length) <= 1),] # extract rows with a multi-name specifies

for(i in 1:nrow(multi_epithet)){
  multi_epithet$specificEpithet[i] <- left(multi_epithet$species[i], " ") # place first term in specificEpithet
  multi_epithet$infraspecificEpithet[i] <- right(multi_epithet$species[i], " ") # place second term in infraspecificEpithet
}

df <- char_fun(df,trimws) # strip spaces from ends of strings
df <- char_fun(df,space_clean) # strip double spaces

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

df_synonym <- df[which(lapply(df$`synonym(s)`, name_length) != 0), ] # extract rows with synonyms

# melt multiple synonyms
df_synonym <- text_to_columns(df_synonym,
                              df_synonym$`synonym(s)`,
                              data = "df_synonym",
                              column = "`synonym(s)`",
                              separator = ";",
                              new_col_name_prefix = "syn")

df <- char_fun(df,trimws) # strip spaces from ends of strings
df <- char_fun(df,space_clean) # strip double spaces

df_synonym$acceptedNameUsage <- ifelse(df_synonym$scientificName == "review", NA, df_synonym$scientificName) # copy accepted scientific name to accepted name column
df_synonym$scientificName <- df_synonym$syn1 # move synonym name to scientific name column
df_synonym$infraspecificEpithet <- NA # clear subspecifc names of accepted name classification
df_synonym$specificEpithet <- NA # clear specific names of accepted name classification
synonyms <- df_synonym # create synonyms data frame

# get synonyms from columns syn2-last
colno <- max(lengths(strsplit(df_synonym$`synonym(s)`, ";"))) # get max number of terms for any value in the column to be split
for (i in 2:colno){
  syn <- paste('syn', i, sep="")
  synonyms_append <- df_synonym[which(!is.na(df_synonym[[syn]]))] # get next set of synonyms
  synonyms_append$scientificName <- synonyms_append[[syn]] # move synonyms to scientific name
  synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms
}

Lewis_synonym_rows <- nrow(synonyms)

# Add TPTdataset and identifiers to synonyms
synonyms$acceptedNameUsageID <- synonyms$taxonID # copy ID to accepted ID column
synonyms$taxonID <- seq.int(Lewis_original_rows + 1, Lewis_original_rows + nrow(synonyms)) # add numeric ID for each synonym name

# deal with parenthesis in synonym scientificName
synonyms$taxonRemarks <- inparens(synonyms$scientifcName) # put things in parentheses in taxonRemark
synonyms$taxonRemarks[ synonyms$taxonRemarks == "" ] <- NA # set all blank taxonRemark to NA
synonyms$scientificName <- outparens(synonyms$scientificName) # get things outside parenthesis for scientificName

# remove parsed columns from synonyms
for (i in 1:colno){
  syn <- paste('syn', i, sep="")
  synonyms[[syn]] <- NULL 
}

synonyms$taxonomicStatus <- "synonym" # add taxonomicStatus of "synonym" to all synonym names
df$taxonomicStatus <- "accepted" # add taxonomicStatus of "accepted" to all non-synonym names

# extract higher taxa for next set of review
higher_taxa <- higher_taxa_rank(synonyms,synonyms$taxonRank) # create higher taxa data frame
synonyms <- species_rank(synonyms,synonyms$taxonRank) # remove higher taxa from working synonym file

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

# generate taxonRank for species and below
for(i in 1:nrow(synonyms)){
  synonyms$taxonRank[i] <- 
    ifelse(!is.na(synonyms$infraspecificEpithet[i]), "subspecies",
           ifelse(!is.na(synonyms$specificEpithet[i]), "species",
                  "review"))
}

# melt scientific name for higher taxa
for(i in 1:nrow(higher_taxa)){
  if(higher_taxa$taxonRank[i] == "genus"){
    higher_taxa$genus[i] <- left(higher_taxa$scientificName[i]," ")
    }else{
          if(higher_taxa$taxonRank[i] == "subfamily"){ 
          higher_taxa$subfamily[i] <- left(higher_taxa$scientificName[i], " ")
          }else{
                if(higher_taxa$taxonRank[i] == "family"){
                higher_taxa$family[i] <- left(higher_taxa$scientificName[i], " ")}
            }
      }
}

# canonical names for taxa ranked subgenus and above - get the lowest ranking term and put it here!
for(i in 1:nrow(higher_taxa)){
  higher_taxa$canonicalName[i] <- ifelse(!is.na(higher_taxa$subgenus[i]), paste(higher_taxa$subgenus[i]),
                                         ifelse(!is.na(higher_taxa$genus[i]), paste(higher_taxa$genus[i]),
                                                ifelse(!is.na(higher_taxa$family[i]), paste(higher_taxa$family[i]),
                                                       ifelse(!is.na(higher_taxa$subfamily[i]), paste(higher_taxa$subfamily[i]),
                                                              "review"))))
}

# generate scientificNameAuthorship for genus and above
higher_taxa$scientificNameAuthorship <- right(higher_taxa$scientificName," ")

synonyms <- rbind(synonyms, higher_taxa) # add higher taxa synonyms back to synonyms file

# flag synonyms that need review
review_synonyms <- synonyms[which(!is.na(synonyms$taxonRemarks)), ] # extract synonyms with taxonRemarks
synonyms <- synonyms[which(is.na(synonyms$taxonRemarks)), ] # leave only NA taxonRemarks in synonyms

review_synonyms$reason <- "review taxon remark"

# Lewis_review <- rbind(review_canonical, review_synonyms) # combine synonyms for review with canonical names for review

df <- rbindlist(list(df, synonyms), fill = TRUE) # combine synonyms with accepted names in working file

no_name <- df[which(lapply(df$author, name_length) == 0),] # extract rows with no author
df <- df[which(lapply(df$author, name_length) != 0),] # remove rows with no author

# clean up or remove "no_name"
Lewis_removed <- no_name[which(is.na(no_name$taxonID)),] # remove rows with no taxonid
write.csv(Lewis_removed,"~/GitHub/tpt-siphonaptera/output/Lewis_removed.csv", row.names = FALSE) # write out Lewis removed
Lewis_reviewed <- no_name[which(!is.na(no_name$taxonID)),] # names with taxonid for cleanup

# melt scientific name for review items
Lewis_reviewed <- melt_scientificname(Lewis_reviewed, 
                                sciname="scientificName", 
                                genus="genus",
                                subgenus="subgenus", 
                                species="specificEpithet", 
                                subspecies="infraspecificEpithet",
                                author="scientificNameAuthorship")

# generate taxonRank for species and below
for(i in 1:nrow(Lewis_reviewed)){
  Lewis_reviewed$taxonRank[i] <- 
    ifelse(!is.na(Lewis_reviewed$infraspecificEpithet[i]), "subspecies",
           ifelse(!is.na(Lewis_reviewed$specificEpithet[i]), "species",
                  "review"))
}

# cast canonical for review items
Lewis_reviewed <- cast_canonical(Lewis_reviewed,
                           canonical="canonicalName", 
                           genus = "genus", 
                           species = "specificEpithet",
                           subspecies = "infraspecificEpithet")

df <- rbind(df, Lewis_reviewed) # return corrected reviewed rows

# Do this after final review...
Lewis_non_dwc <- subset(df, select = c(source, taxonID, species, author, `synonym(s)`)) # get all columns that are not DwC
# remove non DwC columns from working file
df$species <- NULL
df$author <- NULL
df$`synonym(s)` <- NULL

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
 
write.csv(Lewis_non_dwc,"~/GitHub/tpt-siphonaptera/output/Lewis_non_DwC.csv", row.names = FALSE) # removed fields
write.csv(df,"~/GitHub/tpt-siphonaptera/output/Lewis_DwC.csv", row.names = FALSE) # ready for analysis

# Fill in DwC fields
df$nomenclaturalCode <- "ICZN" # set nomenclatural code
df$namePublishedInYear <- right(df$scientificNameAuthorship,",") # get year from author text
df$namePublishedInYear <- gsub("[^0-9]", "",df$namePublishedInYear) # leave only numbers

# get GBIF taxonids
# for (i in 1:nrow(df)){
  # df$GBIFtaxonID[i] <- vlookup(GBIF$taxonID,df$canonicalName[i],GBIF$canonicalName)
  # if (is.na(df$GBIFtaxonID[i])){
  #   df$taxonID[i] <- paste(df$source[i],df$taxonID[i],sep = "")
  # } else {
  #   df$taxonID[i] <- paste("https://www.gbif.org/species/",df$GBIFtaxonID[i],sep = "")
  # }
# }



accepted <- df
# get accepted names for synonyms
for (i in 1:nrow(df)){
  df$acceptedNameUsage[i] <- vlookup(accepted$canonicalName,df$acceptedNameUsageID[i],accepted$taxonID)
}

# get parent names
for (i in 1:nrow(df)){
  if (df$taxonRank[i] == "genus"){
    if (is.na(df$subfamily[i])){
      df$parentNameUsage[i] <- df$family[i]
    } else {
      df$parentNameUsage[i] <- df$subfamily[i]
    }
  }
}


for (i in 1:nrow(df)){
    if (df$taxonRank[i] == "species"){
      if (is.na(df$subgenus[i])){
        df$parentNameUsage[i] <- df$genus[i]
      } else {
        df$parentNameUsage[i] <- df$subgenus[i]
      }
    }
}



for (i in 1:nrow(df)){
  if (df$taxonRank[i] == "subspecies"){
    if (is.na(df$subgenus[i])){
      df$parentNameUsage[i] <- paste(df$genus[i],df$specificEpithet[i], sep = " ")
    } else {
      subgenus <- paste("(",df$subgenus[i],")", sep = "")
      df$parentNameUsage[i] <- paste(df$genus[i],subgenus, df$specificEpithet[i], sep = " ")
    }
  }
}
    

