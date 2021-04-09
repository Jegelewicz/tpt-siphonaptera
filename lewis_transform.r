# add libraries
library(readxl)
library(data.table)
library(stringi)
library(taxotools)
library(dplyr)

# define function: name length
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)

# define function: is not in
'%!in%' <- function(x,y)!('%in%'(x,y))

# define right function
right = function (string, char) {
  substr(string,(unlist(lapply(gregexpr(pattern = char, string), min)) + 1),nchar(string))
}

# define left function
left = function (string,char) {
  substr(string,1,unlist(lapply(gregexpr(pattern = char, string), min)))
}

# read in file
Lewis_World_Species_List <- read_excel("~/GitHub/tpt-siphonaptera/input/Lewis World Species List 6 APR 2021.xlsx")
df <- Lewis_World_Species_List # change filename for ease of use
# df <- df[-which(apply(df,1,function(x)all(is.na(x)))),] # remove empty rows
Lewis_original_rows <- nrow(df)
tpt_dwc_template <- read_excel("input/tpt_dwc_template.xlsx") # read in TPT DarwinCore template
tpt_dwc_template[] <- lapply(tpt_dwc_template, as.character) # set all columns in template to character

# transform column headers
colnames(df) <- tolower(colnames(df)) # lower case column names

# define DwC conversion
convert2DwC <- function(df_colname) {
  x <- gsub('.*subspecies.*','infraspecificEpithet',df_colname)
  x <- gsub('.*rank.*','taxonRank',x)
  x <- gsub('.*author.*','author',x)
  x <- gsub('.*year.*','namePublishedInYear',x)
  x
}

colnames(df) <- convert2DwC(colnames(df)) # convert to DarwinCore terms

df <- rbindlist(list(df, tpt_dwc_template), fill = TRUE) # add all DwC columns

df$TPTdataset <- "Lewis" # add dataset name
df$TPTID <- seq.int(nrow(df)) # add numeric ID for each name

df$kingdom <- "Animalia" # add kingdom
df$phylum <- "Arthropoda" # add phylum
df$class <- "Insecta" # add class
df$order <- "Siphonaptera" # add order

is.na(df$subfamily) <- df$subfamily == "NONE" # remove "NONE" from subfamily

# clean up
# define function: remove '\xa0' chars and non-conforming punctuation
phrase_clean <- function(x) gsub("[\xA0]", "", x)
space_clean <- function(x) gsub("  ", " ", x)

# remove remove '\xa0' chars
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, phrase_clean), .SDcols = cols_to_be_rectified]

# strip spaces from ends of strings
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, trimws), .SDcols = cols_to_be_rectified]

# strip double spaces
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, space_clean), .SDcols = cols_to_be_rectified]

# split specificEpithet when it has two terms
multi_epithet <- df[which(lapply(df$species, name_length) > 1),] # extract rows with a multi-name specifies
df <- df[which(lapply(df$species, name_length) <= 1),] # extract rows with a multi-name specifies

for(i in 1:nrow(multi_epithet)){
  multi_epithet$specificEpithet[i] <- left(multi_epithet$species[i], " ") # place first term in specificEpithet
  multi_epithet$infraspecificEpithet[i] <- right(multi_epithet$species[i], " ") # place second term in infraspecificEpithet
}

# strip spaces from ends of strings
setDT(multi_epithet)
cols_to_be_rectified <- names(multi_epithet)[vapply(multi_epithet, is.character, logical(1))]
multi_epithet[,c(cols_to_be_rectified) := lapply(.SD, trimws), .SDcols = cols_to_be_rectified]

# strip double spaces
setDT(multi_epithet)
cols_to_be_rectified <- names(multi_epithet)[vapply(multi_epithet, is.character, logical(1))]
multi_epithet[,c(cols_to_be_rectified) := lapply(.SD, space_clean), .SDcols = cols_to_be_rectified]

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

fixAuth <- function(x) ifelse(grepl('[a-z]),',x), paste(gsub(')', '',x),')',sep=''),x) # define function to fix cases like: (Jordan & Rothschild), 1922
df$scientificNameAuthorship <- fixAuth(df$scientificNameAuthorship) # apply fix

# cast canonical name
df$canonicalName <- NA # create column for canonicalName

# extract higher taxa for next set of review
higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),] # create dataframe of higher taxa
df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),] # remove higher taxa from working file

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

# Extract rows from higher taxa that need review
flag <- c('review')
review_canonical <- higher_taxa[(higher_taxa$canonical %in% flag), ]
review_canonical$reason <- "review canonical name" # add removal reason
higher_taxa <- higher_taxa[(higher_taxa$canonical %!in% flag), ] # extract review items from higher_taxa
df <- rbind(higher_taxa, df) # add higher taxa back to df for remainder of de-duplication

df_synonym <- df[which(lapply(df$`synonym(s)`, name_length) != 0), ] # extract rows with synonyms

# melt multiple synonyms
# synonym_all <- subset(df_synonym, select = c(scientificName, `synonym(s)`)) # get all rows that include a synonym
colno <- max(lengths(strsplit(df_synonym$`synonym(s)`, '; '))) # get max number of synonyms for any given accepted name
setDT(df_synonym)[, paste0("syn", 1:colno) := tstrsplit(`synonym(s)`, ";")] # parse out synonyms into separate columns

# strip spaces from ends of strings
setDT(df_synonym)
cols_to_be_rectified <- names(df_synonym)[vapply(df_synonym, is.character, logical(1))]
df_synonym[,c(cols_to_be_rectified) := lapply(.SD, trimws), .SDcols = cols_to_be_rectified]

# strip double spaces
setDT(df_synonym)
cols_to_be_rectified <- names(df_synonym)[vapply(df_synonym, is.character, logical(1))]
df_synonym[,c(cols_to_be_rectified) := lapply(.SD, space_clean), .SDcols = cols_to_be_rectified]

df_synonym$acceptedNameUsage <- df_synonym$scientificName # copy accepted scientific name to accepted name column
df_synonym$scientificName <- df_synonym$syn1 # move synonym name to scientific name column
df_synonym$infraspecificEpithet <- NA # clear subspecifc names of accepted name classification
df_synonym$specificEpithet <- NA # clear specific names of accepted name classification
synonyms <- df_synonym # create synonyms data frame

# get synonyms from columns syn2-last
for (i in 2:colno){
  syn <- paste('syn', i, sep="")
  synonyms_append <- df_synonym[which(!is.na(df_synonym[[syn]]))] # get next set of synonyms
  synonyms_append$scientificName <- synonyms_append[[syn]] # move synonyms to scientific name
  synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms
}

Lewis_synonym_rows <- nrow(synonyms)

# Add TPTdataset and identifiers to synonyms
synonyms$TPTdataset <- "Lewis" # add TPT dataset = Lewis
synonyms$acceptedNameUsageID <- paste(synonyms$TPTdataset, synonyms$TPTID, sep = "") # copy ID to accepted ID column
synonyms$TPTID <- seq.int(Lewis_original_rows + 1, Lewis_original_rows + nrow(synonyms)) # add numeric ID for each synonym name

# deal with parenthesis in synonym scientificName
synonyms$taxonRemarks <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", synonyms$scientificName, perl=T) # pu things in parentheses in taxonRemark
synonyms$taxonRemarks[ synonyms$taxonRemarks == "" ] <- NA # set all blank taxonRemark to NA
synonyms$scientificName <- gsub("\\([^()]*\\)", "", synonyms$scientificName) # get things outside parenthesis for scientificName

# strip spaces from ends of strings
setDT(synonyms)
cols_to_be_rectified <- names(synonyms)[vapply(synonyms, is.character, logical(1))]
synonyms[,c(cols_to_be_rectified) := lapply(.SD, trimws), .SDcols = cols_to_be_rectified]

# remove parsed columns from synonyms
for (i in 1:colno){
  syn <- paste('syn', i, sep="")
  synonyms[[syn]] <- NULL 
}

synonyms$taxonomicStatus <- "synonym" # add taxonomicStatus of "synonym" to all synonym names
df$taxonomicStatus <- "accepted" # add taxonomicStatus of "accepted" to all non-synonym names

# extract higher taxa for next set of review
higher_taxa <- synonyms[which(synonyms$taxonRank == "genus"),]
synonyms <- synonyms[which(synonyms$taxonRank == "species" | synonyms$taxonRank == "subspecies"),]

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
# synonyms$taxonRemarks <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", synonyms$scientificName, perl=T) # get things in parenthesis for review
# synonyms$taxonRemarks[ synonyms$taxonRemarks == "" ] <- NA # set all blank remarks to NA
review_synonyms <- synonyms[which(!is.na(synonyms$taxonRemarks)), ] # extract synonyms with taxonRemarks
synonyms <- synonyms[which(is.na(synonyms$taxonRemarks)), ] # leave only NA taxonRemarks in synonyms
# review_synonyms$scientificName <- gsub("\\([^()]*\\)", "", review_synonyms$scientificName) # get things outside parenthesis for review

# strip spaces from ends of strings
setDT(synonyms)
cols_to_be_rectified <- names(synonyms)[vapply(synonyms, is.character, logical(1))]
synonyms[,c(cols_to_be_rectified) := lapply(.SD, trimws), .SDcols = cols_to_be_rectified]

review_synonyms$reason <- "review taxon remark"

Lewis_review <- rbind(review_canonical, review_synonyms) # combine synonyms for review with canonical names for review

df <- rbindlist(list(df, synonyms), fill = TRUE) # combine synonyms with accepted names in working file

# review for duplicates
dupe <- df[,c('canonicalName','taxonRank')] # select columns to check duplicates
review_dups <- df[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),] # create duplicates data frame
review_dups$reason <- "duplicate canonical name" # add reason for removal from working file
df <- anti_join(df,review_dups, by = c("canonicalName", "taxonRank")) # remove duplicate rows from working file
Lewis_review <- rbind(Lewis_review, review_dups) # add removed duplicates to review file 

# sanity check
ifelse(nrow(df) + nrow(Lewis_review) == Lewis_original_rows + Lewis_synonym_rows, 
       "Its all here", 
       "oops")

# write and review Lewis_removed then add back to duplicates
write.csv(df,"~/GitHub/tpt-siphonaptera/output/Lewis_Siphonaptera.csv", row.names = FALSE) # Lewis no dups
write.csv(Lewis_review,"~/GitHub/tpt-siphonaptera/output/Lewis_review.csv", row.names = FALSE) # these need review
print("after review of removed rows, save return file to ~/GitHub/tpt-siphonaptera/input/Lewis_reviewed.xlsx and any remaining rows to ~/GitHub/tpt-siphonaptera/output/Lewis_need_expert_review.xlsx")

df <- read_excel("~/GitHub/tpt-siphonaptera/input/Lewis_Siphonaptera.xlsx") # read in cleaned names
Lewis_review <- read_excel("~/GitHub/tpt-siphonaptera/input/Lewis_reviewed.xlsx") # read in rows reviewed
Lewis_removed <- read_excel("~/GitHub/tpt-siphonaptera/input/Lewis_removed.xlsx") # read in rows not reviewed

df <- rbind(df, Lewis_reviewed) # return kept rows

# review for duplicates
dupe <- df[,c('canonicalName','taxonRank')] # select columns to check duplicates
review_dups <- df[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),] # create duplicates data frame
review_dups$reason <- "duplicate canonical name" # add reason for removal from working file
df <- anti_join(df,review_dups, by = c("canonicalName", "taxonRank")) # remove duplicate rows from working file
Lewis_removed <- rbind(Lewis_removed, review_dups) # add removed duplicates to review file 

# sanity check
ifelse(nrow(df) + nrow(Lewis_removed) == Lewis_original_rows + Lewis_synonym_rows, 
       "Its all here", 
       "oops")

# write and review Lewis_removed then add back to duplicates
write.csv(df,"~/GitHub/tpt-siphonaptera/output/Lewis_Siphonaptera.csv", row.names = FALSE) # Lewis no dups
write.csv(Lewis_removed,"~/GitHub/tpt-siphonaptera/output/Lewis_review2.csv", row.names = FALSE) # these need review
print("after review of removed rows, save return file to ~/GitHub/tpt-siphonaptera/input/Lewis_reviewed2.xlsx and any remaining rows to ~/GitHub/tpt-siphonaptera/output/Lewis_need_expert_review.xlsx")

df <- read_excel("~/GitHub/tpt-siphonaptera/input/Lewis_Siphonaptera.xlsx") # read in cleaned names
Lewis_reviewed <- read_excel("~/GitHub/tpt-siphonaptera/input/Lewis_reviewed2.xlsx") # read in cleaned duplicates
Lewis_removed <- read_excel("~/GitHub/tpt-siphonaptera/input/Lewis_removed.xlsx") # read in rows not reviewed

df <- rbind(df, Lewis_reviewed) # return kept rows

# review for duplicates
dupe <- df[,c('canonicalName','taxonRank')] # select columns to check duplicates
review_dups <- df[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),] # create duplicates data frame
review_dups$reason <- "duplicate canonical name" # add reason for removal from working file
df <- anti_join(df,review_dups, by = c("canonicalName", "taxonRank")) # remove duplicate rows from working file
Lewis_removed <- rbind(Lewis_removed, review_dups) # add removed duplicates to review file 

# sanity check
ifelse(nrow(df) + nrow(Lewis_removed) == Lewis_original_rows + Lewis_synonym_rows, 
       "Its all here", 
       "oops")

# write and review Lewis_removed then add back to duplicates
write.csv(df,"~/GitHub/tpt-siphonaptera/output/Lewis_Siphonaptera.csv", row.names = FALSE) # Lewis no dups
write.csv(Lewis_removed,"~/GitHub/tpt-siphonaptera/output/Lewis_review2.csv", row.names = FALSE) # these need review
print("after review of removed rows, save return file to ~/GitHub/tpt-siphonaptera/input/Lewis_reviewed2.xlsx and any remaining rows to ~/GitHub/tpt-siphonaptera/output/Lewis_need_expert_review.xlsx")


# Do this after final review...
Lewis_non_dwc <- subset(df, select = c(TPTdataset, TPTID, species, author, `synonym(s)`)) # get all columns that are not DwC
# remove non DwC columns from working file
df$species <- NULL
df$author <- NULL
df$`synonym(s)` <- NULL

# order column names
# df[,c(1,2,3,4)]. Note the first comma means keep all the rows, and the 1,2,3,4 refers to the columns.
df <- df[,c("TPTdataset",
            "TPTID",
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
            "taxonRemarks"
)]
# 
# write.csv(Lewis_non_dwc,"~/GitHub/tpt-siphonaptera/output/Lewis_non_DwC.csv", row.names = FALSE) # removed fields
# write.csv(df,"~/GitHub/tpt-siphonaptera/output/Lewis_DwC.csv", row.names = FALSE) # ready for analysis
