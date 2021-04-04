# add libraries
library(readxl)
library(data.table)
library(stringi)
library(taxotools)

# define function: name length
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)

# define function: is not in
'%!in%' <- function(x,y)!('%in%'(x,y))

# define right function
right = function (string, char) {
  substr(string,(char+1),nchar(string))
}

# define left function
left = function (string,char) {
  substr(string,1,char)
}

# read in file
Lewis_World_Species_List_1_APR_2021 <- read_excel("input/Lewis World Species List 1 APR 2021.xlsx")
df <- Lewis_World_Species_List_1_APR_2021 # change filename for ease of use
df <- df[-which(apply(df,1,function(x)all(is.na(x)))),] # remove empty rows
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

df$TPTID <- seq.int(nrow(df)) # add numeric ID for each name

df$kingdom <- "Animalia" # add kingdom
df$phylum <- "Arthropoda" # add phylum
df$class <- "Insecta" # add class
df$order <- "Siphonaptera" # add order

# clean up
# define function: remove '\xa0' chars and non-conforming punctuation
phrase_clean <- function(x) gsub("[^[:alnum:][:blank:]&,()];", "", x)
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
  multi_epithet$specificEpithet[i] <- left(multi_epithet$species[i], unlist(gregexpr(pattern = " ", multi_epithet$species[i]))) # place first term in specificEpithet
  multi_epithet$infraspecificEpithet[i] <- right(multi_epithet$species[i], unlist(gregexpr(pattern = " ", multi_epithet$species[i]))) # place second term in infraspecificEpithet
}

df$specificEpithet <- df$species # place single term species names in specificEpithet

df <- rbind(df,multi_epithet) # return subspecies to working file

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
# extract rows with canonicalName
df$canonicalName <- NA # create column for canonicalName
canonical <- df[which(lapply(df$canonicalName, name_length) != 0),] # remove rows with canonical elsewhere
df <- df[which(lapply(df$canonicalName, name_length) == 0),] # retain only rows with no canonical

# extract higher taxa for next set of review
higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),]

# generate canonical name for genus and below
df <- cast_canonical(df,
                     canonical="canonicalName", 
                     genus = "genus", 
                     species = "specificEpithet",
                     subspecies = "infraspecificEpithet")

# generate taxonRank for genus and below
for(i in 1:nrow(df)){
  df$taxonRank[i] <- 
    ifelse(!is.na(df$infraspecificEpithet[i]), "subspecies",
           ifelse(!is.na(df$specificEpithet[i]), "species",
                  "review"))
}

# canonical names for taxa ranked subgenus and above - get the lowest ranking term and put it here!
for(i in 1:nrow(higher_taxa)){
  higher_taxa$canonicalName[i] <- ifelse(!is.na(higher_taxa$genus[i]), paste(higher_taxa$genus[i]),
                                         ifelse(!is.na(higher_taxa$subgenus[i]), paste(higher_taxa$subgenus[i]),
                                               ifelse(!is.na(higher_taxa$family[i]), paste(higher_taxa$family[i]), "review")))
}

# generate taxonRank for genus and above
for(i in 1:nrow(higher_taxa)){
  higher_taxa$taxonRank[i] <- 
  ifelse(!is.na(higher_taxa$genus[i]), "genus",
     ifelse(!is.na(higher_taxa$subgenus[i]), "subgenus",
         ifelse(!is.na(higher_taxa$family[i]), "family",
                "review")))
}

# Extract rows from higher taxa that need review
flag <- c('review')
review_canonical <- higher_taxa[(higher_taxa$canonical %in% flag), ]
write.csv(review_canonical,"~/GitHub/tpt-siphonaptera/output/review_canonical.csv", row.names = FALSE) # these need review
higher_taxa <- higher_taxa[(higher_taxa$canonical %!in% flag), ] # extract review items from higher_taxa

if(nrow(review_canonical) == 0){
  print('No canonical names in higher_taxa have been flagged for review. Proceed to deduplication.')
  df <- rbind(higher_taxa, df) # add higher taxa back to df for remainder of de-duplication
} else{
  stop('Open the review_canonical file in the output folder, make adjustments as appropriate and save the revised file to input as reviewed_canonical.xlsx before proceeding')
  review_canonical <- reviewed_canonical <- read_excel("input/reviewed_canonical.xlsx") # read in cleaned review file
  higher_taxa <- rbind(higher_taxa, review_canonical) # add reviewed higher_taxa back to the working file
  df <- rbind(higher_taxa, df) # add higher taxa back to df for remainder of de-duplication
  
}

# cast scientific name
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


df_synonym <- df[which(lapply(df$`synonym(s)`, name_length) != 0), ] # extract rows with synonyms

# melt multiple synonyms
# synonym_all <- subset(df_synonym, select = c(scientificName, `synonym(s)`)) # get all rows that include a synonym
colno <- max(lengths(strsplit(df_synonym$`synonym(s)`, '; '))) # get max number of synonyms for any given accepted name
setDT(df_synonym)[, paste0("syn", 1:colno) := tstrsplit(`synonym(s)`, ";")] # parse out synonyms into separate columns
df_synonym$acceptedNameUsage <- df_synonym$scientificName
df_synonym$scientificName <- df_synonym$syn1
synonyms <- df_synonym

for (i in 2:colno){
  syn <- paste('syn', i, sep="")
  synonyms_append <- df_synonym[which(!is.na(df_synonym[[syn]]))] # get next set of synonyms
  synonyms_append$scientificName <- synonyms_append[[syn]] # move synonyms to scientific name
  synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms
}

# Get the parenthesis in scientificName and what is inside
synonyms$taxonRemarks <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", synonyms$scientificName, perl=T) # get things in parenthesis for review
synonyms$taxonRemarks[ synonyms$taxonRemarks == "" ] <- NA # set all blank remarks to NA
review_synonyms <- synonyms[which(!is.na(synonyms$taxonRemarks)), ] # extract synonyms with taxonRemarks
synonyms <- synonyms[which(is.na(synonyms$taxonRemarks)), ] # leave only NA taxonRemarks in synonyms
review_synonyms$scientificName <- gsub("\\([^()]*\\)", "", review_synonyms$scientificName) # get things outside parenthesis for review

# write and review taxonRemarks then add back to synonyms
write.csv(review_synonyms,"~/GitHub/tpt-siphonaptera/output/review_synonyms.csv", row.names = FALSE) # these need review
print("after corrections are made, save file to ~/GitHub/tpt-siphonaptera/input/reviewed_synonyms.xlsx")

reviewed_synonyms <- read_excel("input/reviewed_synonyms.xlsx") # read in cleaned synonyms for review

# add taxonmicStatus of "synonym" for any blanks
for(i in 1:nrow(reviewed_synonyms)){
  reviewed_synonyms$taxonomicStatus[i] <- ifelse(is.na(reviewed_synonyms$taxonomicStatus[i]), 
                                                 "synonym",
                                                 reviewed_synonyms$taxonomicStatus[i])
}

synonyms$taxonomicStatus <- "synonym" # add taxonomicStatus of "synonym" to all synonym names not needing review
df$taxonomicStatus <- "accepted" # add taxonomicStatus of "accepted" to all non-synonym names

synonyms <- rbindlist(list(reviewed_synonyms, synonyms), fill = TRUE) # put reviewed synonyms back with the rest

# melt scientific name of synonyms
synonyms <- melt_scientificname(synonyms, 
                    sciname="scientificName", 
                    genus="genus",
                    subgenus="subgenus", 
                    species="specificEpithet", 
                    subspecies="infraspecificEpithet",
                    author="scientificNameAuthorship")

synonyms$scientificNameAuthorship <- lapply(synonyms$scientificNameAuthorship, trimws) # trim space left at beginning of scientifcNameAuthorship by function

# cast canonicalName for synonyms
synonyms <- cast_canonical(synonyms,
                     canonical="canonicalName", 
                     genus = "genus", 
                     species = "specificEpithet",
                     subspecies = "infraspecificEpithet")

for (i in 1:colno){
  syn <- paste('syn', i, sep="")
  synonyms[[syn]] <- NULL # remove parsed columns from synonyms
}

df <- rbindlist(list(df, synonyms), fill = TRUE) # combine synonyms with accepted names in working file

Lewis_non_dwc <- subset(df, select = c(TPTdataset, TPTID, species, author, `synonym(s)`)) # get all rows that are not DwC
# remove non Dwc columns from working file
df$species <- NULL
df$author <- NULL
df$`synonym(s)` <- NULL
# order column names
#df[,c(1,2,3,4)]. Note the first comma means keep all the rows, and the 1,2,3,4 refers to the columns.
df <- df[,c("TPTdataset", "TPTID", "scientificName", "acceptedNameUsage", "namePublishedInYear", "kingdom",	"phylum",	"class", "order", "family",	"genus", "subgenus", "specificEpithet", "infraspecificEpithet",	"scientificNameAuthorship",	"taxonomicStatus", "nomenclaturalStatus",	"taxonRemarks", "canonicalName"
)]

# review for duplicates
# Review for internal duplication
df$dupe <- c(ifelse(duplicated(df$canonicalName, fromLast = TRUE)  | duplicated(df$canonicalName),
                                           "dupe", NA)) # Flag internal dupes
review_dups <- df[which(grepl('dupe',df$dupe) == TRUE), ] # extract dupes to review file
df <- df[which(grepl('dupe',df$dupe) == FALSE), ] # remove dupes from working file

# write and review taxonRemarks then add back to duplicates
review_dups <- apply(review_dups,2,as.character)
write.csv(review_dups,"~/GitHub/tpt-siphonaptera/output/review_duplicates.csv", row.names = FALSE) # these need review
print("after review of duplicates, save return file to ~/GitHub/tpt-siphonaptera/input/reviewed_duplicates.xlsx")

reviewed_duplicates <- read_excel("input/reviewed_duplicates.xlsx") # read in cleaned duplicates
df <- rbind(df, reviewed_duplicates)

write.csv(Lewis_non_dwc,"~/GitHub/tpt-siphonaptera/output/Lewis_non_DwC.csv", row.names = FALSE) # removed fields
write.csv(df,"~/GitHub/tpt-siphonaptera/output/Lewis_DwC.csv", row.names = FALSE) # ready for analysis
