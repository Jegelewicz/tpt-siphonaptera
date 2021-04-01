# add libraries
library(readxl)
library(data.table)
library(stringi)
library(taxotools)

# read in file
Lewis_World_Species_List_1_APR_2021 <- read_excel("input/Lewis World Species List 1 APR 2021.xlsx")
df <- Lewis_World_Species_List_1_APR_2021 # change filename for ease of use
df <- df[-which(apply(df,1,function(x)all(is.na(x)))),] # remove empty rows

# transform column headers
colnames(df) <- tolower(colnames(df)) # lower case column names

# define DarwinCore conversion function
convert2DwC <- function(df_colname) {
  x <- gsub('.*subspecies.*','infraspecificEpithet',df_colname)
  x <- gsub('.*rank.*','taxonRank',x)
  x <- gsub('.*author.*','author',x)
  x <- gsub('.*year.*','namePublishedInYear',x)
  x
} # this needs work

colnames(df) <- convert2DwC(colnames(df)) # convert to DarwinCore terms

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
split_epithet <- data.frame(do.call('rbind', strsplit(as.character(multi_epithet$species),' ',fixed=TRUE))) # split two terms in species into two columns
colnames(split_epithet)<- c("specificEpithet","infraspecificEpithet") # rename column headers
multi_epithet <- cbind(multi_epithet, split_epithet) # add columns to multi_epithet
df$specificEpithet <- df$species # add specificEpithet column to df and import species
df$infraspecificEpithet <- NA # add infraspecificEpithet column to df
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

# cast scientific name
df$scientificName <- NA # create column for scientificName
for(i in 1:nrow(df)){
  df$scientificName[i] <- ifelse(is.na(df$scientificNameAuthorship[i]) &
                                             is.na(df$canonicalName[i]), NA, # if both canonicalName and scientificNameAuthorship are blank, insert NA
                                           ifelse(is.na(df$scientificNameAuthorship[i]),df$canonicalName[i], # if canonicalName is not blank but scientificNameAuthorship is, insert canonicalName
                                                  ifelse(is.na(df$canonicalName[i]), "no canonical name", # if canonicalName is blank, but scientificNameAuthorship is not, insert warning
                                                         paste(df$canonicalName[i], df$scientificNameAuthorship[i], sep = ' ')) # if both canonicalName and scientificNameAuthorship are NOT blank merge and insert
                                           )
  )
}


# define function: name length
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)
# define function: is not in
'%!in%' <- function(x,y)!('%in%'(x,y))

df_synonym <- df[which(lapply(df$`synonym(s)`, name_length) != 0), ] # extract rows with synonyms

# melt multiple synonyms
synonym_all <- subset(df_synonym, select = c(scientificName, `synonym(s)`))

colno <- max(lengths(strsplit(synonym_all$`synonym(s)`, ';')))
setDT(synonym_all)[, paste0("syn", 1:colno) := tstrsplit(`synonym(s)`, ";")]

synonyms <- synonym_all[, c("scientificName", "syn1")]
colnames(synonyms)<- c("acceptedName","scientificName")

# get second set of synonyms
synonyms_append <- synonym_all[, c("scientificName", "syn2")]
colnames(synonyms_append)<- c("acceptedName","scientificName") # change column names to DwC
synonyms_append <- synonyms_append[!is.na(synonyms_append$scientificName), ] # remove rows with no synonym
synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms


# get third set of synonyms
synonyms_append <- synonym_all[, c("scientificName", "syn3")]
synonyms_append <- synonyms_append[!is.na(synonyms_append$syn3), ] # remove rows with no synonym
colnames(synonyms_append)<- c("acceptedName","scientificName") # change column names to DwC
synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms

# get fourth set of synonyms
synonyms_append <- synonym_all[, c("scientificName", "syn4")]
synonyms_append <- synonyms_append[!is.na(synonyms_append$syn4), ] # remove rows with no synonym
colnames(synonyms_append)<- c("acceptedName","scientificName") # change column names to DwC
synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms

# get fifth set of synonyms
synonyms_append <- synonym_all[, c("scientificName", "syn5")]
synonyms_append <- synonyms_append[!is.na(synonyms_append$syn5), ] # remove rows with no synonym
colnames(synonyms_append)<- c("acceptedName","scientificName") # change column names to DwC
synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms

# get sixth set of synonyms
synonyms_append <- synonym_all[, c("scientificName", "syn6")]
synonyms_append <- synonyms_append[!is.na(synonyms_append$syn6), ] # remove rows with no synonym
colnames(synonyms_append)<- c("acceptedName","scientificName") # change column names to DwC
synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms

# get eighth set of synonyms
synonyms_append <- synonym_all[, c("scientificName", "syn8")]
synonyms_append <- synonyms_append[!is.na(synonyms_append$syn8), ] # remove rows with no synonym
colnames(synonyms_append)<- c("acceptedName","scientificName") # change column names to DwC
synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms

# get tenth set of synonyms
synonyms_append <- synonym_all[, c("scientificName", "syn10")]
synonyms_append <- synonyms_append[!is.na(synonyms_append$syn10), ] # remove rows with no synonym
colnames(synonyms_append)<- c("acceptedName","scientificName") # change column names to DwC
synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms

# get eleventh set of synonyms
synonyms_append <- synonym_all[, c("scientificName", "syn11")]
synonyms_append <- synonyms_append[!is.na(synonyms_append$syn11), ] # remove rows with no synonym
colnames(synonyms_append)<- c("acceptedName","scientificName") # change column names to DwC
synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms

# get twelevth set of synonyms
synonyms_append <- synonym_all[, c("scientificName", "syn12")]
synonyms_append <- synonyms_append[!is.na(synonyms_append$syn12), ] # remove rows with no synonym
colnames(synonyms_append)<- c("acceptedName","scientificName") # change column names to DwC
synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms

# get thirteenth set of synonyms
synonyms_append <- synonym_all[, c("scientificName", "syn13")]
synonyms_append <- synonyms_append[!is.na(synonyms_append$syn13), ] # remove rows with no synonym
colnames(synonyms_append)<- c("acceptedName","scientificName") # change column names to DwC
synonyms <- rbind(synonyms, synonyms_append) # combine with synonyms


