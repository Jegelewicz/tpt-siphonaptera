# Load libraries
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
  substr(string,(char + 1),nchar(string))
}

# define left function
left = function (string,char) {
  substr(string,1,char - 1)
}

# read in file
FMNH_Siphonaptera <- read_excel("~/GitHub/tpt-siphonaptera/input/FMNH_siphonaptera.xlsx", )
df <- FMNH_Siphonaptera # change filename for ease of use

original_rows <- nrow(df)
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
  x <- gsub('.*scientific.*','scientificName',x)
  x
}

colnames(df) <- convert2DwC(colnames(df)) # convert to DarwinCore terms

df <- rbindlist(list(df, tpt_dwc_template), fill = TRUE) # add all DwC columns

df$TPTdataset <- "FMNH" # add dataset name
df$TPTID <- seq.int(nrow(df)) # add numeric ID for each name

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

# melt scientific name
df <- melt_scientificname(df, 
                                sciname="scientificName", 
                                genus="genus",
                                subgenus="subgenus", 
                                species="specificEpithet", 
                                subspecies="infraspecificEpithet",
                                author="scientificNameAuthorship")

df$scientificNameAuthorship <- lapply(df$scientificNameAuthorship, trimws) # trim space left at beginning of scientifcNameAuthorship by function
df$scientificNameAuthorship <- vapply(df$scientificNameAuthorship, paste, collapse = ", ", character(1L)) # set scientific authorship to character

# remove parentheses from subgenera
df$subgenus <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", df$subgenus, perl=T) # get things in parenthesis for review

# extract sp's in specificEpithet and infraspecificEpithet
sp_wildcards <- c('sp', 'sp.', 'spp', 'spp.', 'sp.nov.', 'sp nov', 'sp. nov.', 
                  'prob', 'prob.', 'probably', 'unid', 'unidentified',
                  'spnov1')
variable_sp1 <- paste('sp', as.character(c(0:9)), sep='')
variable_sp2 <- paste('sp.', as.character(c(0:9)), sep='')
variable_sp3 <- paste('sp. ', as.character(c(0:9)), sep='')
sp_wildcards <- c(sp_wildcards, variable_sp1, variable_sp2, variable_sp3)
removed_sp <- df[which(df$specificEpithet %in% sp_wildcards), ] 
removed_sp$reason <- "specificEpithet flagged" # add review reason column
removed_spp <- df[(df$infraspecificEpithet %in% sp_wildcards), ]
removed_spp$reason <- "infraspecificEpithet flagged" # add review reason column
df_review <- rbind(removed_sp, removed_spp) # add extracted records to df_review
df <- df[which(df$specificEpithet %!in% sp_wildcards), ] # remove extracted spcificEpithet records from df
df <- df[which(df$infraspecificEpithet %!in% sp_wildcards), ] # remove extracted infraspecificEpithet records from df

write.csv(df_review,"~/GitHub/tpt-siphonaptera/output/taxa_need_review.csv", row.names = FALSE) # these need review

df_review <- read_excel("~/GitHub/tpt-siphonaptera/input/taxa_reviewed.xlsx", na = "NA")

df <- rbind(df, df_review) # add cleaned reviewed taxa back to working file

df$kingdom <- "Animalia" # add kingdom
df$phylum <- "Arthropoda" # add phylum
df$class <- "Insecta" # add class
df$order <- "Siphonaptera" # add order

# extract higher taxa for next set of review
higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),]

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

# Review higher taxa
write.csv(higher_taxa,"~/GitHub/tpt-siphonaptera/output/review_canonical.csv", row.names = FALSE) # these need review

# after review add back cleaned up names
higher_taxa <- read_excel("input/reviewed_canonical.xlsx", na = "NA") # read in cleaned review file
df <- rbind(higher_taxa, df) # add higher taxa back to df for remainder of de-duplication

FMNH_non_dwc <- subset(df, select = c(TPTdataset, TPTID, `in petra's list`)) # get all columns that are not DwC

# remove non DwC columns from working file
df$`in petra's list` <- NULL

# order column names
#df[,c(1,2,3,4)]. Note the first comma means keep all the rows, and the 1,2,3,4 refers to the columns.
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

# review for duplicates
dupe <- df[,c('canonicalName','taxonRank')] # select columns to check duplicates
review_dups <- df[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),]
df <- anti_join(df, review_dups, by = "TPTID") # remove duplicate rows from working file

# write and review duplicates
write.csv(review_dups,"~/GitHub/tpt-siphonaptera/output/FMNH_review_duplicates.csv", row.names = FALSE) # these need review
print("after review of duplicates, save return file to ~/GitHub/tpt-siphonaptera/input/reviewed_duplicates.xlsx")

reviewed_duplicates <- read_excel("input/reviewed_duplicates.xlsx") # read in cleaned duplicates
df <- rbind(df, reviewed_duplicates)

write.csv(FMNH_non_dwc,"~/GitHub/tpt-siphonaptera/output/FMNH_non_DwC.csv", row.names = FALSE) # removed fields
write.csv(df,"~/GitHub/tpt-siphonaptera/output/FMNH_DwC.csv", row.names = FALSE) # ready for analysis

# compare to Lewis list
Lewis_Siphonaptera <- read.csv("~/GitHub/tpt-siphonaptera/output/Lewis_DwC.csv", na = "NA") # read in cleaned Lewis review file
FMNH_Siphonaptera <- read.csv("~/GitHub/tpt-siphonaptera/output/FMNH_DwC.csv", na = "NA") # read in cleaned FMNH review file

FMNH_in_Lewis <- FMNH_Siphonaptera[FMNH_Siphonaptera$canonicalName %in% Lewis_Siphonaptera$canonicalName,] # get all rows in FMNH with canonical name that matches a row in Lewis

FMNH_not_in_Lewis <- anti_join(FMNH_Siphonaptera, Lewis_Siphonaptera, by = "canonicalName") # get all rows in FMNH that does not match a canonical name in Lewis

merged_siphonaptera <- df <- rbindlist(list(Lewis_Siphonaptera, FMNH_not_in_Lewis), fill = TRUE) # add FMNH terms not in Lewis to working file
