# import libraries
library(stringdist)
library(data.table)
library(stringi)
library(stringr)

# define function: make capitalization proper case
toproper <- function(x) ifelse(!is.na(x), paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))),NA)
# define function: remove '\xa0' chars and non-conforming punctuation
phrase_clean <- function(x) gsub("[^[:alnum:][:blank:]&,()]", "", x)

# "[^[:alnum:][:blank:]?&/\\-]"
# This grammar means: remove everything but:
# [:alnum:] Alphanumeric characters: 0-9 a-Z
# [:blank:] spaces and tabs
# ?&/\\- Specific characters you want to save for some reason. Punctuation signs can be saved here

# fix capitalization for both genus and species
# ignore author, publication, scientificName, and TPTID
# toproper all other fields
for(i in 1:ncol(df)) {
  name <- colnames(df)[i]
  if(grepl('infraspecificepithet', tolower(name), perl = TRUE)|
     grepl('variety', tolower(name), perl = TRUE) |
     grepl('form', tolower(name), perl = TRUE) |
     grepl('specificepithet', tolower(name), perl = TRUE)
  ) {
    df[,i] <- sapply(df[,i], tolower)
  } else if(grepl('author', tolower(name), perl = TRUE) |
            grepl('publi', tolower(name), perl = TRUE) | 
            grepl('tptdataset', tolower(name), perl = TRUE) | 
            grepl('scientific', tolower(name), perl = TRUE) |
            grepl('accepted', tolower(name), perl = TRUE) | 
            grepl('canonical', tolower(name), perl = TRUE) |
            grepl('remark', tolower(name), perl = TRUE) |
            grepl('TPTdataset', tolower(name), perl = TRUE)) {
  } else {
    df[,i] <- sapply(df[,i], toproper)
  }
}

# remove remove '\xa0' chars
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, phrase_clean), .SDcols = cols_to_be_rectified]

# strip spaces from ends of strings
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, trimws), .SDcols = cols_to_be_rectified]

# proceed to tpt_names_for_review script
