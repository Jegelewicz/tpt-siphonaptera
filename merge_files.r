# add libraries
library(data.table)
library(readxl)

# define function: is not in
'%!in%' <- function(x,y)!('%in%'(x,y))

# compare FMNH to NMNH
NMNH_Siphonaptera <- read.csv("~/GitHub/tpt-siphonaptera/output/NMNH_DwC.csv", na = "NA") # read in cleaned NMNH review file
FMNH_Siphonaptera <- read.csv("~/GitHub/tpt-siphonaptera/output/FMNH_DwC.csv", na = "NA") # read in cleaned FMNH review file

FMNH_in_NMNH <- FMNH_Siphonaptera[FMNH_Siphonaptera$canonicalName %in% NMNH_Siphonaptera$canonicalName,] # get all rows in FMNH with canonical name that matches a row in Lewis

FMNH_not_in_NMNH <- FMNH_Siphonaptera[FMNH_Siphonaptera$canonicalName %!in% NMNH_Siphonaptera$canonicalName,] # get all rows in FMNH that does not match a canonical name in Lewis

merged_siphonaptera <- rbindlist(list(NMNH_Siphonaptera, FMNH_not_in_NMNH), fill = TRUE) # add FMNH terms not in NMNH to NMNH

# compare to Lewis list
Lewis_Siphonaptera <- read.csv("~/GitHub/tpt-siphonaptera/output/Lewis_Siphonaptera.csv", na = "NA") # read in cleaned Lewis review file

merged_in_Lewis <- merged_siphonaptera[merged_siphonaptera$canonicalName %in% Lewis_Siphonaptera$canonicalName,] # get all rows in merged with canonical name that matches a row in Lewis

merged_not_in_Lewis <- merged_siphonaptera[merged_siphonaptera$canonicalName %!in% Lewis_Siphonaptera$canonicalName,] # get all rows in merged that do not match a canonical name in Lewis

Lewis_in_merged <- Lewis_Siphonaptera[Lewis_Siphonaptera$canonicalName %in% merged_siphonaptera$canonicalName,] # get all rows in merged with canonical name that matches a row in Lewis

Lewis_not_in_merged <- Lewis_Siphonaptera[Lewis_Siphonaptera$canonicalName %!in% merged_siphonaptera$canonicalName,] # get all rows in merged that do not match a canonical name in Lewis

merged_siphonaptera <- rbindlist(list(Lewis_Siphonaptera, merged_not_in_Lewis), fill = TRUE) # add NMNH terms not in Lewis to Lewis
merged_siphonaptera$taxonID <- paste(merged_siphonaptera$TPTdataset, merged_siphonaptera$TPTID, sep = '')

write.csv(merged_siphonaptera,"~/GitHub/tpt-siphonaptera/output/merged_siphonaptera.csv", row.names = FALSE) # merged file

# review for duplicates
dupe <- merged_siphonaptera[,c('canonicalName')] # select columns to check duplicates
merged_dups <- merged_siphonaptera[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),] # create duplicates data frame

# compare merged file to GBIF
GBIF_Siphonaptera <- read_excel("~/GitHub/tpt-siphonaptera/input/GBIF_Siphonaptera.xlsx") # read in GBIF taxonomy

# review for duplicates
dupe <- GBIF_Siphonaptera[,c('canonicalName')] # select columns to check duplicates
GBIF_dups <- GBIF_Siphonaptera[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),] # create duplicates data frame
GBIF_Siphonaptera <- anti_join(GBIF_Siphonaptera,GBIF_dups, by = c("canonicalName", "taxonRank")) # remove duplicate rows from working file

merged_in_GBIF <- merged_siphonaptera[merged_siphonaptera$canonicalName %in% GBIF_Siphonaptera$canonicalName,] # get all rows in merged with canonical name that matches a row in GBIF

merged_not_in_GBIF <- merged_siphonaptera[merged_siphonaptera$canonicalName %!in% GBIF_Siphonaptera$canonicalName,] # get all rows in merged that do not match a canonical name in GBIF
write.csv(merged_not_in_GBIF, "~/GitHub/tpt-siphonaptera/output/not_in_GBIF_siphonaptera.csv", row.names = FALSE) # names need review)

GBIF_in_merged <- GBIF_Siphonaptera[GBIF_Siphonaptera$canonicalName %in% merged_siphonaptera$canonicalName,] # get all rows in GBIF with canonical name that matches a row in merged

GBIF_not_in_merged <- GBIF_Siphonaptera[GBIF_Siphonaptera$canonicalName %!in% merged_siphonaptera$canonicalName,] # get all rows in GBIF that do not match a canonical name in merged
write.csv(GBIF_not_in_merged, "~/GitHub/tpt-siphonaptera/output/GBIF_not_in_siphonaptera.csv", row.names = FALSE) # names need review)

Siphonaptera <- rbindlist(list(GBIF_in_merged, merged_not_in_GBIF), fill = TRUE) # add GBIF names not in merged to merged
