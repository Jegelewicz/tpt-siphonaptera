# define function: is not in
'%!in%' <- function(x,y)!('%in%'(x,y))

# compare FMNH to NMNH
NMNH_Siphonaptera <- read.csv("~/GitHub/tpt-siphonaptera/output/NMNH_DwC.csv", na = "NA") # read in cleaned Lewis review file
FMNH_Siphonaptera <- read.csv("~/GitHub/tpt-siphonaptera/output/FMNH_DwC.csv", na = "NA") # read in cleaned FMNH review file

FMNH_in_NMNH <- FMNH_Siphonaptera[FMNH_Siphonaptera$canonicalName %in% NMNH_Siphonaptera$canonicalName,] # get all rows in FMNH with canonical name that matches a row in Lewis

FMNH_not_in_NMNH <- FMNH_Siphonaptera[FMNH_Siphonaptera$canonicalName %!in% NMNH_Siphonaptera$canonicalName,] # get all rows in FMNH that does not match a canonical name in Lewis

merged_siphonaptera <- rbindlist(list(NMNH_Siphonaptera, FMNH_not_in_NMNH), fill = TRUE) # add FMNH terms not in Lewis to working file

# compare to Lewis list
Lewis_Siphonaptera <- read.csv("~/GitHub/tpt-siphonaptera/output/Lewis_Siphonaptera.csv", na = "NA") # read in cleaned Lewis review file

merged_in_Lewis <- merged_siphonaptera[merged_siphonaptera$canonicalName %in% Lewis_Siphonaptera$canonicalName,] # get all rows in merged with canonical name that matches a row in Lewis

merged_not_in_Lewis <- merged_siphonaptera[merged_siphonaptera$canonicalName %!in% Lewis_Siphonaptera$canonicalName,] # get all rows in merged that do not match a canonical name in Lewis

Lewis_in_merged <- Lewis_Siphonaptera[Lewis_Siphonaptera$canonicalName %in% merged_siphonaptera$canonicalName,] # get all rows in merged with canonical name that matches a row in Lewis

Lewis_not_in_merged <- Lewis_Siphonaptera[Lewis_Siphonaptera$canonicalName %!in% merged_siphonaptera$canonicalName,] # get all rows in merged that do not match a canonical name in Lewis

merged_siphonaptera <- rbindlist(list(Lewis_Siphonaptera, merged_not_in_Lewis), fill = TRUE) # add FMNH terms not in Lewis to working file
merged_siphonaptera$taxonID <- paste(merged_siphonaptera$TPTdataset, merged_siphonaptera$TPTID, sep = '')

write.csv(merged_siphonaptera,"~/GitHub/tpt-siphonaptera/output/merged_siphonaptera.csv", row.names = FALSE) # merged file


# compare merged file to GBIF
GBIF_Siphonaptera <- read_excel("~/GitHub/tpt-siphonaptera/input/GBIF_Siphonaptera.xlsx") # read in GBIF taxonomy

merged_in_GBIF <- merged_siphonaptera[merged_siphonaptera$canonicalName %in% GBIF_Siphonaptera$canonicalName,] # get all rows in merged with canonical name that matches a row in GBIF

merged_not_in_GBIF <- merged_siphonaptera[merged_siphonaptera$canonicalName %!in% GBIF_Siphonaptera$canonicalName,] # get all rows in merged that do not match a canonical name in GBIF

GBIF_in_merged <- GBIF_Siphonaptera[GBIF_Siphonaptera$canonicalName %in% merged_siphonaptera$canonicalName,] # get all rows in GBIF with canonical name that matches a row in merged

GBIF_not_in_merged <- GBIF_Siphonaptera[GBIF_Siphonaptera$canonicalName %!in% merged_siphonaptera$canonicalName,] # get all rows in GBIF that do not match a canonical name in merged

Siphonaptera <- rbindlist(list(merged_siphonaptera, GBIF_not_in_merged), fill = TRUE) # add GBIF names not in merged to merged

# review for duplicates
dupe <- merged_siphonaptera[,c('canonicalName','taxonRank')] # select columns to check duplicates
review_dups <- merged_siphonaptera[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),]
Siphonaptera <- anti_join(Siphonaptera, review_dups, by = "TPTID") # remove duplicate rows from working file

# write and review duplicates
write.csv(review_dups,"~/GitHub/tpt-siphonaptera/output/FMNH_review_duplicates.csv", row.names = FALSE) # these need review
print("after review of duplicates, save return file to ~/GitHub/tpt-siphonaptera/input/reviewed_duplicates.xlsx")
