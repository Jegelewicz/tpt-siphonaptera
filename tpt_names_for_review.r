# define function: name length
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)
# define function: is not in
'%!in%' <- function(x,y)!('%in%'(x,y))

# extract higher taxa
higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),]

# extract rows with missing information to review file
missing_genus <- df[which(lapply(df$specificEpithet, name_length) != 0 & lapply(df$genus, name_length) == 0),] # select rows with no genus, but has species
missing_genus$reason <- "missing genus" # add review reason column
df <- df[which(lapply(df$genus, name_length) != 0),] # extract rows with no genus, but has species from working dataframe
missing_species <- df[which(lapply(df$specificEpithet, name_length) == 0 & lapply(df$infraspecificEpithet, name_length) != 0),] # extract rows with no species, but has subspecies
missing_species$reason <- "missing specificEpithet" # add review reason column
df_review <- rbind(missing_genus, missing_species) # combine extracted rows that are missing terms to df_review data frame
missing_sciname <- df[which(lapply(df$scientificName, name_length) == 0 & lapply(df$infraspecificEpithet, name_length) != 0),] # extract rows with no scientificName
missing_sciname$reason <- "missing scientificName" # add review reason column
df_review <- rbind(df_review, missing_sciname) # combine extracted rows that are missing terms to df_review data frame
# retain only rows with complete information in working file
df <- df[which(lapply(df$specificEpithet, name_length) != 0 &
                 lapply(df$infraspecificEpithet, name_length) != 0 | 
                 lapply(df$specificEpithet, name_length) != 0 & lapply(df$infraspecificEpithet, name_length) == 0), ] # extract rows with no species, but has subspecies from working dataframe
df <- df[which(lapply(df$scientificName, name_length) != 0), ] # extract rows with no scientificName from working dataframe
df <- rbind(higher_taxa, df) # add higher taxa back to working data frame

# Missing data check 1
if(TOTAL != nrow(df) + 
   nrow(df_review)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
  } else {
    print('all records accounted for')
  } # End of Missing data check 1

# extract rows with unexpected data to review file
multi_epithet <- df[which(lapply(df$specificEpithet, name_length) > 1 | lapply(df$genus, name_length) > 1 | lapply(df$infraspecificEpithet, name_length) > 1),] # extract rows with a multi-name genus, specificEpithet OR infraspecificEpithet
multi_epithet$reason <- "multi term genus, specificEpithet or infraspecificEpithet" # add review reason column
df_review <- rbind(df_review, multi_epithet)
# retain only single term genus, species and subspecies rows in working file
df <- df[which(lapply(df$specificEpithet, name_length) <= 1),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) <= 1),]
df <- df[which(lapply(df$genus, name_length) <= 1),]

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
df_review <- rbind(df_review, removed_sp, removed_spp) # add extracted records to df_review
df <- df[which(df$specificEpithet %!in% sp_wildcards), ] # remove extracted spcificEpithet records from df
df <- df[which(df$infraspecificEpithet %!in% sp_wildcards), ] # remove extracted infraspecificEpithet records from df

# # extract names containing punctuation - this was removed earlier
# # TODO we should check ALL names for punctuation
# punctuated_species <- df[which(lapply(df$genus, containsPunc) == TRUE |
#                                  lapply(df$specificEpithet, containsPunc) == TRUE |
#                                  lapply(df$infraspecificEpithet, containsPunc) == TRUE),]
# 
# punctuated_species$reason <- "contains punctuation" # add review reason column
# df_review <- rbind(df_review, punctuated_species) # add extracted names to df_review
# # remove punctuated names from df
# df <- df[which(lapply(df$genus, containsPunc) == FALSE &
#                  lapply(df$specificEpithet, containsPunc) == FALSE &
#                  lapply(df$infraspecificEpithet, containsPunc) == FALSE),]

# extract higher taxa for next set of review
higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),]

# Pull out short names
short_genus <- df[which(lapply(df$genus, nchar) < 4),] # extract very short genus
short_genus$reason <- "short genus" # add review reason column
df <- df[which(lapply(df$genus, nchar) >= 4),] # remove short genera from df
df_review <- rbind(df_review, short_genus) # add extracted rows to df_review
short_specific <- df[which(lapply(df$specificEpithet, nchar) < 4),] # extract very short specificEpithet
short_specific$reason <- "short specificEpithet" # add review reason column
df <- df[which(lapply(df$specificEpithet, nchar) >= 4),] # remove short specificEpithets from df
df_review <- rbind(df_review, short_specific) # add extracted rows to df_review
short_infra <- df[which(lapply(df$infraspecificEpithet, nchar) != 0 & 
                          lapply(df$infraspecificEpithet, nchar) < 4),] # extract very short infraspecificEpithet
short_infra$reason <- "short infraspecificEpithet" # add review reason column
df <- df[which(lapply(df$infraspecificEpithet, nchar) == 0 | 
                 is.na(df$infraspecificEpithet) | 
                 lapply(df$infraspecificEpithet, nchar) >= 4),] # remove short infraspecificEpithets from df
df_review <- rbind(df_review, short_infra) # add extracted rows to df_review
df <- rbind(df, higher_taxa) # add higher taxa back to working file

# Missing data check 2
if(TOTAL != nrow(df) + 
   nrow(df_review)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  write.csv(df_review,"~/GitHub/ixodes-tpt/output/taxa_need_review.csv", row.names = FALSE) # these need review
  print('all records accounted for, review records in df_review, make changes, remove reason column and save in input folder.
        If any rows should be removed, save them in the input folder as df_removed and proceed to canonical name add')
} # End of Missing data check 2