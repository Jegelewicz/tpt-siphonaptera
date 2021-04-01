# Look for missing higher taxa
TOTAL <- nrow(df)

# define function: name length
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)
# define function: is not in
'%!in%' <- function(x,y)!('%in%'(x,y))

# extract higher taxa for next set of review
higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),]

# TODO can we streamline this process and make sure it repeats for all columns in higher_taxa?
suggested_adds <- data.frame (taxonName  = c(),
                              taxonRank = c()) # Suggested additions to the names list
suggested_remove <- data.frame (taxonName  = c(),
                            taxonRank = c()) # Suggested removals from the names list

# genus
# Get unique list of genera from df and higher_taxa
unique_df <- unique(df$genus) # unique genera in the working data frame
unique_ht <- unique(higher_taxa$genus) # unique genera in the higher taxa data frame
unique_ht <- stri_remove_empty(unique_ht, na_empty = FALSE) # remove empty values from higher_taxa
# compare the two lists and return any names in the working data frame, but not in higher_taxa. These are higher level taxa that we suggest adding.
no_higher_taxon <- unique_df[unique_df %!in% unique_ht] # there is not a unique genus entry for these genera
taxa_not_used <- unique_ht[unique_ht %!in% unique_df] # there is a genus taxon, but no species for it
suggested_append <- as.data.frame(no_higher_taxon) # create data frame for suggested adds to missing_taxa
suggested_append$taxonRank <- "genus" # add reason
suggested_adds <- rbind(suggested_adds,suggested_append) # add rows to suggested_adds data frame
unused_taxa <- as.data.frame(taxa_not_used) # create data frame for suggested removals
unused_taxa$taxonRank <- "genus" # add reason
suggested_remove <- rbind(suggested_remove, unused_taxa) # add rows to missing_taxa data frame
# End genus
# Repeat for other levels up....

# family
# Get unique list of families from df and higher_taxa
unique_df <- unique(df$family) # unique genera in the working data frame
unique_ht <- unique(higher_taxa$family) # unique genera in the higher taxa data frame
unique_ht <- stri_remove_empty(unique_ht, na_empty = FALSE) # remove empty values from higher_taxa
# compare the two lists and return any names in the working data frame, but not in higher_taxa. These are higher level taxa that we suggest adding.
no_higher_taxon <- unique_df[unique_df %!in% unique_ht] # there is not a unique family entry for these genera
taxa_not_used <- unique_ht[unique_ht %!in% unique_df] # there is a family taxon, but no species for it
suggested_append <- as.data.frame(no_higher_taxon) # create data frame for suggested adds to missing_taxa
suggested_append$taxonRank <- "family" # add reason
suggested_adds <- rbind(suggested_adds,suggested_append) # add rows to suggested_adds data frame
unused_taxa <- as.data.frame(taxa_not_used) # create data frame for suggested removals
unused_taxa$taxonRank <- "family" # add reason
suggested_remove <- rbind(suggested_remove, unused_taxa) # add rows to missing_taxa data frame
# End family

# subfamily
# Get unique list of families from df and higher_taxa
unique_df <- unique(df$subfamily) # unique genera in the working data frame
unique_ht <- unique(higher_taxa$subfamily) # unique genera in the higher taxa data frame
unique_ht <- stri_remove_empty(unique_ht, na_empty = FALSE) # remove empty values from higher_taxa
# compare the two lists and return any names in the working data frame, but not in higher_taxa. These are higher level taxa that we suggest adding.
no_higher_taxon <- unique_df[unique_df %!in% unique_ht] # there is not a unique subfamily entry for these genera
taxa_not_used <- unique_ht[unique_ht %!in% unique_df] # there is a subfamily taxon, but no species for it
suggested_append <- as.data.frame(no_higher_taxon) # create data frame for suggested adds to missing_taxa
suggested_append$taxonRank <- "subfamily" # add reason
suggested_adds <- rbind(suggested_adds,suggested_append) # add rows to suggested_adds data frame
unused_taxa <- as.data.frame(taxa_not_used) # create data frame for suggested removals
unused_taxa$taxonRank <- "subfamily" # add reason
suggested_remove <- rbind(suggested_remove, unused_taxa) # add rows to missing_taxa data frame
# End subfamily

# order
# Get unique list of families from df and higher_taxa
unique_df <- unique(df$order) # unique genera in the working data frame
unique_ht <- unique(higher_taxa$order) # unique genera in the higher taxa data frame
unique_ht <- stri_remove_empty(unique_ht, na_empty = FALSE) # remove empty values from higher_taxa
# compare the two lists and return any names in the working data frame, but not in higher_taxa. These are higher level taxa that we suggest adding.
no_higher_taxon <- unique_df[unique_df %!in% unique_ht] # there is not a unique order entry for these genera
taxa_not_used <- unique_ht[unique_ht %!in% unique_df] # there is a order taxon, but no species for it
suggested_append <- as.data.frame(no_higher_taxon) # create data frame for suggested adds to missing_taxa
suggested_append$taxonRank <- "order" # add reason
suggested_adds <- rbind(suggested_adds,suggested_append) # add rows to suggested_adds data frame
unused_taxa <- as.data.frame(taxa_not_used) # create data frame for suggested removals
unused_taxa$taxonRank <- "order" # add reason
suggested_remove <- rbind(suggested_remove, unused_taxa) # add rows to missing_taxa data frame
# End order

# class
# Get unique list of families from df and higher_taxa
unique_df <- unique(df$class) # unique genera in the working data frame
unique_ht <- unique(higher_taxa$class) # unique genera in the higher taxa data frame
unique_ht <- stri_remove_empty(unique_ht, na_empty = FALSE) # remove empty values from higher_taxa
# compare the two lists and return any names in the working data frame, but not in higher_taxa. These are higher level taxa that we suggest adding.
no_higher_taxon <- unique_df[unique_df %!in% unique_ht] # there is not a unique class entry for these genera
taxa_not_used <- unique_ht[unique_ht %!in% unique_df] # there is a class taxon, but no species for it
suggested_append <- as.data.frame(no_higher_taxon) # create data frame for suggested adds to missing_taxa
suggested_append$taxonRank <- "class" # add reason
suggested_adds <- rbind(suggested_adds,suggested_append) # add rows to suggested_adds data frame
unused_taxa <- as.data.frame(taxa_not_used) # create data frame for suggested removals
unused_taxa$taxonRank <- "class" # add reason
suggested_remove <- rbind(suggested_remove, unused_taxa) # add rows to missing_taxa data frame
# End class

# infraorder
# Get unique list of infraorders from df and higher_taxa
unique_df <- unique(df$infraorder) # unique genera in the working data frame
unique_ht <- unique(higher_taxa$infraorder) # unique genera in the higher taxa data frame
unique_ht <- stri_remove_empty(unique_ht, na_empty = FALSE) # remove empty values from higher_taxa
# compare the two lists and return any names in the working data frame, but not in higher_taxa. These are higher level taxa that we suggest adding.
no_higher_taxon <- unique_df[unique_df %!in% unique_ht] # there is not a unique infraorder entry for these genera
taxa_not_used <- unique_ht[unique_ht %!in% unique_df] # there is a infraorder taxon, but no species for it
suggested_append <- as.data.frame(no_higher_taxon) # create data frame for suggested adds to missing_taxa
suggested_append$taxonRank <- "infraorder" # add reason
suggested_adds <- rbind(suggested_adds,suggested_append) # add rows to suggested_adds data frame
unused_taxa <- as.data.frame(taxa_not_used) # create data frame for suggested removals
unused_taxa$taxonRank <- "infraorder" # add reason
suggested_remove <- rbind(suggested_remove, unused_taxa) # add rows to missing_taxa data frame
# End infraorder

# superfamily
# Get unique list of superfamilys from df and higher_taxa
unique_df <- unique(df$superfamily) # unique genera in the working data frame
unique_ht <- unique(higher_taxa$superfamily) # unique genera in the higher taxa data frame
unique_ht <- stri_remove_empty(unique_ht, na_empty = FALSE) # remove empty values from higher_taxa
# compare the two lists and return any names in the working data frame, but not in higher_taxa. These are higher level taxa that we suggest adding.
no_higher_taxon <- unique_df[unique_df %!in% unique_ht] # there is not a unique superfamily entry for these genera
taxa_not_used <- unique_ht[unique_ht %!in% unique_df] # there is a superfamily taxon, but no species for it
suggested_append <- as.data.frame(no_higher_taxon) # create data frame for suggested adds to missing_taxa
suggested_append$taxonRank <- "superfamily" # add reason
suggested_adds <- rbind(suggested_adds,suggested_append) # add rows to suggested_adds data frame
unused_taxa <- as.data.frame(taxa_not_used) # create data frame for suggested removals
unused_taxa$taxonRank <- "superfamily" # add reason
suggested_remove <- rbind(suggested_remove, unused_taxa) # add rows to missing_taxa data frame
# End superfamily

# add back higher taxa
df <- rbind(df, higher_taxa)

# Missing data check
if(TOTAL != nrow(df)
   ) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check
