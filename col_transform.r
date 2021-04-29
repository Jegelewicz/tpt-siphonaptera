# Catalog of Life transform
# read in file
CoL_Siphonaptera <- read_excel("~/GitHub/tpt-siphonaptera/input/CoL_DwC.xlsx", )
df <- CoL_Siphonaptera # change filename for ease of use

original_rows <- nrow(df)

# transform column headers
colnames(df) <- convert2DwC(colnames(df)) # convert to DarwinCore terms
colnames(df) <- gsub("dwc:","",colnames(df)) # get rid of all "dwc:" prefixes in column names
colnames(df)[colnames(df) == "gbif:genericName"] <- "genus" # change weird column name to DwC
tpt_dwc_template <- read_excel("input/tpt_dwc_template.xlsx") # read in TPT DarwinCore template
tpt_dwc_template[] <- lapply(tpt_dwc_template, as.character) # set all columns in template to character
df <- rbindlist(list(df, tpt_dwc_template), fill = TRUE) # add all DwC columns

df$source <- "CoL" # set source to CoL
df$taxonomicStatus <- tolower(df$taxonomicStatus) # make taxonomic status lowercase
df$taxonRank <- tolower(df$taxonRank) # make taxon rank lowercase
df$taxonRank <- ifelse(df$taxonRank == "infraspecific_name", "subspecies", df$taxonRank) # change rank name for subspecies

df$kingdom <- "Animalia" # add kingdom
df$phylum <- "Arthropoda" # add phylum
df$class <- "Insecta" # add class
df$order <- "Siphonaptera" # add order
df$genus <- ifelse(df$taxonRank == "genus", df$scientificName, df$genus)

# get family names for families
df$family <- ifelse(df$taxonRank == "family", df$scientificName, NA) # for families, put scientitifc name in family

# get family names for genera
families <- df[which(df$taxonRank == "family"),] # get the families
df$family <- ifelse(is.na(df$family),vlookup(families$scientificName, df$parentNameUsageID, families$taxonID), df$family) # match parentNameUsageID to taxonID in families to get the family name

# get family names for species
genera <- df[which(df$taxonRank == "genus"),] # get the genera
df$family <- ifelse(is.na(df$family),vlookup(genera$family, df$parentNameUsageID, genera$taxonID), df$family) # match parentNameUsageID to taxonID in genera to get the family name

# get family names for subspecies
species <- df[which(df$taxonRank == "species"),] # get the species
df$family <- ifelse(is.na(df$family),vlookup(species$family, df$parentNameUsageID, species$taxonID), df$family) # match parentNameUsageID to taxonID in genera to get the family name

# get family names for synonyms
accepted <- df[which(df$taxonomicStatus == "accepted"),] # get the accepted names
df$family <- ifelse(is.na(df$family),vlookup(genera$family, df$genus, genera$genus), df$family) # match parentNameUsageID to taxonID in genera to get the family name

# fix known issue
df$family <- ifelse(is.na(df$family),
                    ifelse(df$genus == "Ceratopsyllus",
                           "Ceratophyllidae",
                           NA),
                    df$family) # add family for missing genus 

# clean up
df <- char_fun(df,phrase_clean) # remove \xa0 characters
df <- char_fun(df,trimws) # trim white space
df <- char_fun(df,space_clean) # remove double spaces

# extract higher taxa for next set of review
higher_taxa <- higher_taxa_epithet(df,df$specificEpithet,df$infraspecificEpithet) # create dataframe of higher taxa
df <- species_epithet(df,df$infraspecificEpithet, df$specificEpithet) # remove higher taxa from working file

# generate canonical name for species and below
df <- cast_canonical(df,
                     canonical="canonicalName", 
                     genus = "genus", 
                     species = "specificEpithet",
                     subspecies = "infraspecificEpithet")

# canonical names for taxa ranked subgenus and above - get the lowest ranking term and put it here!
for(i in 1:nrow(higher_taxa)){
  higher_taxa$canonicalName[i] <- ifelse(!is.na(higher_taxa$subgenus[i]), paste(higher_taxa$subgenus[i]),
                                         ifelse(!is.na(higher_taxa$genus[i]), paste(higher_taxa$genus[i]),
                                                ifelse(!is.na(higher_taxa$family[i]), paste(higher_taxa$family[i]),
                                                       ifelse(!is.na(higher_taxa$subfamily[i]), paste(higher_taxa$subfamily[i]),
                                                              "review"))))
}

df <- rbind(higher_taxa, df) # add higher taxa back to df for remainder of de-duplication

# order column names
#df[,c(1,2,3,4)]. Note the first comma means keep all the rows, and the 1,2,3,4 refers to the columns.
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

write.csv(df,"~/GitHub/tpt-siphonaptera/output/CoL_DwC.csv", row.names = FALSE) # ready for analysis
