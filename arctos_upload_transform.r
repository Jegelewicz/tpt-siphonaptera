# this script will convert a DarwinCore taxonomy file to an Arctos hierarchical taxonomy upload file
# get publication from GBIF and add as remark

# add libraries
library(readxl)

tpt_dwc_template <- read_excel("input/tpt_dwc_template.xlsx") # read in TPT DarwinCore template
tpt_dwc_template[] <- lapply(tpt_dwc_template, as.character) # set all columns in template to character
df <- rbindlist(list(GBIF_in_merged, tpt_dwc_template), fill = TRUE) # combine DwC file with file to transform
df <- rbindlist(list(merged_in_GBIF, tpt_dwc_template), fill = TRUE) # combine DwC file with file to transform

names(df)[names(df) == 'canonicalName'] <- 'scientific_name' #change canonicalName to scientific_name

names(df)[names(df) == 'taxonRank'] <- 'name_rank' #change taxonRank to taxon_rank

# get parent name if not supplied
for(i in 1:nrow(df)){
  df$parentNameUsage[i] <- ifelse(!is.na(df$parentNameUsage[i]), df$parentNameUsage, # use parentNameUsage is available
                              ifelse(is.na(df$name_rank[i]), NA, # if both parentNameUsage and taxon_rank are blank, insert NA
                                     ifelse(df$name_rank[i] == "subspecies", df$specificEpithet,   
                                            ifelse(df$name_rank[i] == "species", ifelse(is.na(df$subgenus), df$genus[i], df$subgenus[i]) ,# if name rank is species and there is no subgenus, insert genus, otherwise insert genus
                                                   ifelse(df$name_rank[i] == "subgenus", df$genus[i], # if name rank is species, insert genus is not, insert year
                                                          ifelse(df$name_rank[i] == "genus", df$family[i], #if name rank is genus, insert family
                                                                 ifelse(df$name_rank[i] == "family", df$order, #if name rank is family, insert order
                                                                        NA)) # otherwise NA
                                                   )
                                            )
                                     )
                                     
                              )
  )
}

names(df)[names(df) == 'parentNameUsage'] <- 'parent_name'

df$username <- "jegelewicz" # fill in username

df$hierarchy_name <- "Siphonaptera" # fill in hierarchy name

df$noclass_term_type_1 <- "author_text" # set first non classification term to author_text
names(df)[names(df) == 'scientificNameAuthorship'] <- 'noclass_term_1' # change column with scientifcNameAuthorship to no_class_term_1

names(df)[names(df) == 'nomenclaturalCode'] <- 'noclass_term_2' # change column with nomenclaturalCode to no_class_term_2
df$noclass_term_2 <- ifelse(is.na(df$noclass_term_2), 'ICZN', df$noclass_term_2) # set all nomenclatural code terms
df$noclass_term_type_2 <- ifelse(!is.na(df$noclass_term_2), 'nomenclatural_code', NA) # set second non classification term type to nomenclatural_code

df$noclass_term_type_3 <- 'source_authority' # set third non classification term to source_authority
df$noclass_term_3 <- 'Terrestrial Parasite Tracker Thematic Collection Network' # set third non classification term to Terrestrial Parasite Tracker Thematic Collection Network

names(df)[names(df) == 'taxonRemarks'] <- 'noclass_term_4' # change column with taxonRemark to no_class_term_4
df$noclass_term_type_4 <- ifelse(!is.na(df$noclass_term_4), 'taxon_remark', NA) #set fourth non classification term to taxon_remark if no_class_term_4 is not NA

names(df)[names(df) == 'taxonomicStatus'] <- 'noclass_term_5' # change column with  to no_class_term_5
df$noclass_term_type_5 <- ifelse(!is.na(df$noclass_term_5), 'taxon_status', NA) #set fourth non classification term to taxon_status if no_class_term_5 is not NA
df$noclass_term_5 <- ifelse(df$noclass_term_5 == 'accepted', 'valid', df$noclass_term_5) # adjust to Arctos code table terms

names(df)[names(df) == 'acceptedNameUsage'] <- 'noclass_term_6' # change accepteNameUsage to preferred_name
df$noclass_term_type_6 <- ifelse(!is.na(df$noclass_term_6), 'preferred_name', NA) #set fourth non classification term to preferred_name if no_class_term_6 is not NA

# df$noclass_term_type_7
# df$noclass_term_7 

# order column names
# df[,c(1,2,3,4)]. Note the first comma means keep all the rows, and the 1,2,3,4 refers to the columns.
columns <- df[, colnames(df)[c(grepl("^noclass_term", colnames(df)))]]
df1 <- subset(df, select = grep("^noclass_term", names(df)))
Arctos_upload <- df[,c("username",
                       "hierarchy_name",
                       "scientific_name",
                       "name_rank",
                       "parent_name",
                       "noclass_term_type_1",
                       "noclass_term_1",
                       "noclass_term_type_2",
                       "noclass_term_2",
                       "noclass_term_type_3",
                       "noclass_term_3",
                       "noclass_term_type_4",
                       "noclass_term_4",
                       "noclass_term_type_5",
                       "noclass_term_5",
                       "noclass_term_type_6",
                       "noclass_term_6"
                       )
]
