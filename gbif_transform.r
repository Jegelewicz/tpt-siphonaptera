# GBIF taxo conversion
GBIF <- read_excel("~/GitHub/tpt-siphonaptera/input/GBIF_Flea_new.xlsx") # read in GBIF file
GBIF_origin <- GBIF # keep original file for sanity check

tpt_dwc_template <- read_excel("input/tpt_dwc_template.xlsx") # read in TPT DarwinCore template
tpt_dwc_template[] <- lapply(tpt_dwc_template, as.character) # set all columns in template to character
GBIF <- rbindlist(list(GBIF, tpt_dwc_template), fill = TRUE) # add all DwC columns

# ensure no NA in taxonomicStatus
for (i in 1:nrow(GBIF)){
  if (is.na(GBIF$taxonomicStatus[i])) {
    GBIF$taxonomicStatus[i] <- "doubtful"
  } else {
    GBIF$taxonomicStatus[i] <- GBIF$taxonomicStatus[i]
  }
}

# add dataset for accepted taxa without one
for (i in 1:nrow(GBIF)){
  if (is.na(GBIF$datasetID[i]) & GBIF$taxonomicStatus[i] == "accepted") {
      GBIF$datasetID[i] <- "TPT"
    } else {
      GBIF$datasetID[i] <- GBIF$datasetID[i]
    }
}

GBIF_nodataset <- GBIF[which(is.na(GBIF$datasetID)),] # get taxa with no dataset ID
GBIF_nodataset$reason <- "no dataset"
GBIF <- GBIF[which(!is.na(GBIF$datasetID)),] # remove rows with no dataset ID
GBIF_misapplied <- GBIF[which(GBIF$taxonomicStatus == "misapplied"),] # get taxa with taxonomic status of misapplied - what does that mean?
GBIF_misapplied$reason <- "misapplied"
GBIF <- GBIF[which(GBIF$taxonID %!in% GBIF_misapplied$taxonID),] # remove rows with taxonomicStatus = misapplied - don't know how to treat
# check for duplicate names 
GBIF$reason <- c(ifelse(duplicated(GBIF$scientificName, fromLast = TRUE)  | duplicated(GBIF$scientificName),
                              "duplicate", NA)) # Flag internal dupes
GBIF_dupes_review <- GBIF[which(grepl('duplicate',GBIF$reason) == TRUE), ]  # get duplicates for review
GBIF <- GBIF[which(grepl('duplicate',GBIF$reason) == FALSE), ] # remove all dupes from working file
GBIF_dupes_keep <- GBIF_dupes_review[which(!is.na(GBIF_dupes_review$acceptedNameUsageID)),]
GBIF_dupes <- GBIF_dupes_review[which(is.na(GBIF_dupes_review$acceptedNameUsageID)),]
GBIF <- rbind(GBIF, GBIF_dupes_keep)
GBIF_removed <- rbindlist(list(GBIF_dupes, GBIF_misapplied, GBIF_nodataset), fill = TRUE)
GBIF$source <- "GBIF"

# Do this after final review...
GBIF_non_dwc <- subset(GBIF, select = c(source, taxonID, COVERAGE, datasetID, genericName, reason)) # get all columns that are not DwC
# remove non DwC columns from working file
GBIF$COVERAGE <- NULL
GBIF$datasetID <- NULL
GBIF$genericName <- NULL
GBIF$reason <- NULL
# add subfamily column for consistency
GBIF$subfamily <- NA

# order column names
# df[,c(1,2,3,4)]. Note the first comma means keep all the rows, and the 1,2,3,4 refers to the columns.
GBIF <- GBIF[,c("source",
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
            "subfamily",
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

# sanity check
original <- nrow(GBIF_origin) # number of rows in cleaned file
final <- nrow(GBIF) + nrow(GBIF_removed) # number of rows in converted taxo file plus number of rows in higher taxa
if(original == final) { 
  write.csv(GBIF,"~/GitHub/tpt-siphonaptera/output/GBIF_DwC.csv", row.names = FALSE) # write out transformed GBIF DwC
  write.csv(GBIF_removed,"~/GitHub/tpt-siphonaptera/output/GBIF_removed.csv", row.names = FALSE) # write out removed rows
  write.csv(GBIF_non_dwc,"~/GitHub/tpt-siphonaptera/output/GBIF_non_DwC.csv", row.names = FALSE) # write out removed rows  
  print("YAY")
} else {
  print("rows are missing")
  }
