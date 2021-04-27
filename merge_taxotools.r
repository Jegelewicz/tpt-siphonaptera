# merge files for taxotools report

# read in files
NMNH <- read.csv("~/GitHub/tpt-siphonaptera/output/NMNH_DwC.csv", na = "NA") # read in cleaned NMNH review file # NMNH higher taxa
NMNH_species <- species(NMNH, NMNH$taxonRank) # NMNH species taxa
NMNH_species$taxonomicStatus <- NULL
NMNH_taxo <- DwC2taxo(NMNH_species, source = "NMNH") # transform to taxotool format

FMNH <- read.csv("~/GitHub/tpt-siphonaptera/output/FMNH_DwC.csv", na = "NA") # read in cleaned FMNH review file
FMNH_ht <- higer_taxa(FMNH, FMNH$taxonRank) # FMNH higher taxa
FMNH_species <- species(FMNH, FMNH$taxonRank) # FMNH species taxa
FMNH_species$taxonomicStatus <- NULL
FMNH_taxo <- DwC2taxo(FMNH_species, source = "FMNH")

Lewis <- read.csv("~/GitHub/tpt-siphonaptera/output/Lewis_DwC.csv", na = "NA") # read in cleaned Lewis review file
Lewis_ht <- higer_taxa(Lewis, Lewis$taxonRank) # FMNH higher taxa
Lewis_species <- species(Lewis, Lewis$taxonRank) # FMNH species taxa
Lewis_taxo <- DwC2taxo(Lewis_species)

CoL <- read.csv("~/GitHub/tpt-siphonaptera/output/CoL_DwC.csv", na = "NA") # read in cleaned Lewis review file
CoL_ht <- higer_taxa(CoL, CoL$taxonRank) # CoL higher taxa
CoL_species <- species(CoL, CoL$taxonRank) # CoL species taxa
Col_taxo <- DwC2taxo(CoL_DwC)

dwc_siphonaptera <- rbindlist(list(NMNH_Siphonaptera, FMNH_Siphonaptera, Lewis_Siphonaptera, CoL_DwC), fill = TRUE) # combine all DarwinCore files

taxo_siphonaptera <- dwc_siphonaptera

colnames(taxo_siphonaptera) <- as.character(df1[,2])

taxo_siphonaptera <- rename(taxo_siphonaptera,
                   replace = c("id" = "taxonID",
                               "source" = "TPTdataset",
                               "accid" = "acceptedNameUsageID",
                               "species" = "specificEpithet",
                               "subspecies" = "infraspecificEpithet",
                               "author" = "scientificNameAuthorship",
                               "taxonlevel" = "taxonRank"))
