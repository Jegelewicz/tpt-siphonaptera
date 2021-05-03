# merge files for taxotools report

# read in files
NMNH <- read.csv("~/GitHub/tpt-siphonaptera/output/NMNH_DwC.csv", na = "NA") # read in cleaned NMNH review file # NMNH higher taxa
NMNH$taxonomicStatus <- NULL
# NMNH$taxonomicStatus <- ifelse(is.na(NMNH$taxonomicStatus),"undefined",NMNH$taxonomicStatus) # fill in taxonomic status
NMNH_ht <- higher_taxa_rank(NMNH, NMNH$taxonRank) # NMNH higher taxa
# NMNH_species <- species_rank(NMNH, NMNH$taxonRank) # NMNH species taxa
NMNH_taxo <- DwC2taxo(NMNH, source = "NMNH") # transform to taxotool format

FMNH <- read.csv("~/GitHub/tpt-siphonaptera/output/FMNH_DwC.csv", na = "NA") # read in cleaned FMNH review file
FMNH$taxonomicStatus <- NULL
#FMNH$taxonomicStatus <- ifelse(is.na(FMNH$taxonomicStatus),"undefined",FMNH$taxonomicStatus) # fill in taxonomic status
FMNH_ht <- higher_taxa_rank(FMNH, FMNH$taxonRank) # FMNH higher taxa
# FMNH_species <- species_rank(FMNH, FMNH$taxonRank) # FMNH species taxa
FMNH_taxo <- DwC2taxo(FMNH, source = "FMNH")

Lewis <- read.csv("~/GitHub/tpt-siphonaptera/output/Lewis_DwC.csv", na = "NA") # read in cleaned Lewis review file
Lewis_ht <- higher_taxa_rank(Lewis, Lewis$taxonRank) # FMNH higher taxa
# Lewis_species <- species_rank(Lewis, Lewis$taxonRank) # FMNH species taxa
Lewis_taxo <- DwC2taxo(Lewis, source = "Lewis")

CoL <- read.csv("~/GitHub/tpt-siphonaptera/output/CoL_DwC.csv", na = "NA") # read in cleaned CoL review file
CoL$taxonomicStatus <- ifelse(CoL$taxonomicStatus == "ambiguous_synonym", "synonym", CoL$taxonomicStatus) # replace non-conforming status
CoL_ht <- higher_taxa_rank(CoL, CoL$taxonRank) # CoL higher taxa
# CoL_species <- species_rank(CoL, CoL$taxonRank) # CoL species taxa
CoL <- compact_ids(CoL,id="taxonID",accid="acceptedNameUsageID") # deal with letters and long ids
CoL_taxo <- DwC2taxo(CoL, source = "CoL")

GBIF <- read_excel("~/GitHub/tpt-siphonaptera/input/GBIF_Siphonaptera.xlsx") # read in GBIF file
GBIF$taxonomicStatus <- ifelse(GBIF$taxonomicStatus == "homotypic synonym", "homotypicSynonym",
                               ifelse(GBIF$taxonomicStatus == "heterotypic synonym", "heterotypicSynonym", GBIF$taxonomicStatus)) # replace non-conforming status



GBIF_ht <- higher_taxa_rank(GBIF, GBIF$taxonRank) # GBIF higher taxa
# GBIF_species <- species_rank(GBIF, GBIF$taxonRank) # GBIF species taxa
GBIF_taxo <- DwC2taxo(GBIF, source = "GBIF") # transform to taxo format

taxo_siphonaptera <- rbindlist(list(NMNH_taxo, FMNH_taxo, Lewis_taxo, CoL_taxo, GBIF_taxo), fill = TRUE) # combine all taxo files
siphonaptera_ht <- rbindlist(list(NMNH_ht, FMNH_ht, Lewis_ht, CoL_ht), fill = TRUE) # combine all ht files

#sanity check
original <- nrow(CoL) + nrow(Lewis) + nrow(FMNH) + nrow(NMNH)
final <- nrow(taxo_siphonaptera) + nrow(siphonaptera_ht)
ifelse(original == final, print("yay"),print("ugh"))

# if yay write out the taxo file
write.csv(df,"~/GitHub/tpt-siphonaptera/output/taxo_Siphonaptera.csv", row.names = FALSE) # taxo file

# if ugh, find the problem
GBIF_not_in_taxo <- GBIF[GBIF$taxonID %!in% GBIF_taxo$id,] # get all rows in CoL that do not match an id in taxo
problems <- GBIF_not_in_taxo[GBIF_not_in_taxo$taxonID %!in% GBIF_ht$taxonID,] # get all rows in above that do not match an id in CoL_ht
problems$taxonomicStatus <- NULL
problems_taxo <- DwC2taxo(problems, source = "GBIF") # transform to taxo format
GBIF_taxo <- rbind(GBIF_taxo, problems_taxo)

ugh <- problems[problems$taxonID %!in% problems_taxo$id,] # get all rows in above that do not match an id in CoL_ht
write.csv(ugh,"~/GitHub/tpt-siphonaptera/output/problems.csv", row.names = FALSE)

# Flea_merge
Flea_m1 <- merge_lists(Lewis_taxo, CoL_taxo) # master is Lewis, merging with CoL
Flea_mast1 <- rbind.fill(Lewis_taxo,Flea_m1$addlist,Flea_m1$noaddlist)
Flea_mast1_1 <- cast_cs_field(Flea_mast1,"canonical","source")
Flea_m2 <- merge_lists(Flea_mast1_1, NMNH_taxo) # merge NMNH with working master
Flea_mast2 <- rbind.fill(Flea_mast1_1,Flea_m2$addlist,Flea_m2$noaddlist)
Flea_mast2_1 <- cast_cs_field(Flea_mast2,"canonical","source")
Flea_m3 <-  merge_lists(Flea_mast2_1,FMNH_taxo) # merge FMNH with working master
Flea_mast3 <- rbind.fill(Flea_mast2_1,Flea_m3$addlist,Flea_m3$noaddlist)
Flea_mast3_1 <- cast_cs_field(Flea_mast3,"canonical","source")
Flea_m4 <-  merge_lists(Flea_mast3_1,GBIF_taxo) # merge FMNH with working master
Flea_mast4 <- rbind.fill(Flea_mast3_1,Flea_m4$addlist,Flea_m4$noaddlist)
Flea_mast4_1 <- cast_cs_field(Flea_mast4,"canonical","source")

write.csv(Flea_mast4_1,"~/GitHub/tpt-siphonaptera/output/taxo_Siphonaptera.csv", row.names = FALSE)
