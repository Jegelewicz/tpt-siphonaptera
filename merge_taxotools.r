# merge files for taxotools report

# NMNH taxo conversion
NMNH <- read.csv("~/GitHub/tpt-siphonaptera/output/NMNH_DwC.csv", na = "NA") # read in cleaned NMNH review file
NMNH$taxonomicStatus <- NULL # NMNH does not contain status, so NULL it
NMNH_ht <- higher_taxa_rank(NMNH, NMNH$taxonRank) # get NMNH higher taxa (taxo DwC removes them, this is to check rows later)
NMNH_taxo <- DwC2taxo(NMNH, source = "NMNH") # transform to taxotool format

# sanity check
original <- nrow(NMNH) # number of rows in cleaned file
final <- nrow(NMNH_taxo) + nrow(NMNH_ht) # number of rows in converted taxo file plus number of rows in higher taxa
if(original == final) { print("yay") # print yay if no rows are missing
   } else {
       NMNH_not_in_taxo <- NMNH[NMNH$taxonID %!in% NMNH_taxo$id,] # get all rows in NMNH that do not match an id in taxo
       NMNH_problems <- NMNH_not_in_taxo[NMNH_not_in_taxo$taxonID %!in% NMNH_ht$taxonID,] # get all rows in above that do not match an id in NMNH_ht
       NMNH_problems$taxonomicStatus <- NULL # status is the most likely issue, so NULL it
       NMNH_problems_taxo <- DwC2taxo(problems, source = "NMNH") # transform problems to taxo format)
       NMNH_taxo <- rbind(NMNH_taxo, NMNH_problems_taxo) # return converted problems to working file
       final <- nrow(NMNH_taxo) + nrow(NMNH_ht) # recalculate number of rows in converted taxo file plus number of rows in higher taxa
       if(original == final) { print("yay") # print yay if no rows are missing
       } else {
         NMNH_ugh <- NMNH_problems[NMNH_problems$taxonID %!in% NMNH_problems_taxo$id,] # get all rows in taxo that do not match an id in problems
         write.csv(NMNH_ugh,"~/GitHub/tpt-siphonaptera/output/NMNH_problems.csv", row.names = FALSE) # write out problems for review
       }
   }

# FMNH taxo conversion
FMNH <- read.csv("~/GitHub/tpt-siphonaptera/output/FMNH_DwC.csv", na = "NA") # read in cleaned FMNH review file
FMNH$taxonomicStatus <- NULL # FMNH does not contain status, so NULL it
FMNH_ht <- higher_taxa_rank(FMNH, FMNH$taxonRank) # get FMNH higher taxa (DwC2taxo removes them, this is to check rows later)
FMNH_taxo <- DwC2taxo(FMNH, source = "FMNH")

# sanity check
original <- nrow(FMNH) # number of rows in cleaned file
final <- nrow(FMNH_taxo) + nrow(FMNH_ht) # number of rows in converted taxo file plus number of rows in higher taxa
if(original == final) { print("yay") # print yay if no rows are missing
} else {
  FMNH_not_in_taxo <- FMNH[FMNH$taxonID %!in% FMNH_taxo$id,] # get all rows in FMNH that do not match an id in taxo
  FMNH_problems <- FMNH_not_in_taxo[FMNH_not_in_taxo$taxonID %!in% FMNH_ht$taxonID,] # get all rows in above that do not match an id in FMNH_ht
  FMNH_problems$taxonomicStatus <- NULL # status is the most likely issue, so NULL it
  FMNH_problems_taxo <- DwC2taxo(problems, source = "FMNH") # transform problems to taxo format)
  FMNH_taxo <- rbind(FMNH_taxo, FMNH_problems_taxo) # return converted problems to working file
  final <- nrow(FMNH_taxo) + nrow(FMNH_ht) # recalculate number of rows in converted taxo file plus number of rows in higher taxa
  if(original == final) { print("yay") # print yay if no rows are missing
  } else {
    FMNH_ugh <- FMNH_problems[FMNH_problems$taxonID %!in% FMNH_problems_taxo$id,] # get all rows in taxo that do not match an id in problems
    write.csv(FMNH_ugh,"~/GitHub/tpt-siphonaptera/output/FMNH_problems.csv", row.names = FALSE) # write out problems for review
  }
}

# Lewis taxo conversion
Lewis <- read.csv("~/GitHub/tpt-siphonaptera/output/Lewis_DwC.csv", na = "NA") # read in cleaned Lewis review file
Lewis_ht <- higher_taxa_rank(Lewis, Lewis$taxonRank) # get Lewis higher taxa (DwC2taxo removes them, this is to check rows later)
Lewis_taxo <- DwC2taxo(Lewis, source = "Lewis")

# sanity check
original <- nrow(Lewis) # number of rows in cleaned file
final <- nrow(Lewis_taxo) + nrow(Lewis_ht) # number of rows in converted taxo file plus number of rows in higher taxa
if(original == final) { print("yay") # print yay if no rows are missing
} else {
  Lewis_not_in_taxo <- Lewis[Lewis$taxonID %!in% Lewis_taxo$id,] # get all rows in Lewis that do not match an id in taxo
  Lewis_problems <- Lewis_not_in_taxo[Lewis_not_in_taxo$taxonID %!in% Lewis_ht$taxonID,] # get all rows in above that do not match an id in Lewis_ht
  Lewis_problems$taxonomicStatus <- NULL # status is the most likely issue, so NULL it
  Lewis_problems_taxo <- DwC2taxo(problems, source = "Lewis") # transform problems to taxo format)
  Lewis_taxo <- rbind(Lewis_taxo, Lewis_problems_taxo) # return converted problems to working file
  final <- nrow(Lewis_taxo) + nrow(Lewis_ht) # recalculate number of rows in converted taxo file plus number of rows in higher taxa
  if(original == final) { print("yay") # print yay if no rows are missing
  } else {
    Lewis_ugh <- Lewis_problems[Lewis_problems$taxonID %!in% Lewis_problems_taxo$id,] # get all rows in taxo that do not match an id in problems
    write.csv(Lewis_ugh,"~/GitHub/tpt-siphonaptera/output/Lewis_problems.csv", row.names = FALSE) # write out problems for review
  }
}

# CoL taxo conversion
CoL <- read.csv("~/GitHub/tpt-siphonaptera/output/CoL_DwC.csv", na = "NA") # read in cleaned CoL review file
CoL$taxonomicStatus <- ifelse(CoL$taxonomicStatus == "ambiguous_synonym", "synonym", CoL$taxonomicStatus) # replace non-conforming status
CoL_ht <- higher_taxa_rank(CoL, CoL$taxonRank) # get CoL higher taxa (DwC2taxo removes them, this is to check rows later)
CoL <- compact_ids(CoL,id="taxonID",accid="acceptedNameUsageID") # deal with letters and long ids
CoL_taxo <- DwC2taxo(CoL, source = "CoL")

# sanity check
original <- nrow(CoL) # number of rows in cleaned file
final <- nrow(CoL_taxo) + nrow(CoL_ht) # number of rows in converted taxo file plus number of rows in higher taxa
if(original == final) { print("yay") # print yay if no rows are missing
} else {
  CoL_not_in_taxo <- CoL[CoL$taxonID %!in% CoL_taxo$id,] # get all rows in CoL that do not match an id in taxo
  CoL_problems <- CoL_not_in_taxo[CoL_not_in_taxo$taxonID %!in% CoL_ht$taxonID,] # get all rows in above that do not match an id in CoL_ht
  CoL_problems$taxonomicStatus <- NULL # status is the most likely issue, so NULL it
  CoL_problems_taxo <- DwC2taxo(problems, source = "CoL") # transform problems to taxo format)
  CoL_taxo <- rbind(CoL_taxo, CoL_problems_taxo) # return converted problems to working file
  final <- nrow(CoL_taxo) + nrow(CoL_ht) # recalculate number of rows in converted taxo file plus number of rows in higher taxa
  if(original == final) { print("yay") # print yay if no rows are missing
  } else {
    CoL_ugh <- CoL_problems[CoL_problems$taxonID %!in% CoL_problems_taxo$id,] # get all rows in taxo that do not match an id in problems
    write.csv(CoL_ugh,"~/GitHub/tpt-siphonaptera/output/CoL_problems.csv", row.names = FALSE) # write out problems for review
  }
}

# GBIF taxo conversion
GBIF <- read_excel("~/GitHub/tpt-siphonaptera/input/GBIF_Siphonaptera.xlsx") # read in GBIF file
GBIF$taxonomicStatus <- ifelse(GBIF$taxonomicStatus == "homotypic synonym", "homotypicSynonym",
                               ifelse(GBIF$taxonomicStatus == "heterotypic synonym", "heterotypicSynonym", GBIF$taxonomicStatus)) # replace non-conforming status



GBIF_ht <- higher_taxa_rank(GBIF, GBIF$taxonRank) # get GBIF higher taxa (DwC2taxo removes them, this is to check rows later)
GBIF_taxo <- DwC2taxo(GBIF, source = "GBIF") # transform to taxo format

# sanity check
original <- nrow(GBIF) # number of rows in cleaned file
final <- nrow(GBIF_taxo) + nrow(GBIF_ht) # number of rows in converted taxo file plus number of rows in higher taxa
if(original == final) { print("yay") # print yay if no rows are missing
} else {
  GBIF_not_in_taxo <- GBIF[GBIF$taxonID %!in% GBIF_taxo$id,] # get all rows in GBIF that do not match an id in taxo
  GBIF_problems <- GBIF_not_in_taxo[GBIF_not_in_taxo$taxonID %!in% GBIF_ht$taxonID,] # get all rows in above that do not match an id in GBIF_ht
  GBIF_problems$taxonomicStatus <- NULL # status is the most likely issue, so NULL it
  GBIF_problems_taxo <- DwC2taxo(problems, source = "GBIF") # transform problems to taxo format)
  GBIF_taxo <- rbind(GBIF_taxo, GBIF_problems_taxo) # return converted problems to working file
  final <- nrow(GBIF_taxo) + nrow(GBIF_ht) # recalculate number of rows in converted taxo file plus number of rows in higher taxa
  if(original == final) { print("yay") # print yay if no rows are missing
  } else {
    GBIF_ugh <- GBIF_problems[GBIF_problems$taxonID %!in% GBIF_problems_taxo$id,] # get all rows in taxo that do not match an id in problems
    write.csv(GBIF_ugh,"~/GitHub/tpt-siphonaptera/output/GBIF_problems.csv", row.names = FALSE) # write out problems for review
  }
}

# Flea_merge
Flea_m1 <- merge_lists(Lewis_taxo, CoL_taxo) # master is Lewis, merging with CoL
Flea_mast1 <- rbind.fill(Lewis_taxo,Flea_m1$addlist,Flea_m1$noaddlist)
Flea_mast1_1 <- cast_cs_field(Flea_mast1,"canonical","source") # merge lists, remove dupes, add CoL as source when duped with Lewis
Flea_m2 <- merge_lists(Flea_mast1_1, NMNH_taxo) # merge NMNH with working master
Flea_mast2 <- rbind.fill(Flea_mast1_1,Flea_m2$addlist,Flea_m2$noaddlist)
Flea_mast2_1 <- cast_cs_field(Flea_mast2,"canonical","source") # merge lists, remove dupes, add NMNH as source when duped with working file
Flea_m3 <-  merge_lists(Flea_mast2_1,FMNH_taxo) # merge FMNH with working master
Flea_mast3 <- rbind.fill(Flea_mast2_1,Flea_m3$addlist,Flea_m3$noaddlist)
Flea_mast3_1 <- cast_cs_field(Flea_mast3,"canonical","source") # merge lists, remove dupes, add FMNH as source when duped with working file
Flea_m4 <-  merge_lists(Flea_mast3_1,GBIF_taxo) # merge FMNH with working master
Flea_mast4 <- rbind.fill(Flea_mast3_1,Flea_m4$addlist,Flea_m4$noaddlist)
Flea_mast4_1 <- cast_cs_field(Flea_mast4,"canonical","source") # merge lists, remove dupes, add GBIF as source when duped with working file

write.csv(Flea_mast4_1,"~/GitHub/tpt-siphonaptera/output/taxo_Siphonaptera.csv", row.names = FALSE)

# sanity check
taxo_siphonaptera <- rbindlist(list(NMNH_taxo, FMNH_taxo, Lewis_taxo, CoL_taxo, GBIF_taxo), fill = TRUE) # combine all taxo files
siphonaptera_ht <- rbindlist(list(NMNH_ht, FMNH_ht, Lewis_ht, CoL_ht, GBIF_ht), fill = TRUE) # combine all ht files
original <- nrow(CoL) + nrow(Lewis) + nrow(FMNH) + nrow(NMNH) + nrow(GBIF) # get original number of rows in cleaned files
final <- nrow(taxo_siphonaptera) + nrow(siphonaptera_ht) # get final number of rows in converted taxo files and add to rows in higher taxa files
ifelse(original == final, write.csv(Flea_mast4_1,"~/GitHub/tpt-siphonaptera/output/taxo_Siphonaptera.csv", row.names = FALSE), # if no rows are missing write taxo file
 print("there are rows missing")) # if rows are missing, print error
