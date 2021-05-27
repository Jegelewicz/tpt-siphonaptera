# merge files for taxotools report

# NMNH taxo conversion
NMNH <- read.csv("~/GitHub/tpt-siphonaptera/output/NMNH_DwC.csv", na = "NA") # read in cleaned NMNH review file
NMNH$taxonomicStatus <- NULL # NMNH does not contain status, so NULL it
NMNH_ht <- higher_taxa_rank(NMNH, NMNH$taxonRank) # get NMNH higher taxa (taxo DwC removes them, this is to check rows later)
NMNH_taxo <- DwC2taxo(NMNH, source = "NMNH") # transform to taxotool format

# sanity check
original <- nrow(NMNH) # number of rows in cleaned file
final <- nrow(NMNH_taxo) + nrow(NMNH_ht) # number of rows in converted taxo file plus number of rows in higher taxa
if(original == final) { 
  write.csv(NMNH_taxo,"~/GitHub/tpt-siphonaptera/output/NMNH_taxo.csv", row.names = FALSE) # write out taxo file
  print("YAY")
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
if(original == final) { 
  write.csv(FMNH_taxo,"~/GitHub/tpt-siphonaptera/output/FMNH_taxo.csv", row.names = FALSE) # write out taxo file
  print("YAY")
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
if(original == final) { 
  write.csv(Lewis_taxo,"~/GitHub/tpt-siphonaptera/output/Lewis_taxo.csv", row.names = FALSE) # write out taxo file
  print("YAY")
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
if(original == final) { 
  write.csv(CoL_taxo,"~/GitHub/tpt-siphonaptera/output/CoL_taxo.csv", row.names = FALSE) # write out taxo file
  print("YAY")
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
GBIF <- read.csv("~/GitHub/tpt-siphonaptera/output/GBIF_DwC.csv", na = "NA") # read in cleaned GBIF review file
GBIF$taxonomicStatus <- ifelse(GBIF$taxonomicStatus == "homotypic synonym", "homotypicSynonym",
                               ifelse(GBIF$taxonomicStatus == "heterotypic synonym", "heterotypicSynonym", GBIF$taxonomicStatus)) # replace non-conforming status
GBIF_ht <- higher_taxa_rank(GBIF, GBIF$taxonRank) # get GBIF higher taxa (DwC2taxo removes them, this is to check rows later)
GBIF_taxo <- DwC2taxo(GBIF, source = "GBIF") # transform to taxo format

GBIF_not_in_taxo <- GBIF[GBIF$taxonID %!in% GBIF_taxo$id,] # get all rows in GBIF that do not match an id in taxo
GBIF_problems <- GBIF_not_in_taxo[GBIF_not_in_taxo$taxonID %!in% GBIF_ht$taxonID,] # get all rows in above that do not match an id in GBIF_ht
write.csv(GBIF_problems,"~/GitHub/tpt-siphonaptera/output/GBIF_problems.csv", row.names = FALSE) # status is the most likely issue, so NULL it


# sanity check
original <- nrow(GBIF) # number of rows in cleaned file
final <- nrow(GBIF_taxo) + nrow(GBIF_ht) # number of rows in converted taxo file plus number of rows in higher taxa
if(original == final) { 
  write.csv(GBIF_taxo,"~/GitHub/tpt-siphonaptera/output/GBIF_taxo.csv", row.names = FALSE) # write out taxo file
  print("YAY")
} else {
  GBIF_fix <- read_excel("~/GitHub/tpt-siphonaptera/input/GBIF_problems.xlsx") # read in GBIF fixes
  GBIF_taxo <- rbind(GBIF_taxo, GBIF_fix) # return converted problems to working file
  
  final <- nrow(GBIF_taxo) + nrow(GBIF_ht) # recalculate number of rows in converted taxo file plus number of rows in higher taxa
  if(original == final) { print("yay") # print yay if no rows are missing
  } else {
    GBIF_ugh <- GBIF_problems[GBIF_problems$taxonID %!in% GBIF_problems_taxo$id,] # get all rows in taxo that do not match an id in problems
    write.csv(GBIF_ugh,"~/GitHub/tpt-siphonaptera/output/GBIF_problems.csv", row.names = FALSE) # write out problems for review
  }
}

# Flea_merge
Flea_m1 <- merge_lists(Lewis_taxo, CoL_taxo, "all") # master is Lewis, merging with CoL

# sanity check
original <- nrow(Lewis_taxo) + nrow(CoL_taxo) # number of rows in files to be merged
final <- nrow(Flea_m1) # number of rows in merged file
if(original == final) { 
  print("YAY")
} else {
  write.csv(Flea_m1,"~/GitHub/tpt-siphonaptera/output/Flea_m1.csv", row.names = FALSE) # write out merged file
  print("rows are missing")
}

Flea_m2 <- merge_lists(Flea_m1, NMNH_taxo, "all") # merge NMNH with working master

# sanity check
original <- nrow(Flea_m1) + nrow(NMNH_taxo) # number of rows in files to be merged
final <- nrow(Flea_m2) # number of rows in merged file
if(original == final) { 
  print("YAY")
} else {
  write.csv(Flea_m2,"~/GitHub/tpt-siphonaptera/output/Flea_m2.csv", row.names = FALSE) # write out merged file
  print("rows are missing")
}

Flea_m3 <- merge_lists(Flea_m2,FMNH_taxo, "all") # merge FMNH with working master

# sanity check
original <- nrow(Flea_m2) + nrow(FMNH_taxo) # number of rows in files to be merged
final <- nrow(Flea_m3) # number of rows in merged file
if(original == final) { 
  print("YAY")
} else {
  write.csv(Flea_m3,"~/GitHub/tpt-siphonaptera/output/Flea_m3.csv", row.names = FALSE) # write out merged file
  print("rows are missing")
}

Flea_m4 <- merge_lists(Flea_m3,GBIF_taxo, "all") # merge GBIF with working master

# sanity check
original <- nrow(Flea_m3) + nrow(GBIF_taxo) # number of rows in files to be merged
final <- nrow(Flea_m4) # number of rows in merged file
if(original == final) { 
  print("YAY")
} else {
  write.csv(Flea_m4,"~/GitHub/tpt-siphonaptera/output/Flea_m4.csv", row.names = FALSE) # write out merged file
  print("rows are missing")
}

# get non UTF8 authors
Flea_m4_x <- Flea_m4[which(!is.na(Flea_m4$author) & is.na(iconv(Flea_m4$author, "UTF-8", "UTF-8"))),] # get all non UTF8 authors

if (nrow(Flea_m4_x) == 0){
  "No non UTF8 characters"
} else{
  Flea_m4$author <- gsub("ñ", "n", Flea_m4$author) # replace non-UTF8
  Flea_m4$author <- gsub("á", "a", Flea_m4$author) # replace non-UTF8
  Flea_m4$author <- gsub("é", "e", Flea_m4$author) # replace non-UTF8
  Flea_m4$author <- gsub("ó", "o", Flea_m4$author) # replace non-UTF8
  Flea_m4$author <- gsub("ã", "a", Flea_m4$author) # replace non-UTF8
  Flea_m4$author <- gsub("ö", "o", Flea_m4$author) # replace non-UTF8
  Flea_m4$author <- gsub("ü", "u", Flea_m4$author) # replace non-UTF8
  Flea_m4$author <- gsub("è", "e", Flea_m4$author) # replace non-UTF8
  Flea_m4$author <- gsub("í", "i", Flea_m4$author) # replace non-UTF8
  Flea_m4$author <- gsub("Ã¨", "e", Flea_m4$author) # replace non-UTF8
  Flea_m4$author <- gsub("Ã¶", "o", Flea_m4$author) # replace non-UTF8
  Flea_m4$author <- gsub("Ã¦", "ae", Flea_m4$author) # replace non-UTF8
  Flea_m4_x <- Flea_m4[which(!is.na(Flea_m4$author) & is.na(iconv(Flea_m4$author, "UTF-8", "UTF-8"))),] # get all non UTF8 authors
  print("cleaned non-UTF8 characters, rerun if statement to make sure all were caught")
}

Flea_m4$family <- ifelse(is.na(Flea_m4$family),"None",Flea_m4$family)
Flea_m4$family <- toproper(Flea_m4$family) # ensure all family names are proper case

write.csv(Flea_m4,"~/GitHub/tpt-siphonaptera/output/Flea_merged.csv", row.names = FALSE) #write out merged file

# write out list as documents by family
families <- unique(Flea_m4$family)
family <- data.frame(families)

for (i in 1:nrow(family)){
     fam <- family$families[i]
     file <- paste(fam,"_taxolist.html",sep = "")
      Siphonaptera_checklist <- taxo2doc(Flea_m4,
                                   family=fam,
                                   title="TPT Flea Taxonomy",
                                   mastersource="Lewis",
                                   duplicatesyn=FALSE,
                                   outformat="html_document",
                                   outdir="C:/Users/Teresa/OneDrive/Documents/GitHub/tpt-siphonaptera/output/",
                                   outfile=file)
}

# write out entire list
Siphonaptera_checklist <- taxo2doc(Flea_m4,
                                   title="TPT Flea Taxonomy",
                                   mastersource="Lewis",
                                   duplicatesyn=FALSE,
                                   outformat="html_document",
                                   outdir="C:/Users/Teresa/OneDrive/Documents/GitHub/tpt-siphonaptera/output/",
                                   outfile="Flea_taxolist.html")
