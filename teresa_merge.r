# Teresa's merge
Lewis_merge <- Lewis_taxo
Lewis_merge$merge_tag <- "orig"

CoL_merge <- CoL_taxo
for (i in 1:nrow(CoL_merge)){
  if (CoL_merge$canonical[i] %in% Lewis_merge$canonical){
    CoL_merge$merge_tag[i] <- "add"
  } else {
    CoL_merge$merge_tag[i] <- "new"
  }
  }
}

merge <- rbind(Lewis_merge,CoL_merge)

# get non UTF8 authors
Flea_m4_x <- merge[which(!is.na(merge$author) & is.na(iconv(merge$author, "UTF-8", "UTF-8"))),] # get all non UTF8 authors

if (nrow(Flea_m4_x) == 0){
  "No non UTF8 characters"
} else{
  merge$author <- gsub("ñ", "n", merge$author) # replace non-UTF8
  merge$author <- gsub("á", "a", merge$author) # replace non-UTF8
  merge$author <- gsub("é", "e", merge$author) # replace non-UTF8
  merge$author <- gsub("ó", "o", merge$author) # replace non-UTF8
  merge$author <- gsub("ã", "a", merge$author) # replace non-UTF8
  merge$author <- gsub("ö", "o", merge$author) # replace non-UTF8
  merge$author <- gsub("ü", "u", merge$author) # replace non-UTF8
  merge$author <- gsub("è", "e", merge$author) # replace non-UTF8
  merge$author <- gsub("í", "i", merge$author) # replace non-UTF8
  merge$author <- gsub("Ã¨", "e", merge$author) # replace non-UTF8
  merge$author <- gsub("Ã¶", "o", merge$author) # replace non-UTF8
  merge$author <- gsub("Ã¦", "ae", merge$author) # replace non-UTF8
  Flea_m4_x <- merge[which(!is.na(merge$author) & is.na(iconv(merge$author, "UTF-8", "UTF-8"))),] # get all non UTF8 authors
  print("cleaned non-UTF8 characters, rerun if statement to make sure all were caught")
}

merge$family <- ifelse(is.na(merge$family),"None",merge$family)
merge$family <- toproper(merge$family) # ensure all family names are proper case

# write out entire list
test_checklist <- taxo2doc(merge,
                                   title="TPT Flea Taxonomy",
                                   mastersource="Lewis",
                                   duplicatesyn=TRUE,
                                   outformat="html_document",
                                   outdir="C:/Users/Teresa/OneDrive/Documents/GitHub/tpt-siphonaptera/output/",
                                   outfile="taxolist_test.html")
