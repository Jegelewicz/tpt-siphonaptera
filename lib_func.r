# libraries and functions used in this project

# libraries
library(readxl)
library(data.table)
library(stringi)
library(taxotools)
library(dplyr)
library(plyr)
library(devtools)

devtools::install_github("vijaybarve/taxotools") # install latest version of taxotools from Github

# functions

# define function: name length
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)

# define function: is not in
'%!in%' <- function(x,y)!('%in%'(x,y))

# define function: right
right = function (string, char) {
  substr(string,(unlist(lapply(gregexpr(pattern = char, string), min)) + 1),nchar(string))
}

# define function: left
left = function (string,char) {
  substr(string,1,unlist(lapply(gregexpr(pattern = char, string), min)))
}

# define function: text to columns
text_to_columns <- function(dat, col, data="", column="", separator="", new_col_name_prefix="") { # dat is the data frame to operate on and col is the name of the column to be split
  colno <- max(lengths(strsplit(col, separator))) # get max number of terms for any value in the column to be split
  setDT(dat)[, paste0(new_col_name_prefix, 1:colno) := tstrsplit(col, separator)] # parse out terms into separate columns with column names prefixed with new_col_name_prefix plus consecutive numbers from 1 through colno
}

# define function VLOOKUP (x = return value dataframe and column,
# y = lookup value dataframe and column, z = lookup dataframe and column
# x and z should be from the same dataframe)
vlookup <- function(x,y,z){
  x[match(y,z)]
}

# function: remove '\xa0' chars
phrase_clean <- function(x) gsub("[\xA0]", "", x)

# function: replace double spaces with single spaces
space_clean <- function(x) gsub("  ", " ", x)

# function: get everything from INSIDE any parenthesis
inparens <- function(x)gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", x, perl=T)

# function: get everything from OUTSIDE any parenthesis
outparens <- function(x){
  trimws(gsub("\\([^()]*\\)", "", x))
}

# function: apply a function to ALL character columns
char_fun <- function(x,y){ # x = dataframe, y = function to apply
  setDT(x)
  cols_to_be_rectified <- names(x)[vapply(x, is.character, logical(1))]
  x[,c(cols_to_be_rectified) := lapply(.SD, y), .SDcols = cols_to_be_rectified]
}

# define DwC conversion
convert2DwC <- function(df_colname) {
  x <- gsub('.*subspecies.*','infraspecificEpithet',df_colname)
  x <- gsub('.*rank.*','taxonRank',x)
  x <- gsub('.*author.*','author',x)
  x <- gsub('.*year.*','namePublishedInYear',x)
  x <- gsub('.*scientific.*','scientificName',x)
  x
}

# define function: get higher taxa with epithets
higher_taxa_epithet <- function(dat, sp, spp){ # data is data frame, sp is column where species is given, spp is column where subspecies is given
  dat[which(lapply(sp, name_length) == 0 & 
              lapply(spp, name_length) == 0),] # keep names where species and subspecies are blank
}

# define function: get higher taxa with rank
higher_taxa_rank <- function(dat, rank){ # dat is data frame, rank is column where taxon rank is given
  dat[which(rank != "species" & # remove taxa ranked species
              rank != "subspecies"),] # remove taxa ranked subspecies
}

# define function: get species with epithet
species_epithet <- function(dat, sp, spp){ # data is data frame, sp is column where species is given, spp is column where subspecies is given
  dat[which(lapply(spp, name_length) != 0 | # keep taxa with a subspecies name
              lapply(sp, name_length) != 0),] # keep taxa with a species name
}

# define function: get species with rank
species_rank <- function(dat, col){ # data is dataframe, col is column where rank is given
  df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),] # remove higher taxa from working file
  dat[which(col == "species" | # keep taxa with rank species
            col == "subspecies"),] # keep taxa ranked subspecies
}

# define function: fix cases like (Jordan & Rothschild), 1922 to (Jordan & Rothschild, 1922)
fixAuth <- function(x) ifelse(grepl('[a-z]),',x), paste(gsub(')', '',x),')',sep=''),x)

# taxotools compact id function
compact_ids <- function(dat,id="id",accid="accid",verbose=TRUE){
  if(is.null(dat)){
    return(NULL)
  }
  if(id %!in% names(dat)){
    message("id field missing. Returning NULL")
    return(NULL)
  }
  if(accid %!in% names(dat)){
    message("accid field missing. Returning NULL")
    return(NULL)
  }
  dat <- rename_column(dat,id,"id_")
  dat <- rename_column(dat,accid,"accid_")
  dat$id <- seq(1:nrow(dat))
  dat$accid <- 0
  if(verbose){pb = txtProgressBar(min = 0, max = nrow(dat), initial = 0)}
  for(i in 1:nrow(dat)){
    if (!(is.na(dat$accid_[i]) | dat$accid_[i]==0 |
          dat$accid_[i]=="0" | dat$accid_[i]=="")){
      dat$accid[i] <- dat$id[which(dat$id_==dat$accid_[i])]
    }    
    if(verbose){setTxtProgressBar(pb,i)}
  }
  if(verbose){cat("\n")}
  dat$id_ <- dat$id
  dat$accid_ <- dat$accid
  dat <- rename_column(dat,"id_",id)
  dat <- rename_column(dat,"accid_",accid)
  dat[,c("id","accid")] <- list(NULL)
  return(dat)
}

rename_column <- function(dat,old,new,silent=FALSE){
  if(old %in% colnames(dat)){
    colnames(dat)[which(names(dat) == old)] <- new
  } else {
    if(!silent){
      cat(paste("\nFieldname not found...",old))
    }
  }
  return(dat)
}

taxo2doc <- function(taxolist=NULL,genus=NA,source="",
                     outformat="html_document",
                     outdir=".",outfile="taxolist.html"){
  if(is.null(taxolist)){
    stop("No taxolist to process")
  }
  if("species" %!in% names(taxolist)){
    taxolist <- melt_canonical(taxolist,
                               canonical="canonical",
                               genus="genus",
                               species="species",
                               subspecies="subspecies")
  }
  tfile <- tempfile("taxo_", fileext = c(".rmd"))
  con <- file(tfile)
  sink(con, append=TRUE)
  if(source==""){
    cat(paste('---\ntitle: "Taxonomic list" \n---\n\n'))
  } else {
    cat(paste('---\ntitle: "Taxonomic list: ',source,'"\n---\n\n'))
  }
  mytaxo <- taxolist
  mytaxo <- mytaxo[!duplicated(mytaxo$canonical),]
  mytaxo$family[which(is.na(mytaxo$family))] <- "-"
  if("author" %in% names(mytaxo)){
    mytaxo$author[which(is.na(mytaxo$author))] <- ""
  } else {
    mytaxo$author <- ""
  }
  mytaxo <- mytaxo[with(mytaxo, order(family,genus,species,subspecies)),]
  mytaxo <-mytaxo[which(mytaxo$species!="unidentifiable"),]
  if(!is.na(genus)){
    mytaxo <-mytaxo[which(mytaxo$genus %in% genus),]
  }
  cat(paste("  \n\n"))
  mytaxo_ac <- mytaxo[which(mytaxo$accid==0),]
  if(nrow(mytaxo_ac)==0){
    cat(paste("# ",source,"has nothing to display  \n"))
    sink() 
    return()
  }
  fam <- ""
  for(i in 1:nrow(mytaxo_ac)){
    if(mytaxo_ac$family[i]!=fam){
      cat(paste("  \n\n"))
      cat(paste("## Family: _",mytaxo_ac$family[i],"_  \n",sep=''))
      fam <- mytaxo_ac$family[i]
    }
    mytaxo_ac$author[i] <- utf2ascii(mytaxo_ac$author[i])
    cat(paste(i," _",mytaxo_ac$canonical[i],"_ ",mytaxo_ac$author[i],"  \n",sep = ''))
    if(nrow(mytaxo[which(mytaxo$accid==mytaxo_ac$id[i]),])>0){
      synlst <- mytaxo[which(mytaxo$accid==mytaxo_ac$id[i]),]
      for(j in 1:nrow(synlst)){
        synlst$author[j] <- utf2ascii(synlst$author[j])
        cat(paste("\t  = _",synlst$canonical[j],"_ " ,synlst$author[j],"  \n",sep = ''))
      }
    }
  }
  sink() 
  rmarkdown::render(input=tfile,
                    output_format=outformat,
                    output_dir = outdir,
                    output_file = outfile)
  return(NULL)
}

utf2ascii <- function(x){
  x <- ifelse(!is.na(x) & Encoding(x)=="UTF-8",
              stringi::stri_trans_general(x,id="Latin-ASCII"),x)
  return(x)
}

toproper <- function(x) ifelse(!is.na(x),
                               paste0(toupper(substr(x, 1, 1)),
                                      tolower(substring(x, 2))),NA)
