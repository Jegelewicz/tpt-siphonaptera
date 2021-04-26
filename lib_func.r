# libraries and functions used in this project

# libraries
library(readxl)
library(data.table)
library(stringi)
library(taxotools)
library(dplyr)

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
  setDT(dat)[, paste0(new_col_name_prefix, 1:colno) := tstrsplit(col, separator)] # parse out terms into separate columns with column names prefixed with new_col_name_prefix plus consecutive numbers from 1
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

