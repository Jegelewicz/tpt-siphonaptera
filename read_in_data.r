# Read in my files
library(readxl)
library(readr)

BYU_Siphonaptera_Synonyms <- read_excel("~/GitHub/ixodes-tpt/input/BYU Siphonaptera Synonyms.xlsx", 
                                          +     sheet = "Siphonaptera BYU Syn DwC full")
BYU_Siphonaptera <- read_excel("~/GitHub/ixodes-tpt/input/BYU Siphonaptera.xlsx", 
                                 +     sheet = "Siphonaptera BYU DwC full")
FMNH_Siphonaptera <- read_excel("~/GitHub/ixodes-tpt/input/FMNH Siphonaptera.xlsx", 
                                  +     sheet = "Siphpnaptera FMNH DwC full")
NMNH_Siphonaptera <- read_excel("~/GitHub/ixodes-tpt/input/NMNH Siphonaptera.xlsx", 
                                  +     sheet = "Siphonaptera NMNH DwC full")

# BYU_Siphonaptera_Synonyms <- read.csv("~/GitHub/ixodes-tpt/input/BYU_Siphonaptera_Synonyms.csv", encoding="UTF-8")
# BYU_Siphonaptera <- read.csv("~/GitHub/ixodes-tpt/input/BYU_Siphonaptera.csv", encoding="UTF-8")
# CoL_Siphonaptera <- read.csv("~/GitHub/ixodes-tpt/input/CoL_Siphonaptera.csv", encoding="UTF-8")
# FMNH_Siphonaptera <- read.csv("~/GitHub/ixodes-tpt/input/FMNH_Siphonaptera.csv", encoding="UTF-8")
# GBIF_Siphonaptera <- read.csv("~/GitHub/ixodes-tpt/input/GBIF_Siphonaptera.csv", encoding="UTF-8")
# ITIS_Siphonaptera <- read.csv("~/GitHub/ixodes-tpt/input/ITIS_Siphonaptera.csv", encoding="UTF-8")
# NMNH_Siphonaptera <- read.csv("~/GitHub/ixodes-tpt/input/NMNH_Siphonaptera.csv", encoding="UTF-8")

# create source summary dataframe
BYU <- nrow(BYU_Siphonaptera) + 
  nrow(BYU_Siphonaptera_Synonyms)# number of original records in BYU Sources
# COL <- nrow(CoL_Siphonaptera) # number of original records in CoL Source
FMNH <- nrow(FMNH_Siphonaptera) # number of original records in FMNH Source
# GBIF <- nrow(GBIF_Siphonaptera) # number of original records in GBIF Source
# ITIS <- nrow(ITIS_Siphonaptera) # number of original records in ITIS Source
NMNH <- nrow(NMNH_Siphonaptera) # number of original records in NMNH Source
TOTAL <- nrow(BYU_Siphonaptera) + 
  nrow(BYU_Siphonaptera_Synonyms) + 
  nrow(FMNH_Siphonaptera) + 
#   nrow(GBIF_Siphonaptera) +
  nrow(NMNH_Siphonaptera) # number of original records in all sources
source_code <- c("BYU", "FMNH", "NMNH", "Total")
original_name_count <- c(BYU, FMNH, NMNH, TOTAL)
source_summary <- data.frame(source_code, original_name_count)

# combine all sources for cleaning
df <- rbind(BYU_Siphonaptera, BYU_Siphonaptera_Synonyms, FMNH_Siphonaptera, NMNH_Siphonaptera)
# df <- rbind(BYU_Siphonaptera,BYU_Siphonaptera_Synonyms, CoL_Siphonaptera, FMNH_Siphonaptera, GBIF_Siphonaptera, ITIS_Siphonaptera, NMNH_Siphonaptera)

# verify number of names
# verify no records were lost
verification_passed = FALSE # set variable to FALSE
if(TOTAL != nrow(df)) { # test that TOTAL is equal to number of names in combined data frame
} else {
  verification_passed = TRUE # if true, change variable to TRUE
}

if(verification_passed) { # if variable is TRUE
  print('name count verified, proceed to tpt_text_cleaning script') # print message that count was verified
  } else { # if variable is FALSE
  print('Verification was not passed. Some records appear to have been lost. Script was terminated. Please address errors.') # print message that count was not verified
  }
