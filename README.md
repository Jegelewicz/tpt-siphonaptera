# tpt-siphonaptera
Code for cleaning and merging Siphonaptera taxonomy for the Terrestrial Parasite Tracker

## Taxonomy Cleaning for Terrestrial Parasite Tracker Taxonomy

The R scripts in this repository were designed for cleaning taxonomic classifications received from various sources for the Terrestrial Parasite Tracker Thematic Collections Network (TPT) Taxonomy Reource. Specific scripts were created for transforming data from each resource as well as merging the resources for review.

### lib_func.r
Loads all needed libraries and functions for other scripts. Should be run before any other scripts are run.

### Lewis_transform.r
Transforms BYU Lewis list as updated provided by Mike Hastriter to Darwin Core

#### Input
File Name | Description 
 -- | -- 
Lewis World Species List MMM DD YYYY.xlsx | Lewis database as provided by Mike Hastriter at BYU
Lewis_reviewed.xlsx | Names from Lewis_name_review output that have been corrected and are to be returned to the working file
Lewis_removed.xlsx | Names from Lewis_name_review output that have been removed from the working file
tpt_dwc_template.xlsx | Template (no data) for Darwin Core file

#### Output
File Name | Description 
 -- | -- 
Lewis_duplicates.csv | Names removed from the original data because they were duplicates 
Lewis_name_review.csv | Names removed from the original data that need review before adding back or removing (see inputs above)
Lewis_non_DwC.csv | Name ID plus all non Darwin Core fields from original file
Lewis_DwC.csv | Name ID plus all applicable Darwin Core fields

### NMNH_transform.r
Transforms Smithsonian (NMNH) list of taxa to Darwin Core

#### Input
File Name | Description 
 -- | -- 
NMNH_Siphonaptera.xlsx | Catalog of fleas from the Smithsonian
NMNH_reviewed.xlsx | Names from NMNH_name_review output that have been corrected and are to be returned to the working file
tpt_dwc_template.xlsx | Template (no data) for Darwin Core file

#### Output
File Name | Description 
 -- | -- 
NMNH_need_review.csv | Names removed from the original data that need review before adding back or removing (see inputs above)
NMNH_non_DwC.csv | Name ID plus all non Darwin Core fields from original file
NMNH_DwC.csv | Name ID plus all applicable Darwin Core fields

### FMNH_transform.r
Transforms Field Museum (FMNH) list of taxa to Darwin Core

#### Input
File Name | Description 
 -- | -- 
FMNH_Siphonaptera.xlsx | List of flea names from the Field Museum
FMNH_reviewed.xlsx | Names from NMNH_name_review output that have been corrected and are to be returned to the working file
tpt_dwc_template.xlsx | Template (no data) for Darwin Core file

#### Output
File Name | Description 
 -- | -- 
FMNH_need_review.csv | Names removed from the original data that need review before adding back or removing (see inputs above)
FMNH_non_DwC.csv | Name ID plus all non Darwin Core fields from original file
FMNH_DwC.csv | Name ID plus all applicable Darwin Core fields

### CoL_transform.r
Transforms Catalogue of Life (CoL) download to Darwin Core

#### Input
File Name | Description 
 -- | -- 
CoL_DwC.xlsx | Flea names from Catalogue of Life download
tpt_dwc_template.xlsx | Template (no data) for Darwin Core file

#### Output
File Name | Description 
 -- | -- 
CoL_DwC.csv | Name ID plus all applicable Darwin Core fields

### merge_taxotools.r
Transforms Global Biodiversity Information Facility (GBIF) download and all ofther Darwin Core files to taxotools format, then merges them and generates a checklist for expert review

#### Input
File Name | Description 
 -- | -- 
Lewis_DwC.csv | Output of Lewis_transform.r
NMNH_DwC.csv | Output of NMNH_transform.r
FMNH_DwC.csv | Output of FMNH_transform.r
CoL_DwC.csv | Output of CoL_transform.r
GBIF_Siphonaptera.xlsx | Flea names from GBIF download (already in DwC format, but still transformed a bit in this script)

#### Output
File Name | Description 
 -- | -- 
problems.csv | Names that could not be merged and need review
taxo_siphonaptera.csv | Merged list of names
Flea_taxolist.html | Checklist of merged names for expert review

### arctos_upload_transform.r
Transforms Darwin Core files to Arctos hierarchical tool upload format (awaiting final list to create transform)

#### Input
File Name | Description 
 -- | -- 
Arctos_upload.csv | Template (no data) for Arctos upload

#### Output
File Name | Description 
 -- | -- 

### Usage
Information from a source may need to be run through the appropriate script multiple times. Any change to a primary source will require re-run and a new merge.
