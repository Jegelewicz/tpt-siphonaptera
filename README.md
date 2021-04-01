# tpt-siphonaptera
Code for cleaning and merging Siphonaptera taxonomy for the Terrestrial Parasite Tracker

## Taxonomy Cleaning for Terrestrial Parasite Tracker Taxonomy

The R script in this repository was designed for cleaning taxonomic classifications received from various sources for the Terrestrial Parasite Tracker Thematic Collections Netowrk (TPT) Taxonomy Reource.

### Input
Input is required to be csv and is expected to include at least the following columns:
 - kingdom
 - phylum
 - class
 - order
 - family
 - genus
 - species (specific epithet)
 - taxon Author name (may be combined with or separate from published year)
 - taxon published year

Other information may be included in the file, including ranks between the standard ranks listed above and subspecific epithets.
**NOTE** All taxonomy fields are expected to include a single term, "species" is really "specific epithet" and "subspecies" is really "infraspecific epithet".

### Output
Running the script will produce the following outputs in csv:
File Name | Description 
 -- | -- 
taxa_no_issues | Classifications without issues in Darwin Core format 
taxa_need_review | Classifications that need review with a comment on why they were flagged 
duplicates | Classifications removed from the original data because they were duplicates 
suggested_adds | A list of higher taxon names that probably need to be added because they are used by children
higher_taxa_not_used | A list of higher taxon names that are in the file, but not used by any children, these may need to be removed, or it may indicate that names are missing from or misspelled in the original source
similar_names | A comparative list of names that appear closely related. These should be reviewed to ensure there are no misspellings or errors that cleaning otherwise would not catch.

### Usage
Information from a source may need to be run through the script multiple times. We suggest that after the first pass, all output that requires review is assessed and any necessary changes incorporated into the taxa_no_issues.csv file which should be run through the script a second time. Repeat this process until there are no longer rows in taxa_need_review.csv, or duplicates.csv and it is certain that all items in suggested_adds.csv, higher_taxa_not_used.csv and similar_names.csv are reasonable. The final product in taxa_no_issues.csv will be added to the TPT Taxonomy resource.
