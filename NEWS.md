# EMLeditor v0.1.0.5 "Congaree Boardwalk Loop"

## 12 April 2023
  * `set_doi()` and `set_datastore_doi()` will now automatically update the online urls listed in the metadata for each data file to correspond to the new location. Caution: metadata with a DOI generated prior to 12 April 2023 may have incorrect online URLs.
  * Attempt to add .Rmd template to Rstudio

## 04 April 2023

  * `upload_data_package()` maximum file size increased to 32Mb (from 4Mb)

## 24 March 2023

  * Added tryCatch to `.get_park_poygon()` to improve error handling for invalid park codes.
  * Improved `set_content_units()` error handling to specifically test for invalid park codes prior to executing & report list of invalid park codes to user.
  * Fixed (yet another) bug in `get_content_units()`.

# EMLeditor v0.1.0.4 "Acadia"

## 21 March 2023

Summary

Added a new function `upload_data_package()` that will upload data package files to the appropriate draft reference on DataStore. Individual files must be < 4Mb.

### Major changes:

  * Added `upload_data_package()` that will upload data package files to the appropriate draft reference on DataStore. The function is only compatible with .csv data files and requires a single EML metadata file ending in *_metadata.xml to all be present in a single folder/directory. The metadata file must have a DOI specified. `upload_data_package()` will extract the DOI from metadata and check to see if a corresponding reference exists on DataStore. If the reference exists, the function will upload each file in the data package (including the metadata file). 
  
### Minor changes

  * Minor update to `get_doi()` points; if DOI doesn't exist the function now refers users to both `set_doi()` and `set_datastore_doi()`.
  * Minor updates to documentation for consistency and grammar.

# EMLeditor v0.1.0.3 "Hall of Mosses"

## February 24, 2023

Summary

Added a new function, `set_datastore_doi()` that will initiate a draft reference on DataStore and insert the DOI into metadata

### Major changes:

  * Added a new function, `set_datasore_doi()` that will initiate a draft reference on DataStore and insert the DOI into metadata. It requires that the user be logged on to the VPN and that the metadata has a title for the data package. The function will warn the user if the metadata already contains a DOI and will ask it they really want to generate a new draft reference and new DOI. 

### Minor changes

  * Updated documentation to reflect the new `set_datastore_doi()` function.
  * Updated the `get_title()` and `get_doi()` functions to get just the data package title and just the data package DOI, respectively. They had been returning multiple titles and dois if the <title> and <alternateIdentifier> fields were used multiple times in the metadata.

# EMLeditor v0.1.0.2 "Devils Tower"

## February 09, 2023

# Summary

Bug fixes, update `set_cui()` codes, flesh out `set_int_rights`. Update documentation.

### Major changes

  * replaced PUBVER and PUBFUL codes with PUBLIC in `set_cui()`.
  * removed NPSONLY code from `set_cui()`.
  * major bug fixes to `set_content_units()`.
  * updated `set_int_rights()` to also populate licenseName field.
  
### Minor changes

  * fixed minor typos in documentation
  * moved `set_int_rights()` from "additional functions" to "minimal workflow"

# EMLeditor v0.1.0.1 "Whitebark Pine"

## January 24, 2023

### Summary

Added `check_eml()` function.

### Major changes

`check_eml()` function is a wrapper that calls `DPchecker::run_congruence_checks()` with `check_metadata-only = TRUE`. To run all the metadata-specific tests that will be run during a congruence test.

### Minor changes

  * specify ISO 639-2B in `set_language()`
  * added documentation for `check_eml()`
  * Changed Non-NPS user section to more apt, "Custom Publisher/Producer"
  * added a "Additional Functions" section in addition to the Minimal Workflow that outlines functions like `set_title()`, `set_abstract()` and `set_int_rights()`.
  * moved `write_readme()` and the new `check_eml()` to the file check_eml.R.

***

# EMLeditor v0.1.0.0, "Electric Peak"

## December 1, 2022

### Summary

Added `set_int_rights()` function.

### Major Changes

`set_int_rights()` allows users to update the intellectual rights text supplied by 3rd party EML generators with one of 3 NPS-specific options. Enforces congruence between CUI and license.

***

## November, 2022

### Summary

Updating to v0.1.0.0 "Electric Peak" is recommended for all users in order to take full advantage of metadata/DataStore integration included the most up-to-date locations and specifications for DataStore metadata elements.

### Major Changes

1) The ability to switch "set_" class functions from a verbose (asks user for input, provides feedback) to silent (no feedback, no prompts) to enable scripting.

2) Inclusion of set_publisher function to customize the publisher and agencyOriginated options for non-NPS users, for NPS partners and contractors.

### Enhancements

1) CUI can now be overwritten as well as written

2) write_readme dynamically populates publisher information

3) Renamed functions and parameters to conform to tidyverse style guides

4) Removed redundant functions (set_doi)

5) Added ability to set DRR title and DOI

6) write_readme now defaults to printing to the screen (but can still save to a .txt file)

7) Update documentation to reflect changes

### Bug Fixes

Let's just leave this at "a lot".

***

# EMLeditor 0.0.1.1

* Added a `NEWS.md` file to track changes to the package.
