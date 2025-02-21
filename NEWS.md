# EMLeditor v1.0.1(under development)
## 2025-02-22
  * Add `CONTRIBUTING.md` file
  
## 2025-02-06
  * Bug fix in `set_int_rights` that was affecting CC0 licenses.

# EMLeditor v1.0.0
## 2025-01-16
  * Fix bug in `set_project` and update associated documentation.

## 2024-11-04
  * Fix documentation in EMLscript code chunk that can make it difficult/confusing to enter user input after running the `upload_data_package` function.

## 2024-10-23
  * updates and fixes to documentation including spelling, grammar, and clarity
  * remove non-exported functions from documentation
  * move several packages from Imports to Suggests (ISOcodes, sf, gdata)

# EMLeditor v0.1.6
## 2024-09-17
  * Update `set_content_units` to have correct east and west GPS coordinates for bounding boxes.

## 2024-08-29
  * Update readme: add R-CMD-CHECK badge; use pak to install instead of devtools
  * Update licenseName field on restricted references to read, "Unlicensed (not for public dissemination)"
  * add github actions: build check
  * add `set_project` to the EMLscript template (.Rmd) and the github.io documentation pages.
  * update `set_project` so that it adds projects instead of replacing them.
  * update `set_project` to use cli errors/warnings
  * add minimal unit test for `set_project`

## 2024-08-21
  * add id tag to projects to help DataStore identify DataStore projects vs. other projects.

## 2024-08-20
  * add helper.R file with a test_path function to facilitate unit tests
  * update unit test code to run both interactively and during build checks
  * add yaml file to conduct github actions: build test

## 2024-07-10
  * add in the new function `set_project()` and attempt to update existing function, `set_protocol()`.
  * update license from MIT to CC0.

## 2024-07-02
  * updated all API requests to user v6 API instead of v7.

## 2024-07-01
  * Updated `.get_park_polygon()` to use the latest version of the API rather than a legacy version.

## 2024-06-26
  * Added new function, `set_new_creator()` which can add one or more creators to EML.

## 2024-05-01
  * Fix documentation: typo/formatting for the description of `set_int_rights()` in the EML Creation Script github.io page.

## 2024-04-29
  * Bug fix for `set_cui()` deprecation message: now points to the correct updated function (`set_cui_code()`).

## 2024-04-08
  * Bug fix for `set_doi()`, which was not always updating dataTable URLs.

# EMLeditor v0.1.5 "Little Bighorn"

## 2024-04-01
  * Fix bug in `set_creator_orcids()`: no longer adds https://orcid.org/NA for creators without an orcid.
  * Added checks in `set_creator_orcids()` such that users must specify NA (not "NA") and to check that the length of the orcid list supplied matches the length of the authors in metadata (excluding organizational authors).
  * Updated `set_creator_orcids()` documentation to specify that the function can also be used to remove orcids from authors.
  * Updated the EML creation script to reference `set_cui_code()` as opposed to the (now deprecated) `set_cui()`.

## 2024-04-01
  * Fix bug in `set_cui_code()` that was detecting both CUI code and CUI marking.
  * Fix bug in `set_cui_marking()`.
  * Fix bug in `set_creator_order()`.

## 2024-03-12
  * make `write_readme()` a non-exported function.

## 2024-02-29
  * Add function `get_cui_code()`. Deprecate function `get_cui()`.
  * Add function `get_cui_marking()`.

## 2024-02-22
  * Added function `set_missing_data()` which allows users to add missing data codes and missing data code definitions to metadata.
  * Added utility functions `.get_user_input()` and `.get_user_input3()`. Refactored all set_ class functions to use these sub-functions rather than readLines() to get user input.

## 2024-02-13
  * Deprecated `set_cui()` in favor of `set_cui_dissem()`, which does the exact same thing as `set_cui()` but the function name has been updated to distinguish the action of the function from the newly added `set_cui_code()` function.
  * Updated the publisher contact email in `set_npspublisher()` from irma@nps.gov to nrss_datastore@nps.gov to reflect DataStore changes in the contact email address.

# EMLeditor v0.1.4 "Mackinac Island"

## 2024-01-18

  * Added the function `remove_datastore_files()`, which can detach files from a DataStore Reference. In conjunction with `upload_data_package()` this allows a user to update and make changes to the files in a data package (for instance, in response to review of the data package) prior to activating the data package.

# EMLeditor v0.1.3 "Single Pen"

## 2023-11-07
  * Updated EML template script to fix typos, remove the write_readme section, and add more explanation of the personnel.txt file.
  * Updated `set_datastore_doi()` to use the correct doi prefix when dev = TRUE and display the correct URL upon draft reference creation.
  * Ported over most of the documentation on the EML Creation Script from the NPS_EML_Script repo to all be held under this repo.
  * Updated documentation on making EML; updated the Readme to reflect the fact that all EML creation documentation/instructions are now included in the EMLeditor package
  * Removed the "Get Started" (EMLeditor.rmd) file as it was pretty redundant with the readme.md file.
  * Updated the template script in R studio to include package provenance for function calls.
 
# EMLeditor v0.1.2 "Mukooda Trail"

## 06 October 2023
  * Updated `set_datastore_doi()` and `upload_data_package()` functions to allow them to work with IRMA dev for testing and training purposes.
  * Updated `upload_data_package()` to prevent file upload if the reference already has files associated with it.

# EMLeditor v0.1.1 "Big South Fork"

## 29 August 2023
  * Updated all rest API services from v4/v5 to v6. Units services remain at v2.

## 15 August 2023
  * Fix bugs in `get_authors()`
  * Fix bugs in `get_abstract()`

## 19 July 2023
  * Fix examples in `set_creator_orgs()`
  * Add function `set_methods()` allows user to add or replace existing methods sections.
  * Add function `get_methods()` returns the methods section (as a list)
  * Add function `set_additiona_info()` allows the user to set or replace existing addtitionalInfo. AdditionalInfo becomes "Notes" on the DataStore landing page.
  * Add function `get_additional_info()` returns the additionalInfo ("notes") from metadata.

## 12 July 2023
  * Add global variable bindings
  * Fix how set_creator_orcids handles orcids; now takes a 19-char string as input and saves orcid as a 37-char string with "https://orcid.org/" prefix.

## 10 July 2023
  * Fixed minor typos in EML creation script.

## 30 June 2023
  * Added a "park_units" parameter to `set_creator_orgs()`. This takes a park unit (or list of park units) and uses the IRMA units service to populate the organizationName with the FullName of the park_unit specified. If you specify park_units, you cannot also specify "creator_orgs" - non-park unit organizations must be added as creators using a separate call to `set_creator_orgs()` (and the creators can subsequently be reorganized using `set_creator_order()`).

## 22 June 2023
  * Bug fix for `set_int_rights()` - previously it only worked if you used `set_cui()`, exported to .xml and then re-imported to R. Now you can go straight from `set_cuio()` to `set_int_rights()`.
  * Updated documentation: more information on additional functions available and how to use `upload_datapackage()` function
  * updated the EML script template: EMLassemblyline::make_eml now does not write a .xml by default but instead defaults to generating an R object that can subsequently be processed via EMLeditor functions.


# EMLeditor v0.1.0.7 "Clingmans Dome"

## 16 June 2023
  * added the function `set_creator_order()`, which allows users to re-order the creators (authors on DataStore) as well as remove creators. 

## 14 June 2023
  * updates to the EML creation script; the make_eml() function stopped saving an EML object in R and was just writing a .xml to the working directory. Add the the arguments `return.obj = TRUE` and `write.file = FALSE` to get it to do the opposite: save an EML object to R for further processing and not write the .xml (so as not to create confusion later on)
  * added the function `set_creator_orgs()` which allows users to add organizations as creators (authors on DataStore). EMLassemblyline did not appear to support this functionality. 

## 13 June 2023
  * added the function `set_creator_orcids()` which allows users to add or edit ORCiDs for individuals (not organizations) listed as creators

## 12 June 2023
  * updated error messages in `get_author_list()` and `get_citation()` to be more informative, especially when surName is missing from the creator/individualName

## 21 April 2023
  * updated `set_datastore_doi()` so that it does not prompt to use `set_datastore_doi()` if there is no previous doi. Fixed readline prompt cursor on the wrong line. Now generates draft references with blank fields instead of place-holder strings (except for the title).

# EMLeditor v0.1.0.6 "Double Arch"

## 19 April 2023
  * updated `set_content_units()` to include the attribute "system = content unit link" as part of the geographicCoverage element for each geographicCoverage element that is also a park content unit link (the old text in the field, "NPS Content Unit Link:" will be retained).

## 18 April 2023
  * updated `set_data_urls()`, `set_doi()`, and `set_datastore_doi()` to handle cases when there is only one dataTable (as well as multiple data tables).
  * updated `set_cui()` to handle cases when there is no previus additionalMetadata element in metadata.
  * updated `set_cui()` and `set_int_rights()` such that they can accept parameters in any case, not just upper (`set_cui()`) or lower (`set_int_rights()`).

## 13 April 2023
  * added `set_data_urls()` function to update dataTable urls in metadata to correspond to the DOI in the metadata. 
  * updated `get_doi()` to add a line return to error message.

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

### Summary

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
