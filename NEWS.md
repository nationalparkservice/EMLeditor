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
