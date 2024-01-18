
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CodeFactor](https://www.codefactor.io/repository/github/roblbaker/emleditor/badge)](https://www.codefactor.io/repository/github/roblbaker/emleditor)

<!-- badges: end -->

# EMLeditor

## Overview

The goal of EMLeditor is to edit EML-formatted xml files. Specifically,
EMLeditor provides many functions that will be useful to the U.S.
National Park Service when generating metadata for statistical data
packages uploaded to DataStore. NPS affiliation is assumed as default.
However, some of the functions for viewing and editing metadata may be
useful to people outside the NPS.

## Installation and updates

You can install and update the development version of EMLeditor from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nationalparkservice/EMLeditor")
```

To install all the packages in the
[NPSdataverse](https://github.com/nationalparkservice/NPSdataverse)
(including EMLeditor):

``` r
devtools::install_github("nationalparkservice/NPSdataverse")
```

## Workflow outline

EMLeditor comes with a template Rmarkdown script that you can edit to
generate a fully fledged EML document. The script includes and
accompanying documentation includes information on:

1)  Generating an initial EML document using the R/EMLassemblyline
    package functions
2)  Adding in NPS specific and DataStore specific EML elements using the
    R/EMLeditor package functions
3)  Checking the EML document to make sure it is schema-valid and passes
    all the necessary tests for uploading to DataStore
4)  Generating a draft data package reference on DataStore and
    incorporating DOIs into the metadata
5)  Uploading a completed data package to DataStore

Please *DO NOT ACTIVATE* the DataStore reference: prior to activation,
data packages need to be reviewed via a yet-to-be-created process.

To access the EML creation script from within EMLeditor, install (or
update) the EMLeditor package and restart R. From within Rstudio, select
the “File” drop-down menu and choose “New File” (the first option). From
within the “New File” menu, select “Rmarkdown…”. In the pop-up menu,
select “From Template on the left hand side. Then choose the
template,”Editable_EML_Creation_Workflow {EMLeditor}” then click “OK”.

If you use EMLeditor functions to alter your metadata (e.g. “set” class
functions) they will also silently add the National Park Service as a
publisher (including location, [ROR id](https://ror.org/), etc) to your
metadata unless you set NPS=FALSE. If you leave the default setting as
NPS=TRUE, EMLeditor will also assume the data package is being created
“by or for the NPS” and add that information to the metadata.

EMLeditor will also add information about the version of EMLeditor you
used to edit your metadata (for instance if you used “set” class
functions).
