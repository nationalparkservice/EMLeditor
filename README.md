
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CodeFactor](https://www.codefactor.io/repository/github/roblbaker/emleditor/badge)](https://www.codefactor.io/repository/github/roblbaker/emleditor)
<!-- badges: end -->

# EMLeditor

##### v0.1.3

##### “Single Pen”

<!-- badges: start -->
<!-- badges: end -->

# Overview
The goal of EMLeditor is to edit Ecological Metadata Language (EML) formatted xml files. Specifically, EMLeditor provides many functions that will be useful to the U.S. National Park Service when generating metadata for statistical data packages uploaded to DataStore. NPS affiliation is assumed as default. However, some of the functions for viewing and editing metadata may be useful to people outside the NPS. If you are not an NPS user, you will want to read the sections on [Custom Publisher/Producer](articles/a05_advanced_functionality.html#custom-publisherproducer).

EMLeditor itself cannot be used to generate EML, only to edit EML. The web pages and documentation associated with the EMLeditor R package, however cover the entire process of generating and editing EML, checking, and uploading metadata (and data) to [DataStore](https://irma.nps.gov/DataStore/) (the National Park Service digital repository). The documentation therefore relies on more than just the functions in the EMLeditor R package. Specifically, the documentation refers to functions included in the following packages: [EML](https://www.cran-e.com/package/EML), [EMLassemblyline](https://ediorg.github.io/EMLassemblyline/), [QCkit](https://nationalparkservice.github.io/QCkit/), EMLeditor, [DPchecker](https://nationalparkservice.github.io/DPchecker/), and [tidyverse](https://www.tidyverse.org/).

All of the required packages except tidyverse can be accessed through the [NPSdataverse](https://nationalparkservice.github.io/NPSdataverse/).

A quick note on function provenance: The package name followed by a double colon is used to indicate the function provenance (e.g. `EML::write_eml()` indicates that the function `write_eml()` comes from the package EML). This level of documentation is a work in progress.

# Install and update 
To install all the packages in the
[NPSdataverse](https://github.com/nationalparkservice/NPSdataverse) (including EMLeditor):

```r 
install.packages(c("devtools", "tidyverse") # only required once

# install or update NPSdataverse:
devtools::install_github("nationalparkservice/NPSdataverse") # typically only required once
```

It is recommended to always load EMLeditor and all the necessary packages for generating EML: 
```r
library(tidyverse)
library(NPSdataverse)
```
Loading the entire NPSdataverse library is the preferred method as it will check whether all of your NPSdataverse packages are up to date (including EMLeditor) and provide instructions on how to update them if you are not working with the latest version.

Upon loading the NPSdataverse, you should see a list of all the packages that have been loaded along with the NPSdataverse and their version numbers. If you see the message, "All NPSdataverse packages are up to date", you should be ready to start.  If you see the message, "The following packages are out of date" follow the on-screen instructions to update the indicated packages. If you see neither of those two messages, your version of NPSdataverse is quite old. You will need to update the NPSdataverse package itself before proceeding. 

# Create a data package
To create a data package including generating EML, please refer to the web pages listed under "Articles", specifically the instructions on the [EML Creation Script](articles/a02_EML_creation_script.html) page.

You may also be interested in the [pre-requisites necessary](articles/a01_prereqs.html) before starting to generate EML a series of guides on how to [edit the .txt files](articles/a03_Template_edits.html) that are generated, some instructions on [editing EML](articles/a04_Editing_fixing_eml.html) after it is created, and options for [advanced users](articles/a05_advanced_functionality.html).


