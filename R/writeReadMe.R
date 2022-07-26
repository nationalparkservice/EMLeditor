###############################################################################
###############################################################################
################# write.readMe() writes a text file ###########################
###############################################################################
###############################################################################

#emlObject is an R object read in from a *.metadata.xml file formatted as EML using the EML::read_eml() function.
#outfile is the name of the file you want to write to, in quotes. Typically, "ReadMe.txt"

#example:
#emldoc<-EML::read_eml("ReadMeTest.xml", from="xml")
#write.readMe(emldoc, "ReadMe2.txt")

write.readMe<-function(emlObject, outfile){

  #get components:
  Ref<-get.DSRefID(emlObject) #Data Store Ref. ID
  doi<-get.DOI(emlObject) #DOI
  title<-get.title(emlObject) #title
  abstract<-get.abstract(emlObject) #abstract
  fileInfo<-get.fileInfo(emlObject) #names, sizes, descriptions
  start<-get.beginDate(emlObject) #content start date
  end<-get.endDate(emlObject) #content end date
  units<-get.parkUnits(emlObject) #park units where data were collected
  CUI<-get.CUI(emlObject) #controlled unclassified information status
  DRRdoi<-get.DRRdoi(emlObject) #DOI of accompanying DRR
  citation<-get.citation(emlObject) #citation

  #Write to the components to the textfile specified in "outfile".
  cat(paste0("ReadMe file for DataStore reference# ", Ref), file=outfile, sep="\n")

  cat(paste0("Digital Object Identifier (DOI): ", doi, "\n"), file=outfile, sep="\n", append=TRUE)

  cat("Title:", file=outfile, sep="\n", append=TRUE)

  cat(paste0(title, "\n"), file=outfile, sep="\n", append=TRUE)

  cat("Abstract:", file=outfile, sep="\n", append=TRUE)

  cat(abstract, file=outfile, sep="\n", append=TRUE)

  cat(paste0("\n", "Files:"), file=outfile, sep="\n", append=TRUE)

  cat(stargazer::stargazer(fileInfo, summary=FALSE, type="text"), file=outfile, sep="\n", append=TRUE)

  cat("\nContent:", file=outfile, sep="\n", append=TRUE)

  cat(paste0("Begin date: ", start), file=outfile, sep="\n", append=TRUE)

  cat(paste0("End date: ", end), file=outfile, sep="\n", append=TRUE)

  if(!is.na(units)){
  cat(units, file=outfile, sep="\n", append=TRUE)}
  if(is.na(units)){
    cat(paste0("Park Unit Connections: ", units), file=outfile, sep="\n", append=TRUE)
  }

  cat(paste0("Sensitivity: ", CUI), file=outfile, sep="\n", append=TRUE)

  cat(paste0("Data Quality: Information about production and quality of this product can be found in the Data Release Report (DRR) at: ", DRRdoi, "\n"), file=outfile, sep="\n", append=TRUE)

  cat("\nCitation:\n", file=outfile, sep="\n", append=TRUE)

  cat(citation, file=outfile, sep="\n", append=TRUE)
}

###############################################################################
####################### end write.readMe() ####################################
###############################################################################
