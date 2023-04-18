
#File upload:
# 1) User inputs path to data package
# 2) function reads data package file names
#     a. function checks for just .csv and one .xml
# 3) function loads the EML formatted .xml file
# 4) function extracts DS reference ID from metadata (DOI field)
# 5) For each file in the data package:
#    a. API call POST: Request token
#       Requires file name (new token for each file?)
#       Requires datastore reference id
#    b. API call PUT: upload file (in fragments?)
#       Requires token (which includes reference id info?)
#       requires path to file
#    c. Check each fragment for successful upload
#    d. Some sort of upload progress indicator?
#    d. Tell user upon successfully uploading each full file
# 6) Tell user upon successful completion of data package upload
# 7) Provide url to draft reference




library(httr)
library(jsonlite)
library(tidyverse)

#### next try using the PUT method (more complex, requires token, upload files
# in fragments and then check each frag for completion; re-run incompletefrags)

req <- httr::POST("https://irmadevservices.nps.gov/datastore-secure/v4/rest/Reference/2297221/UploadFile/TokenRequest",
                  httr::authenticate(":", "", "ntlm"),
                  httr::add_headers('Content-Type'='application/json'),
                  body =  "\"Book1.csv\"",
                  httr::verbose()
                  )
# status_code<-httr::stop_for_status(req)$status_code
# status_code
#[1] 200

#MIME encoding types: xml typically "application/xml" but also "text/xml"
#MIME encoding types: csv "text/csv"

r2 <- httr::PUT(url,
                httr::content_type("text/csv"),
                httr::authenticate(":", "", "ntlm"),
                httr::add_headers('Content-Range' = 'bytes 0-83/83'),
                body = curl::form_data(readBin("Book1.csv",
                                               "raw",
                                               file.info("Book1.csv")$size),
                                       type= "text/csv"), #type necessary for xml?
                httr::progress(type="up", con=""),
                httr::verbose()
                )


req <- httr::POST("https://irmadevservices.nps.gov/datastore-secure/v4/rest/Reference/2297221/UploadFile/TokenRequest",
                  httr::authenticate(":", "", "ntlm"),
                  httr::add_headers('Content-Type'='application/json'),
                  body =  "\"Book1.csv\"",
                  httr::verbose()
)
url<-req$headers$location
url
#[1] "https://irmadevservices.nps.gov/datastore-secure/v4/rest/Reference/2297221/UploadFile/d78f8a90-c162fa69"

Book1<-read.csv("Book1.csv")

fsize <- file.size("Book1.csv")
#[1] 83
toread <- file("Book1.csv", "rb") #rb open connection for reading in binary mode
data_read <- readBin(toread, integer(), size=1, n=(fsize/2), endian = "little") #read in data in binary mode

writeBin(data_read, "book1.bin", useBytes= TRUE)

r2 <- httr::PUT(url,
                httr::add_headers('Content-Range' = 'bytes 0-49/83'),
                httr::content_type("text/csv"),
                httr::authenticate(":", "", "ntlm"),
                body = curl::form_data(
                                readBin(toread, integer(),
                                        size=1, n=50, endian = "little"),
                                type="application/csv"),

                            #type="application/csv"),
                #httr::progress(type="up", con=""),
                httr::verbose()
)

r2 <- httr::PUT(url,
                httr::add_headers('Content-Range' = 'bytes 0-40/83'),
                httr::content_type("application/octet-stream"),
                httr::authenticate(":", "", "ntlm"),
                body = list("file"=curl::form_data("Book1.bin",
                  type="application/octet-stream"),
                  encode="multipart"),
                httr::progress(type="up", con=""),
                httr::verbose()
)


#/rest/Reference/{referenceID}/UploadFile/Status/{token}
url2<-"https://irmadevservices.nps.gov/datastore-secure/v4/rest/Reference/2297738/UploadFile/Status/91a029f0-1a2c8b0d"

req3 <- httr::GET(url)

req2
str(req2)
json<-httr::content(req2, "text")
json
getwd()
list.files(full.names = TRUE)
