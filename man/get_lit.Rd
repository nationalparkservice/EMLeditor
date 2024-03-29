% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getEMLfunctions.R
\name{get_lit}
\alias{get_lit}
\title{Get literature cited}
\usage{
get_lit(eml_object)
}
\arguments{
\item{eml_object}{is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(\if{html}{\out{<filename>}}, from="xml").}
}
\value{
character string
}
\description{
get_lit prints bibtex fromated literature cited to the screen.
}
\details{
get_lit currently only supports bibtex formatted references. get_lit gets items from the \if{html}{\out{<literatureCited>}} tag and prints them to the screen.
}
\examples{
\dontrun{
get_lit(eml_object)
}
}
