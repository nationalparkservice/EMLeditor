% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getEMLfunctions.R
\name{get_begin_date}
\alias{get_begin_date}
\title{returns the first date}
\usage{
get_begin_date(eml_object)
}
\arguments{
\item{eml_object}{is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(\if{html}{\out{<filename>}}, from="xml").}
}
\value{
a text string
}
\description{
get_begin_date returns the date of the earliest data point in the data package
}
\details{
returns the date from the \if{html}{\out{<beginDate>}} tag. Although dates should be formatted according to ISO-8601 (YYYY-MM-DD) it will also check for a few other common formats and return the date as a text string: "DD Month YYYY"
}
\examples{
\dontrun{
get_begin_date(eml_object)
}
}
