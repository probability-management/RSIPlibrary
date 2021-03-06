% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SIPlibrary2.R
\name{SIPlibrary2}
\alias{SIPlibrary2}
\title{SIPLibrary 2.0 Generator}
\usage{
SIPlibrary2(DATAFRAME, filename, author, METADF = NULL)
}
\arguments{
\item{DATAFRAME}{Data.frame containing SIPs.}

\item{filename}{Character, ending in ".xlsx"}

\item{author}{Character.}

\item{METADF}{Data.frame with named rows of same length as DATAFRAME.}
}
\value{
Completion notification, file output to WD
}
\description{
Outputs a data.frame and an optional data.frame containing metadata to a .xlsx
format SIPmath Tools readable library file. Allows a user to easily output R
SIPs to Excel users.
}
\examples{
testdf <- data.frame(SIP_1 = rnorm(5000),
SIP_2 = rnorm(5000),
SIP_3 = rnorm(5000),
SIP_4 = rnorm(5000),
SIP_5 = rnorm(5000))

testmeta <- sapply(testdf, FUN = summary)

SIPlibrary2(testdf, "finaltesting.xlsx", "Aaron Brown", testmeta)

}
