% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HDRSeeds.R
\name{HDRSeeds}
\alias{HDRSeeds}
\title{HDR Seed Generator}
\usage{
HDRSeeds(n, entity = 1, var_id = "generate", option1 = 0, option2 = 0)
}
\arguments{
\item{n}{An inetger equal to the number of SIPs to generate seeds for.}

\item{entity}{An integer between 1 and 100 million relating to the entity
making the simulation.}

\item{var_id}{An integer between 1 and 100 million to be the starting value
for each independent variable seed.}

\item{option1}{An optional integer between 1 and 100 million to serve as a
secondary ID.}

\item{option2}{An optional integer between 1 and 10 million to serve as a
tertiary ID.}
}
\value{
Data.frame containing integer values for simulation seeds.
}
\description{
Helper function to create the seeds data.frame to be used by the SIPlibrary3
function.
}
\examples{
seeds <- HDRSeeds(5)
seedsspecific <- HDRSeeds(5, 500, 1, 600, 700)

}
