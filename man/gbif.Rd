\name{gbif}

\alias{gbif}

\title{
Data from GBIF
}

\description{
This function downloads species occurence records from the Global Biodiversity Information Facility (\href{http://www.gbif.org}{GBIF}) data portal. You can download either a single species (if you append a '*' to the species name) or a subspecies of comparable level. You can download the data for an entire genus by using \code{species='*'}. Before using this function, please first check the GBIF \href{http://data.gbif.org/terms.htm}{data use agreement}.
}


\usage{
gbif(genus, species="", geo=TRUE, sp=FALSE, removeZeros=TRUE, download=TRUE, getAlt=TRUE, feedback=3)
}
 
\arguments{
  \item{genus}{Character. genus name}
  \item{species}{Character. species name. Use '*' to download the entire genus. Append '*' to the species name to get all sub-taxa }
  \item{geo}{Logical. If \code{TRUE}, only records that have a georeference (longitude and latitude values) will be downloaded}
  \item{sp}{Logical. If \code{TRUE}, \code{geo} will be set to \code{TRUE} and a \link[sp]{SpatialPointsDataFrame} will be returned}
  \item{removeZeros}{Logical. If \code{TRUE}, all records that have a latiude OR longitude of zero will be removed if \code{geo==TRUE}, or set to \code{NA} if \code{geo==FALSE}. If \code{FALSE}, only records that have a latiude AND longitude that are zero will be removed / set to \code{NA}}.
  \item{download}{Logical. If \code{TRUE}, records will be downloaded, else only the number of records will be shown}
  \item{getAlt}{Logical. If \code{TRUE}, elevation data (4 character variables) will be processed into a single new numerical variable}
  \item{feedback}{Integer. Lower values give less feedback (0-no messages; 3-all messages)}
}


\value{
data frame 
}


\references{
\url{http://data.gbif.org/occurrences/}
}

\examples{
\dontrun{
# note the differences:

gbif('solanum', download=F)
gbif('solanum', '*', download=F)
gbif('solanum', 'acaule', download=F)
gbif('solanum', 'acaule f. acaule', download=F)
gbif('solanum', 'acaule*', download=F)

gbif('Batrachoseps', '*' , geo=F, down=F)
gbif('Batrachoseps', '*' , geo=T, down=F)
gbif('Batrachoseps', 'luciae', geo=T, down=F)
g <- gbif('Batrachoseps', 'luciae', geo=T)
plot(g$lon, g$lat)

gs <- gbif('Batrachoseps', 'luciae', sp=T)
plot(gs)
}
}

\author{
Robert J. Hijmans
}


\keyword{spatial}

