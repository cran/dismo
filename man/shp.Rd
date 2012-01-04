
\name{shapefile}

\alias{shapefile}

\title{
Read a shapefile
}

\description{
Simple wrapper around readOGR (rgdal package)
}


\usage{
shapefile(filename, verbose=FALSE)
}
 
\arguments{
  \item{filename}{Character. Full filename of a ESRI shapefile}
  \item{verbose}{Logical. If \code{TRUE}, information about the file is printed}
}

\value{
Spatial*DataFrame 
}

\author{
Robert J. Hijmans
}

\keyword{spatial}

