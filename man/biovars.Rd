\name{biovars}

\docType{methods}

\alias{biovars}
\alias{biovars,matrix,matrix,matrix-method}
\alias{biovars,Raster,Raster,Raster-method}
\alias{biovars,vector,vector,vector-method}

\title{bioclimatic variables}

\description{
Function to create 'bioclimatic variables' from monthly climate data. 
}


\usage{
biovars(prec, tmin, tmax, ...) 
}
 

\arguments{
  \item{prec}{monthly precipitation data}
  \item{tmin}{monthly minimum temperature data}
  \item{tmax}{monthly maximum temperature data}
  \item{...}{ Additional arguments }
}

\details{
coming soon
}

\value{
Depending on the class of the input data, an object of class 'vector', 'matrix' or 'RasterBrick'
}


\author{
Robert J. Hijmans
}


\keyword{spatial}

