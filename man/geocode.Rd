\name{geocode}

\alias{geocode}

\title{Georeferencing with Google}

\description{
A wrapper around the Google geocoding web-service. It returns 0 to n matches. It is important to be as precise as possible, e.g. always include the country in the locality description. 

The purpose of using this function should be to display the locations on a map in a browser. You should check the Google terms of use \url{http://code.google.com/apis/maps/terms.html} to see if your usage of this function (and the underlying Google API) is permitted.
}

\usage{geocode(x, oneRecord=FALSE, extent=NULL, progress='') }

\arguments{
  \item{x}{A vector of locality descriptions}
  \item{oneRecord}{Logical. If \code{TRUE} a single record for each item in x is returned by averaging the coordinates and taking the union of all bounding boxes }
  \item{extent}{An Extent object, or an object that can be coerced to one, to bias the search towards that region}
  \item{progress}{Character. Valid values are "" (no progress indicator), "text" (the default) and "windows" (on that platform only)}    
}

\value{
matrix
}

\seealso{\code{\link[dismo]{biogeomancer}}}

\author{Robert J. Hijmans}

\examples{
\dontrun{
geocode(c('1600 Pennsylvania Ave NW, Washington DC', 'Luca, Italy', 'Kampala'))
geocode(c('San Jose', 'San Jose, Mexico'))
geocode(c('San Jose', 'San Jose, Mexico'), oneRecord=TRUE)
}
}

\keyword{ spatial }

