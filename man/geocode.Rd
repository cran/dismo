\name{geocode}

\alias{geocode}

\title{Georeferencing with Google}

\description{
A wrapper around the Google geocoding web-service. It returns 0 to n matches. It is important to be as precise as possible, e.g. always include the country in the locality description. 
}

\usage{geocode(x, boxes='', extent=NULL) }

\arguments{
  \item{x}{A vector of locality descriptions}
  \item{boxes}{Character. "", "only", or "one". It it is "", 0 to n localities and bounding boxes are returned. If it is 'only', only a single bounding box is returned (the union of all boxes) for each record in \code{x}. If it is 'one', a single bounding box will be returned that is the union of the bounding box for all records }
  \item{extent}{An Extent object, or an object that can be coerced to one, to bias the search towards that region}
}

\value{
matrix
}

\seealso{\code{\link[dismo]{biogeomancer}}}

\author{Robert J. Hijmans}

\examples{
# geocode(c('1600 Pennsylvania Ave NW, Washington DC', 'Luca, Italy', 'Kampala'))
# geocode(c('San Jose', 'San Jose, Mexico'))
# geocode(c('San Jose', 'San Jose, Mexico'), boxes='one')
}

\keyword{ spatial }

