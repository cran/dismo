\name{gmap}

\alias{gmap}
\alias{Mercator}


\title{Get a Google map}

\description{
Regrieve a 'Google Map' from the Google static maps webservice. This function is a slightly modified version from a function in the RgoogleMaps package that provides more functionality. The projection of these maps is "Mercator". The Mercator function is a helper function to plot points on top of the map, or to transform map-coordinates back to long/lat.
}

\usage{
gmap(x, exp=1, type='terrain', filename='', ...) 
Mercator(p, inverse = FALSE) 
}

\arguments{
  \item{x}{An Extent object or an object that can be coerced to one (e.g. a Raster object)}
  \item{exp}{An exansion factor to enlarge (by mutiplication) the extent specified by \code{x} }
  \item{type}{Character. Choose from 'roadmap', 'sattellite', 'hybrid', 'terrain' }
  \item{filename}{Character. filename (optional) }
  \item{...}{Graphical parameters }
  \item{p}{Points. A two-column matrix, or a SpatialPoints object}
  \item{inverse}{ Should the inverse projection be done (i.e. from Mercator to longitude/latitude }  
}

\value{
A RasterLayer
}

\note{
Note that the returned RasterLayer has a Mercator projection. To plot points (or lines or polygons) on top of it, these need to be transformed first.

A matrix of lon/lat data can be transformed with the function used in the example. 'spatial*' objects can be transformed with spTransform
p <- spTransform(x, "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
}

\author{Based on code by Markus Loecher, Sense Networks <markus at sensenetworks.com>; adapted by Robert J. Hijmans}

\examples{
e = extent( -121.9531 , -120.3897 , 35.36 , 36.61956 )
#r = gmap(e)
#plot(r)

# transform points to Mercator for plotting on top of map:
pt <- matrix(c(-121, 36), ncol=2)
ptm <- Mercator(pt)
#points(ptm, cex=3)
Mercator(ptm, inverse=TRUE)

# transform Spatial objects to Mercator for plotting on top of map
# here for points, but particularly relevant for lines and polygons
if (require(rgdal)) {
pt <- data.frame(pt)
coordinates(pt) = ~X1 + X2
pt@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
ptm2 <- spTransform(pt, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
#points(ptm, col='red', pch='x', cex=3)
}
}


\keyword{ spatial }

