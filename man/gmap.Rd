\name{gmap}

\alias{gmap}
\alias{Mercator}

\title{Get a Google map}

\description{
Retrieve a 'Google Map' that can be used as a background for plotting points and other spatial data.

You should check the terms of service before you use these maps: \url{http://code.google.com/apis/maps/terms.html}

The projection of the returned maps is "Mercator" and your data may need to be transformed before you can plot on top of these maps. You can use the \code{Mercator} function for that, and to transform map coordinates back to longitude/latitude.

This function uses the Google static maps web-service, and is based on functions by Markus Loecher for the \code{RgoogleMaps} package. 
}

\usage{
gmap(x, exp=1, type='terrain', filename='', ...) 
Mercator(p, inverse = FALSE) 
}

\arguments{
  \item{x}{A textual locality description, or an Extent object or an object that can be coerced to one (e.g. a Raster* or Spatial* object)}
  \item{exp}{An expansion factor to enlarge (by multiplication) the extent specified by \code{x} }
  \item{type}{Character. Choose from 'roadmap', 'satellite', 'hybrid', 'terrain' }
  \item{filename}{Character. Filename (optional). You can open the resulting file in a GIS program }
  \item{...}{Graphical parameters }
  
  \item{p}{Points. A two-column matrix, or a SpatialPoints object}
  \item{inverse}{ Should the inverse projection be done (i.e. from Mercator to longitude/latitude }  
}

\value{
A RasterLayer
}

\note{
If argument \code{x} is a textual locatlity description, the \code{\link[dismo]{geocode}} function is used to retrieve the extent that should be mapped.

Change the type to 'roadmap' if the map returned says "sorry we have no imagery here"; or use a larger extent.

The returned RasterLayer has a Mercator projection. To plot points (or lines or polygons) on top of it, these need to be transformed first.

A matrix of lon/lat data can be transformed with the Mercator function used in the example below. 'Spatial*' objects can be transformed with \code{spTransform}
p <- spTransform(x, "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
}

\author{Robert Hijmans, based on code by Markus Loecher, Sense Networks <markus at sensenetworks.com> in the RgoogleMaps package}

\examples{
\dontrun{
if(require(rgdal)){
# get a map using names
g = gmap('Australia')
plot(g, inter=TRUE)
gs = gmap('Sydney, New South Wales, Australia', type='satellite')
plot(gs, inter=TRUE)
gs = gmap('Sydney, Australia', type='satellite', exp=3)
plot(gs, inter=TRUE)
gs = gmap('Sydney, Australia', type='hybrid', exp=8)
plot(gs, inter=TRUE)

# from a maxtrix with lon/lat points
x = runif(30)*10 + 40
y = runif(30)*10 - 20
xy = cbind(x, y)
g = gmap(xy, type='hybrid')
plot(g, inter=TRUE)
points(Mercator(xy) , col='red', pch=20)

# or from an Extent object
e = extent( -121.9531 , -120.3897 , 35.36 , 36.61956 )
# you can also get an Extent object by clicking on the map twice after using:
# drawExtent()
r = gmap(e)
plot(r, interpolate=TRUE)

# transform points to Mercator for plotting on top of map:
pt <- matrix(c(-121, 36), ncol=2)
ptm <- Mercator(pt)
points(ptm, cex=3, pch=20, col='blue')
Mercator(ptm, inverse=TRUE)

# transform Spatial objects to Mercator for plotting on top of map
# here for points, but particularly relevant for lines and polygons
pt <- data.frame(pt)
coordinates(pt) = ~X1 + X2
pt@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
ptm2 <- spTransform(pt, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
points(ptm, col='red', pch='x', cex=3)
}
}
}

\keyword{ spatial }

