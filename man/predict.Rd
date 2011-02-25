\name{predict}

\docType{methods}

\alias{predict}
\alias{predict,Bioclim-method}
\alias{predict,Domain-method}
\alias{predict,Mahalanobis-method}
\alias{predict,MaxEnt-method}
\alias{predict,ConvexHull-method}

\title{Distribution model predictions}

\description{
Make a RasterLayer with a prediction based on a model object of class Bioclim, Domain, MaxEnt, ConvexHull, or Mahalanobis. Provide a Raster* object with the independent variables. The layerNames in the Raster* object should include those expected by the model. 
}

\section{Methods}{
\describe{
\code{predict(object, x, ext=NULL, filename='', progress='text', ...)}
  
\tabular{rll}{  
  \tab  \code{object}  \tab A fitted model of class Bioclim, Domain, MaxEnt, ConvexHull, or Mahalanobis (classes that inherit from DistModel)\cr
  \tab  \code{x} \tab A Raster* object or a data.frame\cr
  \tab  \code{ext} \tab An \link[raster]{extent} object to limit the prediction to a sub-region of \code{x}. Or an object that can be coerced to an Extent object by \code{\link[raster]{extent}}; such as a Raster* or Spatial* object \cr
  \tab  \code{filename}  \tab Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot \cr
  \tab \code{progress} \tab Character. Valid values are "" (no progress bar), "text", "tcltk", and "windows" (on that platform only) \cr
  \tab  \code{...}  \tab Additional model specific arguments and arguments for file writing (see below)\cr
}

The following additional arguments can be passed, to replace default values, when writing the results to a file
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If TRUE, "filename" will be overwritten if it exists \cr
 }


For \link[dismo]{maxent} models, there is an additional argument \code{'args'} used to pass arguments (options) to the maxent software. See the help page for \code{ \link[dismo]{maxent}} for more information.
 
For \link[dismo]{bioclim} models, there is an additional argument \code{'tails'} which you can use to ignore the left or right tail of the percentile distribution for a variable. If supplied, tails should be a character vector with a length equal to the number of variables used in the model. Valid values are "both" (the default), "low" and "high". For example, if you have a variable x with an observed distribution between 10 and 20 and you are predicting the bioclim value for a value 25, the default result would be zero (outside of all observed values); but if you use tail='low', the high (right) tail is ignored and the value returned will be 1.
 
 }}

\value{
A RasterLayer or, if \code{x} is a matrix), a vector. 
}

\seealso{
For spatial predictions with GLM, GAM, BRT, randomForest, etc., see \link[raster]{predict} in the Raster package.

To fit a model that can be used with this predict method, see \code{ \link[dismo]{maxent}, \link[dismo]{bioclim}, \link[dismo]{mahal}, \link[dismo]{domain}, \link[dismo]{convHull}}
}

\author{ Robert J. Hijmans}

\examples{
logo <- stack(system.file("external/rlogo.grd", package="raster"))
pts <- matrix(c(48, 48, 48, 53, 50, 46, 54, 70, 84, 85, 74, 84, 95, 85, 66, 42, 26, 4, 19, 17, 7, 14, 26, 29, 39, 45, 51, 56, 46, 38, 31, 22, 34, 60, 70, 73, 63, 46, 43, 28), ncol=2)
b <- bioclim(logo, pts)
# prediction for a sub-region
e <- extent(30,90,20,60)
p <- predict(b, logo, progress='text', ext=e)
plot(p)
}

\keyword{methods}
\keyword{spatial}
