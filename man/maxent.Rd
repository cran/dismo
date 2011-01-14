\name{maxent}

\alias{maxent}

\alias{maxent,missing,missing-method}
\alias{maxent,Raster,ANY-method}
\alias{maxent,SpatialGridDataFrame,ANY-method}
\alias{maxent,data.frame,vector-method}

\title{Maxent}

\description{
Build a "MaxEnt" (Maximum Entropy) species distribution model (see references below). The function uses environmental data for locations of known presence and for a large number of 'background' locations. Environmental data can be extracted from raster files. The result is a model object that can be used to predict the suitability of other locations, for example, to predict the entire range of a species. 

This function uses the MaxEnt species distribution model software, which is a java program that you can download from \url{http://www.cs.princeton.edu/~schapire/maxent/}. Put the file 'maxent.jar' in the 'java' folder of this package. That is the folder returned by \code{system.file("java", package="dismo")}. You need maxent version 3.3.3b or higher. Please note that this program (maxent.jar) cannot be redistributed or used for commercial or for-profit purposes.
}

\usage{
maxent(x, p, ...)
}

\arguments{
\item{x}{Predictors. Raster* object or SpatialGridDataFrame, containing grids with predictor variables. These will be used to extract values from for the point locations. \code{x} can also be a data.frame, in which case each column should be a predictor variable and each row a presence or background record. }

\item{p}{Occurrence data. This can be a data.frame, matrix, SpatialPoints* object, or a vector. If \code{p} is a data.frame or matrix it represents a set of point locations; and it must have two columns with the first being the x-coordinate (longitude) and the second the y-coordinate (latitude). Coordinates can also be specified with a SpatialPoints* object (defined in the sp package). Background points are sampled randomly from the cells that are not NA in the first predictor variable, unless background points are specified with an additional argument \code{a} (see Details). If \code{x} is a data.frame, \code{p} should be a vector with a length equal to \code{nrow(x)} and contain 0 (background) and 1 (presence) values, to indicate which records (rows) in data.frame \code{x} are presence records, and which are background records. }

\item{...}{Additional arguments. See Details}
}

\details{
Additional arguments:
\tabular{rll}{
\tab \code{a} \tab Background points. Only used if \code{p} is not missing, and not a vector.  \cr
\tab \code{factors} \tab Which (if any) variables should be considered as categorical? Either by (layer)name or by index. Only used when argument \code{x} is a Raster* object because it is not needed in other cases as you can set the appropriate class to the variables in the data.frame \cr 
\tab \code{args} \tab Additional argument that can be passed to Maxent. See the Maxent help. For this function, only the arguments relevant to model fitting are used. (e.g. there is no point in using args='outputformat=raw' when *fitting* the model; but you can use these in the \code{predict} functions. Some others (e.g. outputfiletype) do not apply at all to the R implementation (the "predict" function has its own arguments for that) \cr
}

if you want to give maxent (the Java virtual machine that runs it) more memory, you do that by running something like this (for 1 GB) \bold{before} you load the dismo library.

\code{options(java.parameters = "-Xmx1g" )} 
}


\value{
A RasterLayer object or a vector. 
}

\seealso{
\code{\link[dismo]{predict}}
}

\author{Steven Phillips and Robert J. Hijmans}

\references{

\url{http://www.cs.princeton.edu/~schapire/maxent/}

Steven J. Phillips, Miroslav Dudik, Robert E. Schapire, 2004.  A maximum entropy approach to species distribution modeling. Proceedings of the Twenty-First International Conference on Machine Learning. p. 655-662.

Steven J. Phillips, Robert P. Anderson, Robert E. Schapire, 2006. Maximum entropy modeling of species geographic distributions. Ecological Modelling 190:231-259. 

Jane Elith, Steven J. Phillips, Trevor Hastie, Miroslav Dudik, Yung En Chee, Colin J. Yates, 2011. A statistical explanation of MaxEnt for ecologists. Diversity and Distributions 17:43-57. \url{http://dx.doi.org/10.1111/j.1472-4642.2010.00725.x}
}

\examples{
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if (file.exists(jar)) {

# get predictor variables
predictors <- stack(list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), pattern='grd', full.names=TRUE ))
#plot(predictors)

# file with presence points
occurence <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
occ <- read.table(occurence, header=TRUE, sep=',')[,-1]

# witholding a 20% sample for testing 
fold <- kfold(occ, k=5)
occtest <- occ[fold == 1, ]
occtrain <- occ[fold != 1, ]

# fit model, biome is a categorical variable
me <- maxent(predictors, occtrain, factors='biome')
# see the maxent results in a browser:
# me

# use "args"
# me2 <- maxent(predictors, occtrain, factors='biome', args=c("-J", "-P"))

# plot showing importance of each variable
plot(me)

# predict to entire dataset
r <- predict(me, predictors, progress='text') 
# r <- predict(me, predictors, progress='text', args=c("outputformat=raw"))

plot(r)
points(occ)

#testing
# background data
bg <- randomPoints(predictors, 1000)

#simplest way to use 'evaluate'
e1 = evaluate(me, p=occtest, a=bg, x=predictors)

# alternative 1
# extract values
pvtest <- data.frame(xyValues(predictors, occtest))
avtest <- data.frame(xyValues(predictors, bg))

e2 = evaluate(me, p=pvtest, a=avtest)

# alternative 2 
# predict to testing points 
testp <- predict(me, pvtest) 
head(testp)
testa <- predict(me, avtest) 

e3 = evaluate(p=testp, a=testa)
e3
plot(e3, 'ROC')
}
}

\keyword{spatial}

