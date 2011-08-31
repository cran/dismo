\name{pwdSample}

\alias{pwdSample}

\title{Pair-wise distance sampling}

\description{
Select pairs of points from two sets (without replacement) that have a similar distance to their nearest point in another set of points. 

For each point in "\code{fixed}", a random point is selected from "\code{sample}" that has a similar distance (as defined by \code{threshold}) to its nearest point in "\code{reference}" (note that these are likely to be different points in \code{reference}). If no such point within the threshold distance is found in \code{sample}, the point in \code{fixed} is dropped.

Hijmans (2011) proposes this sampling approach to remove 'spatial sorting bias' (\code{\link{ssb}}) from evaluation data used in cross-validation of presence-only species distribution models. In that context, \code{fixed} are the testing-presence points, \code{sample} the testing-absence (or testing-background) points, and \code{reference} the training-presence points.
}

\usage{
pwdSample(fixed, sample, reference, tr=0.33, n=1, lonlat=TRUE, warn=TRUE) 
}
 
\arguments{
  \item{fixed}{two column matrix (x, y) or (longitude/latitude) or SpatialPoints object, for point locations for which a pair should be found in \code{sample} }
  \item{sample}{as above for point locations from which to sample to make a pair with a point from \code{fixed}}
  \item{reference}{as above for reference point locations to which distances are computed}
  \item{n}{How many pairs do you want for each point in \code{fixed}}
  \item{tr}{ the threshold distance for a pair of points (one of \code{fixed} and one of \code{sample}) to their respective nearest point in \code{reference}. The absolute difference in distance between the candidate point pairs in \code{fixed} and \code{reference} (dfr) and the distance between candidate point pairs in \code{sample} and \code{reference} (dsr) must be smaller than \code{tr} * dfr. I.e. if the dfr = 100 km, and tr = 0.1, dsr must be between >90 and <110 km to be considered a valid pair.}
  \item{lonlat}{ Logical. Use \code{TRUE} if the coordinates are spherical (in degrees), and use \code{FALSE} if they are planar }
  \item{warn}{ Logical. If \code{TRUE} a warning is given if \code{nrow(fixed) < nrow(sample)} }
}

\value{
A matrix of nrow(fixed) and ncol(n), that indicates, for each point (row) in \code{fixed} which point(s) in \code{sample} it is paired to; or \code{NA} if no suitable pair was available.
}

\references{
Hijmans, R.J., 2011. Cross-validation of species distribution models: removing spatial sorting bias and calibration with a null-model. Forthcoming. 
}

\author{
Robert J. Hijmans
}


\seealso{\code{\link{gridSample}}}

\examples{
ref <- matrix(c(-54.5,-38.5, 2.5, -9.5, -45.5, 1.5, 9.5, 4.5, -10.5, -10.5), ncol=2)
fix <- matrix(c(-56.5, -30.5, -6.5, 14.5, -25.5, -48.5, 14.5, -2.5, 14.5, -11.5, -17.5, -11.5), ncol=2)
r <- raster()
extent(r) <- c(-110, 110, -45, 45)
r[] <- 1
set.seed(0)
sam <- randomPoints(r, n=50)

par(mfrow=c(1,2))
plot(sam, pch='x')
points(ref, col='red', pch=18, cex=2)
points(fix, col='blue', pch=20, cex=2)

i <- pwdSample(fix, sam, ref, lonlat=TRUE)
i
sfix <- fix[!is.na(i), ]
ssam <- sam[i[!is.na(i)], ]
ssam

plot(sam, pch='x', cex=0)
points(ssam, pch='x')
points(ref, col='red', pch=18, cex=2)
points(sfix, col='blue', pch=20, cex=2)

# try to get 3 pairs for each point in 'fixed'
pwdSample(fix, sam, ref, lonlat=TRUE, n=3)
}

\keyword{spatial}
