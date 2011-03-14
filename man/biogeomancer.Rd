\name{biogeomancer}

\alias{biogeomancer}

\title{Georeferencing}

\description{
A link to the biogeomancer georeferencing web-service. 
See \url{http://bg.berkeley.edu/latest/} for more information and a rich visual interface.
}

\usage{
biogeomancer(country = "", adm1 = "", adm2 = "", locality = "", singleRecord=TRUE, progress="text")
}

\arguments{
  \item{country}{Character. Country }
  \item{adm1}{Character. Name of the first-level administrative subdivsion. E.g. the State in the United States and India, the Province in China and Canada}
  \item{adm2}{Character. Name of the second-level administrative subdivsion. E.g. the county in the United States, and the district in India and China }
  \item{locality}{Character. Locality description}
  \item{singleRecord}{Boolean. If \code{TRUE}, the record with lowest uncertainty is selected when several records are returned}  
  \item{progress}{Character. Valid values are "" (no progress indicator), "text" (the default), "tcltk", and "windows" (on that platform only)}  
}

\value{
data frame with three columns: longitude, latitude, and uncertainty (see Wieczorek et al., 2004). The datum is always WGS84.
}


\references{ 
Guralnick, R.P., J. Wieczorek, R. Beaman, R.J. Hijmans and the BioGeomancer Working Group, 2006. BioGeomancer:  Automated georeferencing to map the world's biodiversity data. PLoS Biology 4: 1908-1909.
\url{http://dx.doi.org/10.1371/journal.pbio.0040381}

Wieczorek, J., Q. Guo and R.J. Hijmans, 2004. The point-radius method for georeferencing point localities and calculating associated uncertainty. 
International Journal of Geographic Information Science 18: 745-767.

\url{http://bg.berkeley.edu/latest/}
}

\author{Robert J. Hijmans}

\examples{
# biogeomancer(country='United States', adm1='California', adm2='Yolo', locality=c('Davis', 'Woodland'))
# biogeomancer(country='United States', adm1='California', adm2='Yolo', locality='1.5 miles North of Davis')
}

\keyword{ spatial }

