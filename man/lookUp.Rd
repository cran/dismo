\name{ lookup }

\alias{country}
\alias{adm}
\alias{alt}


\title{ lookup }

\description{ Look up geographic data for long/lat data }

\usage{
country(lonlat, radius=0)
adm(lonlat, radius=0, maxrows=1)
alt(lonlat)
}


\arguments{
  \item{lonlat}{ matrix with longitude and latitude values}
  \item{radius}{ radius around each point to consider}
  \item{maxrows}{ max nr or rows (records) to be returned }
}

\value{
Character or numeric
}

\author{ Robert J. Hijmans}

\keyword{ spatial }

