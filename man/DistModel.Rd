\name{DistModel-class}

\Rdversion{1.1}

\docType{class}

\alias{DistModel-class}

\title{Class "DistModel"}
\description{
Parent class to store distribution modeling results. This is a virtual Class, no objects may be created from it. Several other classes inherit from it, including Bioclim, Domain, MaxEnt.}


\section{Slots}{
  \describe{
    \item{\code{presence}:}{presence data used}
    \item{\code{absence}:}{absence or background data used}
    \item{\code{hasabsence}:}{Logical indicating whether there is any absence data}
  }
}


\author{
Robert J. Hijmans
}

\keyword{classes}
