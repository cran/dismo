\name{movingFun}

\alias{movingFun}

\title{Moving functions}

\description{
Helper function to compute 'moving' functions, such as the 'moving average'
}

\usage{
movingFun(x, n, fun=function(x){mean(x, na.rm=TRUE)}, type='around', circular=FALSE) 
}
 

\arguments{
  \item{x}{A vector}
  \item{n}{Size of the 'window', i.e. the number of sequential elements to use in the function}
  \item{fun}{A function like mean, min, max, sum}
  \item{type}{Character. One of 'around', 'to', or 'from'. The choice indicates which values should be used in the computation. The focal element is always used. If \code{type} is 'around', the other elements are before and after the focal element. Alternatively, you can select the elements preceding the focal element ('to') or those coming after it {'from'}. For example, to compute the movingFun with \code{n=3} for element 5 of a vector; 'around' used elements 4,5,6; 'to' used elements 3,4,5, and 'from' uses elements 5,6,7}
  \item{circular}{Logical. If \code{true}, the data are considered to have a circular nature (e.g. months of the year), and the last elements in vector \code{x} are used in the computation of the moving function of the first element(s) of the vector, and the first elements are used in the computation of the moving function for the last element(s)}
}


\value{
Vector
}

\author{
Robert J. Hijmans
}

\examples{
movingFun(1:12, 3, mean)
movingFun(1:12, 3, mean, 'to')
movingFun(1:12, 3, mean, 'from')
movingFun(1:12, 3, mean, circular=TRUE)

}

\keyword{spatial}

