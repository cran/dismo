# based on function Istat in package SDMTools by Jeremy VanDerWal
# adapted by Robert J. Hijmans 
# Date : October 2012
# Version 1.0
# Licence GPL v3


nicheOverlap <- function (x, y, mask=TRUE, checkNegatives=TRUE) {
	s <- stack(x, y)

	# to assure that both have the same NA cells
	if (mask) {
		s <- mask(s,  x * y)
	}
	
	if (checkNegatives) {
		minv <- cellStats(s, 'min', na.rm=TRUE)
		if (any(minv < 0)) {
			stop('values of "x" and "y" should be non-negative')
		}
	}
	
    cs <- cellStats(s, 'sum', na.rm=TRUE)
	r <- overlay(s, fun=function(i,j) (sqrt(i / cs[1]) - sqrt(j / cs[2] ))^2)
	1 - 0.5 * sqrt(cellStats(r, 'sum', na.rm=TRUE))
}

