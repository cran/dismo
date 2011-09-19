\name{evaluate}

\alias{evaluate}


\title{ Model evaluation }

\description{ 
Cross-validation of models with presence/absence data. Given a vector of presence and a vector of absence values (or a model and presence and absence points and predictors), confusion matrices are computed (for varying thresholds), and model evaluation statistics are computed for each confusion matrix / threshold. See the description of class \code{\link{ModelEvaluation-class}} for more info.
}

\usage{
evaluate(p, a, model, x, tr, ...)
}
 
\arguments{
  \item{p}{presence points (x and y coordinates or SpatialPoints* object). 
  
          Or, if \code{x} is missing, values at presence points
		  
		  Or, a matrix with values to compute predictions for}
		   
  \item{a}{absence points (x and y coordinates or SpatialPoints* object). 
  
			Or, if \code{x} is missing, values at presence points.
			
			Or, a matrix with values to compute predictions for}
			
  \item{model}{any fitted model, including objects inherting from 'DistModel'; not used when \code{x} is missing}

  \item{x}{Optional. Predictor variables (object of class Raster*). If present, \code{p} and \code{a} are interpreted as (spatial) points }
  
  \item{tr}{Optional. a vector of threshold values to use for computing the confusion matrices}
  
  \item{...}{Additional arguments for the predict function}  
}

\value{
An object of \code{\link{ModelEvaluation-class}}
}

\author{
Robert J. Hijmans
}

\references{
Fielding, A.H. and J.F. Bell, 1997. A review of methods for the assessment of prediction errors in conservation presence/absence models. Environmental Conservation 24:38-49
}

\examples{
# p = the predicted value for 50 known cases (locations) with presence of the phenomenon (species)
p = rnorm(50, mean=0.7, sd=0.3)
# b = the predicted value for 50 known cases (locations) with absence of the phenomenon (species)
a = rnorm(50, mean=0.4, sd=0.4)
e = evaluate(p=p, a=a)

# threshold at maximum kappa
e@t[which.max(e@kappa)]

# threshold at maximum of the sum of the sensitivity (true positive rate) and specificity (true negative rate)
e@t[which.max(e@TPR + e@TNR)]

plot(e, 'ROC')
plot(e, 'TPR')
boxplot(e)
density(e)

str(e)
}

\keyword{spatial}

