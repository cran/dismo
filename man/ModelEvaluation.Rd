\name{ModelEvaluation-class}
\Rdversion{1.1}
\docType{class}

\alias{ModelEvaluation-class}

\title{Class "ModelEvaluation"}
\description{
Class to store results of model cross-validation with presence/absence data (0/1 data)
}

\section{Slots}{
  \describe{
    \item{\code{presence}:}{presence data used }
    \item{\code{absence}:}{absence data used}
    \item{\code{np}:}{number of presence points}
    \item{\code{na}:}{number of absence points}
    \item{\code{auc}:}{Area under the receiver operator (ROC) curve}
    \item{\code{pauc}:}{ p-value for AUC}
    \item{\code{cor}:}{ Correlation coefficient}
    \item{\code{pcor}:}{p-value for correlation coefficient }
    \item{\code{t}:}{ vector of thresholds used to compute confusion matrices }
    \item{\code{confusion}:}{ confusion matrices }
    \item{\code{prevalence}:}{ prevalence }
    \item{\code{ODP}:}{ tbd }
    \item{\code{CCR}:}{ tbd }
    \item{\code{TPR}:}{ True positive rate }
    \item{\code{TNR}:}{ True negative rate }
    \item{\code{FPR}:}{ False positive rate }
    \item{\code{FNR}:}{ False negative rate }
    \item{\code{PPP}:}{ tbd }
    \item{\code{NPP}:}{ tbd }
    \item{\code{MCR}:}{ tbd }
    \item{\code{OR}:}{ tbd }
    \item{\code{kappa}:}{ Cohen's kappa }
  }
}

\seealso{ 
\code{\link[dismo]{evaluate}} 
}

\author{
Robert J. Hijmans
}

\keyword{classes}
