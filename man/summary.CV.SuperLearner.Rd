\name{summary.CV.SuperLearner}
\alias{summary.CV.SuperLearner}
\alias{print.summary.CV.SuperLearner}

\title{
Summary Function for Cross-Validated Super Learner
}
\description{
summary method for the \code{CV.SuperLearner} function
}
\usage{

\method{summary}{CV.SuperLearner}(object, \dots)

\method{print}{summary.CV.SuperLearner}(x, digits, \dots)
}

\arguments{
  \item{object}{
	an object of class "CV.SuperLearner", the result of a call to \code{CV.SuperLearner}.
}
  \item{x}{
	an object of class "summary.CV.SuperLearner", the result of a call to \code{summary.CV.SuperLearner}. 
}
  \item{digits}{
	the number of significant digits to use when printing.
}
  \item{\dots}{ additional arguments \dots}
}
\details{
Summary method for \code{CV.SuperLearner}.  Calculates the V-fold cross-validated estimate of either the mean squared error or the -2*log(L) depending on the loss function used.
}
\value{
\code{summary.CV.SuperLearner} returns a list with components
\item{call}{
	The function call from \code{CV.SuperLearner}
}
\item{method}{
	Describes the loss function used.  Currently either least squares of negative log Likelihood.
}
\item{V}{
	Number of folds
}
\item{Risk.SL}{
	Risk estimate for the super learner
}
\item{Risk.dSL}{
	Risk estimate for the discrete super learner (the cross-validation selector)
}
\item{Risk.library}{
	A matrix with the risk estimates for each algorithm in the library
}
\item{Table}{
	A table with the mean risk estimate and standard deviation across the folds for the super learner and all algorithms in the library
}
}

\author{ Eric C Polley \email{ecpolley@berkeley.edu} }

\seealso{
\code{\link{CV.SuperLearner}}
}


\keyword{methods}

