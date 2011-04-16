\name{print.SuperLearner}
\Rdversion{1.1}
\alias{print.SuperLearner}
\alias{print.CV.SuperLearner}
\alias{coef.SuperLearner}
\alias{coef.CV.SuperLearner}

\title{
print a SuperLearner object
}
\description{
reports the coefficients (weights) for each algorithm in the library
}
\usage{
\method{print}{SuperLearner}(x, \dots)
\method{print}{CV.SuperLearner}(x, \dots)
\method{coef}{SuperLearner}(object, \dots)
\method{coef}{CV.SuperLearner}(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A SuperLearner object
}
  \item{object}{
A SuperLearner object
}
  \item{\dots}{
additional arguments
}
}
\details{
prints the coefficients from the SuperLearner model.  The predicted values from \code{newX} can be found in \code{x\$SL.predict}
}
\value{
prints a coefficient vector and the cross-validated risk for each algorithm in the library
}


\author{ Eric C Polley \email{ecpolley@berkeley.edu} }
\keyword{methods}



