\name{predict.SuperLearner}
\alias{predict.SuperLearner}
\title{Predict method for SuperLearner object}
\description{
  obtains predictions on a new data set from a SuperLearner model fit.  May require the original data if one of the library algorithms uses the original data in its predict method.
}
\usage{
predict.SuperLearner(object, newdata, X = NULL, Y = NULL, ...)
}
\arguments{
  \item{object}{ output from \code{SuperLearner()} }
  \item{newdata}{ New X values for prediction}
  \item{X}{ original data set used to fit \code{object} }
  \item{Y}{ original outcome used to fit \code{object} }
  \item{\dots}{ often helpful to include \dots}
}
\details{
  if \code{newdata} is omitted the predicted values from \code{object} are returned.  Each algorithm in the Super Learner library needs to have a corresponding prediction function with ``predict.'' prefixed onto the algorithm name (e.g. \code{predict.SL.glm} for \code{SL.glm}).
}
\value{
	\item{fit}{ Predicted values for Super Learner}
	\item{fit.library}{Predicted values for each algorithm in library}
	\item{cand.names}{ A list of the algorithms used in the super learner}
}

\references{ 
	van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2008) Super Learner, \emph{Statistical Applications of Genetics and Molecular Biology}, \bold{6}, article 25. \url{http://www.bepress.com/sagmb/vol6/iss1/art25}  
}
\author{ Eric C Polley \email{ecpolley@berkeley.edu} }

\seealso{ \code{\link{SuperLearner}}}

\keyword{models}

