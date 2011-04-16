\name{screen.template}
\alias{rank.p.glm}
\alias{screen.template}
\alias{screen.glmP}
\alias{All}
\alias{screen.glmRank}
\alias{screen.randomForest}
\alias{screen.SIS}
\alias{screen.corP}
\alias{screen.corRank}
\alias{screen.glmnet}

\title{ Template file for creating new screening functions in the Super Learner }
\description{
  univariate generalized linear model p-value ranking, used internally for \code{SuperLearner} 
}
\usage{
screen.template(Y.temp, X.temp, family, obsWeights, id, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y.temp}{ outcome}
  \item{X.temp}{ data }
  \item{family}{family for \code{glm} }
  \item{obsWeights}{ observation weights}
  \item{id}{ cluster identification}
  \item{\dots}{ often helpful to include \dots for additional arguments}
}

\value{
  \item{whichVariable}{
  A logical vector with the length equal to the number of columns in \code{X.temp}.  TRUE indicates the variable (column of X.temp) should be included.
}

}

\author{ Eric C Polley \email{ecpolley@berkeley.edu} }

\examples{
screen.template
}
\keyword{misc}
