\name{CV.SuperLearner.hazard}
\Rdversion{1.1}
\alias{CV.SuperLearner.hazard}

\title{
Computes the V-fold cross-validated hazard estimation using binary Super Learner
}
\description{
V-fold Cross validated hazard estimation using binary Super Learner
}
\usage{
CV.SuperLearner.hazard(N.delta, n.controls=1, time.df=5, time, X, SL.library, outside.V=20, inside.V = 20, shuffle=FALSE, verbose=FALSE, family=binomial(), method="NNLS", id=NULL, save.fit.library=FALSE, trim.logit=0.0001, discreteTime = TRUE, obsWeights = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N.delta}{
Discrete survival process.  often created by \code{create.discrete}
}
  \item{n.controls}{
number of randomly sampled observations from risk set using \code{incidence.sample}.  Set to 0 for entire sample.
}
  \item{time.df}{
degrees of freedom for the \code{gam} model.  Currently not implemented
}
  \item{time}{
the time variable.  often \code{delta.u} from \code{create.discrete}
}
  \item{X}{
the baseline variables.  Do not include the outcome nor time
}
  \item{SL.library}{
algorithms for binary Super Learner
}
\item{outside.V}{
  An integer for the number of folds to split the data into 
}
\item{inside.V}{
  An integer for the number of folds each Super Learner should use
}
  \item{shuffle}{
shuffle rows in data frame before creating cross validation folds
}
  \item{verbose}{
more detailed output
}
  \item{family}{
binomial
}
  \item{discreteTime}{ currently not implemented}
  \item{method}{
Loss function for combining prediction in the library.  Currently either "NNLS" (the default) or "NNloglik".  NNLS is non-negative least squares and will work for both gaussian and binomial.  NNloglik is a non-negative binomial likelihood maximization
}
  \item{id}{
subject identification variable
}
  \item{save.fit.library}{
logical variable for saving the fit of each algorithm in \code{SL.library}
}
  \item{trim.logit}{ Only used if \code{method="NNloglik"}.  specifies a truncation level for the logit function for stability.}
  \item{obsWeights}{ observation weights}
}

\value{
\item{CV.fit.SL}{A list containing the output from each \code{SuperLearner}}
\item{pred.SL}{The V-fold cross-validation super learner predictions for the outcome.  These can be used to estimate the honest cross-validated risk}
\item{pred.discreteSL}{ The V-fold cross-validated discrete super learner prediction for the outcome.  The discrete super learner selects the algorithm with the minimum internal cross-validated risk estimate.  See output value \code{whichDiscreteSL} for the algorithm name associated with each fold}
\item{whichDiscreteSL}{ The prediction algorithm selected in each outside V fold as the discrete super learner}
\item{pred.library}{The V-fold cross-validation predictions for the outcome from all algorithms in the library}
\item{coef.SL}{ a matrix of coefficients in the SuperLearner across the V folds }
\item{folds}{ a list with the cross-validation splits}
\item{call}{ the function call}
}
\author{ Eric C Polley \email{ecpolley@berkeley.edu} }

\seealso{
\code{\link{SuperLearner.hazard}}
}

\keyword{models}