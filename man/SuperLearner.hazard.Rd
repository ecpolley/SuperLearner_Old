\name{SuperLearner.hazard}
\Rdversion{1.1}
\alias{SuperLearner.hazard}

\title{
Hazard estimation using binary Super Learner
}
\description{
Hazard estimation using binary Super Learner
}
\usage{
SuperLearner.hazard(N.delta, n.controls = 1, time.df = 5, time, X, newX, time.newX, SL.library, V = 20, shuffle = FALSE, verbose = FALSE, family = binomial(), method="NNLS", id = NULL, save.fit.library = TRUE, trim.logit=0.0001, discreteTime = TRUE, obsWeights=NULL)
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
  \item{newX}{
test data frame
}
  \item{time.newX}{
time points in the newX data frame
}
  \item{SL.library}{
algorithms for binary Super Learner
}
  \item{V}{
number of cross validation folds for the Super Learner
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
\item{SL.predict}{ predicted probability of having an event}
\item{cand.names}{ gives a list of all algorithms in the library, including any screening algorithms}
\item{SL.library}{ gives a list of all algorithms in the library}
\item{init.coef}{ coefficient estimates from the non-negative least squares }
\item{coef}{ Coefficient estimates in the super learner }
\item{library.predict}{ predicted values from all candidates in \code{SL.library} estimated on the entire training sample}
\item{cv.risk}{ V-fold cross-validated risk estimates for each algorithm in the library}
\item{newZ}{ predicted values from all candidates in \code{SL.library} estimated in the V-fold cross validation}
 \item{fit.library}{ a list containing the fit of each model in SL.library on the full data set}
\item{id}{ cluster identification variable}
\item{namesX}{ The variable names in the X data frame}
\item{DATA.split}{ a list with the cross-validation splits}
\item{method}{ method used to estimate weight for prediction in library}
\item{whichScreen}{ a logical matrix indicating which variables were selected for each screening algorithm}
  \item{trim.logit}{ Only used if \code{method="NNloglik"}.  specifies a truncation level for the logit function for stability.}
\item{errorsInCVLibrary}{ A logical vector with length equal to the number of algorithms in the library.  equal to 1 if the corresponding algorithm failed on any of the cross-validation splits}
\item{errorsInLibrary}{ A logical vector with length equal to the number of algorithms in the library.  equal to 1 if the corresponding algorithm failed on full data}
}
\author{ Eric C Polley \email{ecpolley@berkeley.edu} }

\seealso{
\code{\link{SuperLearner}}
}


\keyword{models}