\name{incidenceSample}
\Rdversion{1.1}
\alias{incidenceSample}
\alias{createDiscrete}

\title{
Generates incidence sample for discrete survival data
}
\description{
Generates incidence sample for discrete survival data.  Uses output from \code{create.discrete()} or user must match the naming structure.  For each event, a random sample from the risk set at the same time window is selected.
}
\usage{
incidenceSample(long.DATA, n.controls = 1)
createDiscrete(time, event, dataX, delta.upper = NULL, n.delta=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long.DATA}{
output from \code{create.discrete}
}
  \item{n.controls}{
Number of randomly sampled observations.  If set to 0, the function returns the entire long.DATA data frame.
}
  \item{time}{follow-up time}
  \item{event}{event indicator}
  \item{dataX}{data.frame with covariates}
  \item{delta.upper}{used to pass in already defined time windows}
  \item{n.delta}{number of time windows}
}

\value{
Returns a data frame containing the subsample
}

\author{ Eric C Polley \email{ecpolley@berkeley.edu} }

\seealso{
\code{\link{SuperLearner.hazard}}
}


\keyword{ models}

