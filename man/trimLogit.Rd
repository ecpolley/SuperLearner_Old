\name{trimLogit}
\Rdversion{1.1}
\alias{trim.logit}
\alias{trimLogit}

\title{
truncated-probabilities logit transformation 
}
\description{
computes the logit transformation on the truncated probabilities
}
\usage{
trimLogit(x, trim = 1e-05)
}

\arguments{
  \item{x}{
vector of probabilities
}
  \item{trim}{
value to truncate probabilities at. currently symmetric truncation (trim and 1-trim).
}
}

\value{
logit transformed values
}
\author{ Eric C Polley \email{ecpolley@berkeley.edu} }

\seealso{
\code{\link{SuperLearner}}
}
\examples{
x <- runif(10)
trimLogit(x,trim=0.01)
}

\keyword{models}