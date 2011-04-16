\name{CVFolds}
\alias{CVFolds}

\title{
Generate list of row numbers for each fold in the cross-validation
}
\description{
Generate list of row numbers for each fold in the cross-validation
}
\usage{
CVFolds(V, N.all, shuffle, id, stratifyCV, Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{
number of folds
}
  \item{N.all}{
total sample size
}
  \item{shuffle}{
shuffle the row numbers before split
}
  \item{id}{
id variable
}
  \item{stratifyCV}{
should splits be stratified by the binary outcome
}
  \item{Y}{
outcome
}
}
\value{
\item{DATA.split}{
	list of length V with each element being the row numbers corresponding to the observations in the corresponding validation set.
}
}

\author{ Eric C Polley \email{ecpolley@berkeley.edu} }

\keyword{utilities}
