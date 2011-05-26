\name{latex.summary.CV.SuperLearner}
\alias{latex.summary.CV.SuperLearner}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
latex method for summary.CV.SuperLearner
}
\description{
convert output from summary.CV.SuperLearner into latex table.  Method for the \code{\link[Hmisc]{latex}} function in \code{Hmisc}
}
\usage{
\method{latex}{summary.CV.SuperLearner}(object, subset = TRUE, report = c("Risk", "SE", "Min", "Max"), addNamesTable = NULL, booktabs = TRUE, dec = 3, rowname = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object of class summary.CV.SuperLearner}
  \item{subset}{
logical. include the subset column
}
  \item{report}{
which columns of outcome to report.  should be a subset of \code{c("Risk", "SE", "Min", "Max")}}
  \item{addNamesTable}{
Additional naming matrix.  This should be a two column character matrix.  The first column is the name as it appears in the SL.library, the second column is the name as it should appear in the latex table.
}
  \item{booktabs}{
passed to \code{latex}
}
  \item{dec}{
passed to \code{latex}
}
  \item{rowname}{
passed to \code{latex}
}
  \item{\dots}{
additional arguments passed to \code{latex}
}
}

\value{
returns to output from \code{latex}
}

\author{ Eric C Polley \email{ecpolley@berkeley.edu} }

\keyword{utilities}
