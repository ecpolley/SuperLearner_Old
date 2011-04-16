\name{listWrappers}
\alias{listWrappers}

\title{
list all wrapper functions in SuperLearner
}
\description{
will list all wrapper functions in \code{SuperLearner} package
}
\usage{
listWrappers(what = "both")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{what}{
can be \code{both} for both prediction algorithms and screening algorithms, \code{SL} for the prediction algorithms, or \code{screen} for the screening algorithms, or anything else will return a list of all functions in the \code{SuperLearner} package
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{ Eric C Polley \email{ecpolley@berkeley.edu} }
% \note{
% %%  ~~further notes~~
% }

\seealso{
\code{\link{SuperLearner}}
}
\examples{
listWrappers(what = "SL")
listWrappers(what = "screen")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

