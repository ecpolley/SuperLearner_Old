\name{CV.SuperLearner}
\alias{CV.SuperLearner}

\title{computes V-fold cross validation of the Super Learner}
\description{
  Computes the V-fold cross-validation estimates from the Super Learner.  The function splits the data into V folds and calls \code{SuperLearner}.
}
\usage{
CV.SuperLearner(Y, X, SL.library, outside.V = 20, inside.V = 20, shuffle = TRUE, verbose = FALSE, family = gaussian(), method="NNLS", id=NULL, save.fit.library=FALSE, trim.logit=0.001, obsWeights= NULL, stratifyCV = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{ The outcome variable}
  \item{X}{ The predictor variables}
  \item{SL.library}{ The library of prediction algorithms to be used in \code{convex.SL}}
  \item{outside.V}{ An integer for the number of folds to split the data into }
  \item{inside.V}{ An integer for the number of folds each Super Learner should use }
  \item{shuffle}{ A logical value indicating whether the rows of the data should be shuffled before the data splits  }
  \item{verbose}{ A logical value to produce additional output }
   \item{family}{ currently allows \code{gaussian} or \code{binomial} to describe the distribution of the outcome.}
  \item{method}{ Loss function for combining prediction in the library.  Currently either "NNLS" (the default), "NNLS2", or "NNloglik".  NNLS and NNLS2 are non-negative least squares based on the Lawson-Hanson algorithm and the dual method of Goldfarb and Idnani, respectively.  NNLS and NNLS2 will work for both gaussian and binomial outcomes.  NNloglik is a non-negative binomial likelihood maximization using the BFGS quasi-Newton optimization method.}
  \item{id}{cluster identification variable.  For the cross-validation splits used to find the weights for each prediction algorithm, \code{id} forces observations in the same cluster to be in the same validation fold.}
  \item{obsWeights}{ observation weights}
  \item{save.fit.library}{ a logical value whether to save the fit of each algorithm in the library on the full data set.  This must be TRUE for \code{predict.SuperLearner} to work.}
  \item{trim.logit}{ Only used if \code{method="NNloglik"}.  specifies a truncation level for the logit function for stability.}
  \item{stratifyCV}{ a logical value for the cross-validation splits.  If TRUE and the family is binomial then the splits will stratify on the outcome to give (roughly) equal proportions of the outcome in all splits.  Currently will not work in combination with cluster id.}
  \item{\dots}{ additional arguments \dots}
}
\details{
  see \code{\link{SuperLearner}} for details on the Super Learner
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
\references{	van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2008) Super Learner, \emph{Statistical Applications of Genetics and Molecular Biology}, \bold{6}, article 25. \url{http://www.bepress.com/sagmb/vol6/iss1/art25}  
}
\author{ Eric C Polley \email{ecpolley@berkeley.edu} }

\seealso{\code{\link{SuperLearner}}}
\examples{
\dontrun{ 
## simulate data
set.seed(23432)
## training set
n <- 200
p <- 20
X <- matrix(rnorm(n*p), nrow=n, ncol=p)
colnames(X) <- paste("X",1:p, sep="")
X <- data.frame(X)
Y <- X[, 1] + X[, 2]^2 - X[, 3] + X[, 1]*X[, 4] + X[, 5] + X[, 6] - X[, 7] + rnorm(n)

## test set
m <- 1000
newX <- matrix(rnorm(m*p), nrow=m, ncol=p)
colnames(newX) <- paste("X",1:p, sep="")
newX <- data.frame(newX)
newY <- newX[, 1] + newX[, 2]^2 - newX[, 3] + newX[, 1]*newX[, 4] + newX[, 5] + newX[, 6] - newX[, 7] + rnorm(m)

## generate Library and run Super Learner
SL.library <- c("SL.glmnet","SL.glm","SL.randomForest")
test <- SuperLearner(Y=Y, X=X, newX=newX, SL.library=SL.library, verbose=TRUE, V=20)
test
testCV <- CV.SuperLearner(Y=Y, X=X, SL.library=SL.library, verbose=TRUE, outside.V=10, inside.V = 20)
testCV
## compare SuperLearner honest CV risk with discrete super learner CV risk
mean((Y - testCV$pred.SL)^2)
mean((Y - testCV$pred.discreteSL)^2)
apply(testCV$pred.library, 2, function(x) mean((Y - x)^2))
summary(testCV)

## Binary outcome:
set.seed(1)
N <- 200
X <- matrix(rnorm(N*10), N, 10)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] + .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))

SL.library <- c("SL.glmnet","SL.glm","SL.randomForest", "SL.knn20", "SL.knn30", "SL.knn40", "SL.knn50", "SL.glmnet.alpha50", "SL.gam", "SL.gam.3")

testCV.NNLS <- CV.SuperLearner(Y=Y, X=X, SL.library=SL.library, verbose=TRUE, outside.V=10, inside.V = 20, method = "NNLS", family = binomial())
summary(testCV.NNLS)

testCV.NNloglik <- CV.SuperLearner(Y=Y, X=X, SL.library=SL.library, verbose=TRUE, outside.V=10, inside.V = 20, method = "NNloglik", family = binomial())
summary(testCV.NNloglik)
 
}
}
\keyword{models}

