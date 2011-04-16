\name{SuperLearner}
\alias{SuperLearner}
\alias{ParSuperLearner}
\alias{convex.SL}

\title{Super Learner Prediction Function}
\description{A Prediction Function for the Super Learner.  The \code{SuperLearner} function takes a training set pair (X,Y) and gives the predicted values based on a validation set.}
\usage{
SuperLearner(Y, X, newX, SL.library, V = 20, shuffle = TRUE, verbose = FALSE, family = gaussian(), method="NNLS", id=NULL, save.fit.library=TRUE, trim.logit=0.001, obsWeights = NULL, stratifyCV = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{ The outcome in the training data set}
  \item{X}{ The predictor variables in the training data set }
  \item{newX}{ The predictor variables in the validation data set }
  \item{SL.library}{ A vector of prediction algorithms, listed as strings.  A list of all functions included in the SuperLearner package can be found with \code{listWrappers()}}
  \item{V}{ Number of cross-validation folds }
  \item{shuffle}{ a logical value indicating whether the rows in the training data set should be shuffled before creating the V-fold cross-validation splits}
  \item{verbose}{ a logical value for a detailed output (helpful for debugging elements in the library) }
  \item{family}{ currently allows \code{gaussian} or \code{binomial} to describe the distribution of the outcome.}
  \item{method}{ Loss function for combining prediction in the library.  Currently either "NNLS" (the default), "NNLS2", or "NNloglik".  NNLS and NNLS2 are non-negative least squares based on the Lawson-Hanson algorithm and the dual method of Goldfarb and Idnani, respectively.  NNLS and NNLS2 will work for both gaussian and binomial outcomes.  NNloglik is a non-negative binomial likelihood maximization using the BFGS quasi-Newton optimization method.}
  \item{id}{cluster identification variable.  For the cross-validation splits used to find the weights for each prediction algorithm, \code{id} forces observations in the same cluster to be in the same validation fold.}
  \item{save.fit.library}{ a logical value whether to save the fit of each algorithm in the library on the full data set.  This must be TRUE for \code{predict.SuperLearner} to work.}
  \item{trim.logit}{ Only used if \code{method="NNloglik"}.  specifies a truncation level for the logit function for stability.}
  \item{obsWeights}{ observation weights (need to make sure prediction and screening algorithms in your library are setup to use these weights, otherwise they will be ignored)}
  \item{stratifyCV}{ a logical value for the cross-validation splits.  If TRUE and the family is binomial then the splits will stratify on the outcome to give (roughly) equal proportions of the outcome in all splits.  Currently will not work in combination with cluster id.}
  \item{\dots}{ additional arguments \dots}
}
\details{
  \code{SuperLearner} fit the super learner prediction algorithm.  The weights for each algorithm in \code{SL.library} is estimated, along with the fit of each algorithm.

The prescreen algorithms.  These algorithms first rank the variables in \code{X} based on either a univariate regression p-value of the \code{randomForest} variable importance.  A subset of the variables in \code{X} is selected based on a pre-defined cut-off.  With this subset of the X variables, the algorithms in \code{SL.library} are then fit.
}
\value{
  \item{cand.names}{ gives a list of all algorithms in the library, including any screening algorithms}
  \item{SL.library}{ gives a list of all algorithms in the library}
  \item{SL.predict}{ predicted values from the super learner with convex weights}
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
  \item{obsWeights}{ observation weights}
}
\references{ 
	van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2008) Super Learner, \emph{Statistical Applications of Genetics and Molecular Biology}, \bold{6}, article 25. \url{http://www.bepress.com/sagmb/vol6/iss1/art25}  
}
\author{ Eric C Polley \email{ecpolley@berkeley.edu} }
\examples{
\dontrun{
## simulate data
set.seed(23432)
## training set
n <- 500
p <- 50
X <- matrix(rnorm(n*p),nrow=n,ncol=p)
colnames(X) <- paste("X",1:p,sep="")
X <- data.frame(X)
Y <- X[,1] + sqrt(abs(X[,2] * X[,3])) + X[,2] - X[,3] + rnorm(n)

## test set
m <- 1000
newX <- matrix(rnorm(m*p),nrow=m,ncol=p)
colnames(newX) <- paste("X",1:p,sep="")
newX <- data.frame(newX)
newY <- newX[,1] + sqrt(abs(newX[,2] * newX[,3])) + newX[,2] - newX[,3] + rnorm(m)

# generate Library and run Super Learner
SL.library <- c("SL.glmnet", "SL.glmnet.alpha50", "SL.glm","SL.randomForest", "SL.gam", "SL.gam.3", "SL.polymars", "SL.step", "SL.bart", "SL.mean")
test <- SuperLearner(Y=Y, X=X, newX=newX, SL.library=SL.library, verbose=TRUE, V=10, method = "NNLS")
test

# library with screening
SL.library <- list(c("SL.glmnet", "All", "screen.glmP"), c("SL.glm", "screen.randomForest", "All", "screen.glmP"), "SL.randomForest", c("SL.polymars", "All", "screen.glmP"), "SL.bart", "SL.mean")
test <- SuperLearner(Y=Y, X=X, newX=newX, SL.library=SL.library, verbose=TRUE, V=10, method = "NNLS")
test

# binary outcome
set.seed(1)
N <- 200
X <- matrix(rnorm(N*10), N, 10)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] + .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))

SL.library <- c("SL.glmnet","SL.glm","SL.randomForest", "SL.knn20", "SL.knn30", "SL.knn40", "SL.knn50", "SL.glmnet.alpha50", "SL.gam", "SL.gam.3", "SL.mean")

# least squares loss function
test.NNLS <- SuperLearner(Y=Y, X=X, SL.library=SL.library, verbose=TRUE, V = 20, method = "NNLS", family = binomial())
test.NNLS

# negative log binomial likelihood loss function
test.NNloglik <- SuperLearner(Y=Y, X=X, SL.library=SL.library, verbose=TRUE, V = 20, method = "NNloglik", family = binomial())
test.NNloglik

# 2
# adapted from library(SIS)
set.seed(1)
# training
b <- c(2, 2, 2, -3*sqrt(2))
n <- 150
p <- 200
truerho <- 0.5
corrmat <- diag(rep(1-truerho, p)) + matrix(truerho, p, p)
corrmat[, 4] = sqrt(truerho)
corrmat[4, ] = sqrt(truerho)
corrmat[4, 4] = 1
cholmat <- chol(corrmat)
x <- matrix(rnorm(n*p, mean=0, sd=1), n, p)
x <- x %*% cholmat
feta <- x[, 1:4] %*% b
fprob <- exp(feta) / (1 + exp(feta))
y <- rbinom(n, 1, fprob)

# test
m <- 10000
newx <- matrix(rnorm(m*p, mean=0, sd=1), m, p)
newx <- newx%*%cholmat
newfeta <- newx[, 1:4] %*% b
newfprob <- exp(newfeta) / (1 + exp(newfeta))
newy <- rbinom(m, 1, newfprob)

DATA2 <- data.frame(Y = y, X = x)
newDATA2 <- data.frame(Y = newy, X=newx)


# library with screening
SL.library <- list(c("SL.glmnet", "All", "screen.glmP", "screen.SIS"), c("SL.glm", "screen.randomForest", "screen.glmP", "screen.SIS"), "SL.randomForest", "SL.knn", "SL.knn20", "SL.knn30", "SL.knn40", "SL.knn50", "SL.knn60", "SL.knn70", c("SL.polymars", "All", "screen.randomForest", "screen.glmP", "screen.SIS"), c("SL.step.forward", "All", "screen.randomForest", "screen.glmP", "screen.SIS"))
test <- SuperLearner(Y=DATA2$Y, X=DATA2[, -1], newX=newDATA2[, -1], SL.library=SL.library, verbose=TRUE, V=10, family=binomial())
test

}
}
\keyword{models}

