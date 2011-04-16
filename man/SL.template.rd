\name{SL.template}
\alias{SL.template}
\alias{SL.bayesglm}
\alias{SL.gam}
\alias{SL.gbm.1}
\alias{SL.gbm.2}
\alias{SL.glm}
\alias{SL.glmnet}
\alias{SL.glmnet.alpha25}
\alias{SL.glmnet.alpha50}
\alias{SL.glmnet.alpha75}
\alias{SL.knn}
\alias{SL.knn20}
\alias{SL.knn30}
\alias{SL.knn40}
\alias{SL.knn50}
\alias{SL.knn60}
\alias{SL.knn70}
\alias{SL.knn80}
\alias{SL.knn90}
\alias{SL.knn100}
\alias{SL.knn200}
\alias{SL.nnet}
\alias{SL.nnet.3}
\alias{SL.nnet.4}
\alias{SL.nnet.5}
\alias{SL.polymars}
\alias{SL.randomForest}
\alias{SL.ridge}
\alias{SL.step.plr}
\alias{SL.svm}
\alias{SL.svm.eps}
\alias{predict.SL.svm.eps}
\alias{predict.SL.template}
\alias{predict.SL.bayesglm}
\alias{predict.SL.gam}
\alias{predict.SL.gbm.1}
\alias{predict.SL.gbm.2}
\alias{predict.SL.glm}
\alias{predict.SL.glmnet}
\alias{predict.SL.glmnet.alpha25}
\alias{predict.SL.glmnet.alpha50}
\alias{predict.SL.glmnet.alpha75}
\alias{predict.SL.knn}
\alias{predict.SL.knn20}
\alias{predict.SL.knn30}
\alias{predict.SL.knn40}
\alias{predict.SL.knn50}
\alias{predict.SL.knn60}
\alias{predict.SL.knn70}
\alias{predict.SL.knn80}
\alias{predict.SL.knn90}
\alias{predict.SL.knn100}
\alias{predict.SL.knn200}
\alias{predict.SL.nnet}
\alias{predict.SL.nnet.3}
\alias{predict.SL.nnet.4}
\alias{predict.SL.nnet.5}
\alias{predict.SL.polymars}
\alias{predict.SL.randomForest}
\alias{predict.SL.ridge}
\alias{predict.SL.step.plr}
\alias{predict.SL.svm}
\alias{SL.cforest}
\alias{predict.SL.cforest}
\alias{SL.DSA}
\alias{predict.SL.DSA}
\alias{SL.DSA.2}
\alias{predict.SL.DSA.2}
\alias{SL.step}
\alias{predict.SL.step}
\alias{SL.step.forward}
\alias{predict.SL.step.forward}
\alias{SL.step.interaction}
\alias{predict.SL.step.interaction}
\alias{SL.stepAIC}
\alias{predict.SL.stepAIC}
\alias{SL.cv.spls}
\alias{My.cv.spls}
\alias{SL.spls}
\alias{predict.SL.cv.spls}
\alias{predict.SL.spls}
\alias{SL.logreg}
\alias{predict.SL.logreg}
\alias{SL.gam.3}
\alias{predict.SL.gam.3}
\alias{SL.gam.4}
\alias{predict.SL.gam.4}
\alias{SL.gam.5}
\alias{predict.SL.gam.5}
\alias{SL.rF}
\alias{SL.svm.eps}
\alias{SL.polymars.dimreduce}
\alias{predict.SL.gbm}
\alias{SL.bart}
\alias{predict.SL.bart}
\alias{SL.rpart}
\alias{predict.SL.rpart}
\alias{SL.loess}
\alias{predict.SL.loess}
\alias{SL.bagging}
\alias{predict.SL.bagging}
\alias{SL.bagTree}
\alias{predict.SL.bagTree}
\alias{SL.bagTree.unit}
\alias{SL.mean}
\alias{predict.SL.mean}
\alias{SL.caret}
\alias{SL.caret.rpart}
\alias{predict.SL.caret}
\alias{SL.mars}
\alias{predict.SL.mars}
\alias{SL.earth}
\alias{predict.SL.earth}
\alias{SL.glmFactor}


\title{ Template file for creating new prediction functions to be used in Super Learner}
\description{
  This function is only a template for creating new prediction functions to add to the library in the super learner.  The function does not run.  see \code{\link{SL.glmnet}} for an example of a working prediction function.
}
\usage{
SL.template(Y.temp, X.temp, newX.temp, family, obsWeights, id,...)
}

\arguments{
  \item{Y.temp}{ The outcome }
  \item{X.temp}{ The training data set}
  \item{newX.temp}{ The validation data set (the observation to make predictions with)}
  \item{family}{ currently allows \code{gaussian} or \code{binomial} to describe the distribution of the outcome}
  \item{obsWeights}{ observation weights}
  \item{id}{ cluster identification}
  \item{\dots}{ often helpful to include \dots}
}
\details{
  A list with all the wrappers included in the package can be found with the function \code{listWrappers()}.

  All prediction functions in the library must have at least the three arguments Y.temp, X.temp and newX.temp.  If the argument family is missing, the \dots must be used.  The same function will be used in the cross-validation step as in the full data step of the super learner.  

  Many of the algorithms have tuning parameters specified by the provided functions.  These tuning parameters can be easily changed (and also provide a mechanism for new algorithms).  For example, the \code{SL.gam} model requires the degree to be pre-specified.  The default in \code{SL.gam} is degree 2.  To change these parameters, simply create a new function as in: \code{SL.gam.3 <- function(...,deg.gam=3){SL.gam(...,deg.gam=deg.gam)}}.  It is often also necessary to create the corresponding predict function as in \code{predict.SL.gam.3 <- function(...){predict.SL.gam(...)}}.  Check the arguments of the functions listed below for adjustable tuning parameters.

\code{screen.foo} functions are designed for dimension reduction steps.  The can be supervised or unsupervised, but are limited to selecting a subset of the variables in \code{X.temp}.  The functions return a logical vector with length equal to the number of variables in \code{X}.

}
\value{
  \item{out}{ The predicted outcome (response) on the scale of the outcome.  For example, if the outcome is Binomial, the predictions should be probabilities (type="response")}
	\item{fit}{ A list of the elements needed for the predict method on the specific algorithm}
}

\author{ Eric Polley}

\seealso{ Examples \code{\link{SL.glmnet}}, \code{\link{SL.polymars}} and \code{\link{SL.glm}}  }
\examples{
\dontrun{
# 
	SL.template <- function(Y.temp,X.temp,newX.temp,family=gaussian(),...){
	if(family$family=="gaussian"){
		# insert estimation and prediction function 
	}
	if(family$family=="binomial"){
		# insert estimation and prediction function 
	}
	# out returns predicted responses (on the scale of the outcome)
	out <- numeric()
	# fit returns all objects needed for predict.SL.template
	fit <- vector("list",length=0)
	foo <- list(out=out,fit=fit)
	class(foo) <- c("SL.template")
	return(foo)

	# screen functions must return a logical vector of length ncol(X)
	screen.template <-function (Y.temp, X.temp, family = gaussian(), ...) 
	{
	   if (family$family == "gaussian") {

	   }
	   if (family$family == "binomial") {

	   }
	whichVariable <- rep(TRUE, ncol(X.temp))
	   return(whichVariable)
}
}

	# 
predict.SL.template <- function(object,newdata,family,X=NULL,Y=NULL,...) {
		# insert prediction function
}


### gaussian family

SL.cforest
SL.gam # deg=2
SL.gam.3 
SL.gam.4
SL.gam.5
SL.gbm.1
SL.gbm.2
SL.glm
SL.glmnet  # alpha = 1.00
SL.glmnet.alpha50 
SL.glmnet.alpha75
SL.glmnet.alpha25
SL.nnet   # size = 2
SL.nnet.3 
SL.nnet.4 
SL.nnet.5
SL.polymars
SL.randomForest
SL.ridge
SL.svm
SL.bayesglm
SL.DSA
SL.DSA.2
SL.stepAIC
SL.step
SL.step.forward
SL.step.interaction
SL.cv.spls
SL.logreg

SL.library <- c("SL.cforest","SL.gam","SL.gam.3","SL.gam.4","SL.gam.5","SL.gbm.1","SL.gbm.2","SL.glm","SL.glmnet","SL.glmnet.alpha25","SL.glmnet.alpha50","SL.glmnet.alpha75","SL.nnet","SL.nnet.3","SL.nnet.4","SL.nnet.5","SL.polymars","SL.randomForest","SL.ridge","SL.svm","SL.bayesglm","SL.DSA","SL.DSA.2","SL.step","SL.step.forward","SL.step.interaction","SL.stepAIC","SL.cv.spls")

### binomial family

SL.knn  # k =10
SL.knn20
SL.knn30 
SL.knn40
SL.knn50 
SL.knn60
SL.knn70
SL.knn80
SL.knn90 
SL.knn100 
SL.knn200 
SL.gam # deg=2
SL.gam.3 
SL.gam.4 
SL.gam.5
SL.gbm.1
SL.gbm.2
SL.glm
SL.glmnet  # alpha = 1.00
SL.glmnet.alpha50 
SL.glmnet.alpha75
SL.glmnet.alpha25
SL.nnet   # size = 2
SL.nnet.3 
SL.nnet.4 
SL.nnet.5 
SL.polymars
SL.randomForest
SL.svm
SL.step.plr
SL.bayesglm
SL.DSA
SL.DSA.2
SL.stepAIC
SL.step
SL.step.forward
SL.step.interaction
SL.logreg

SL.library <- c("SL.knn","SL.knn20","SL.knn30","SL.knn40","SL.knn50","SL.knn60","SL.knn70","SL.knn80","SL.knn90","SL.knn100","SL.knn200","SL.gam","SL.gam.3","SL.gam.4","SL.gam.5","SL.gbm.1","SL.gbm.2","SL.glm","SL.glmnet","SL.glmnet.alpha25","SL.glmnet.alpha50","SL.glmnet.alpha75","SL.nnet","SL.nnet.3","SL.nnet.4","SL.nnet.5","SL.polymars","SL.randomForest","SL.svm","SL.step.plr","SL.bayesglm","SL.DSA","SL.DSA.2","SL.step","SL.step.forward","SL.step.interaction","SL.stepAIC")

}
}

\keyword{programming}

