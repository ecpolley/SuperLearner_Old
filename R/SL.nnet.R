# nnet{nnet}
# can change the size
# SL.nnet.3 <- function(...,size=3){SL.nnet(...,size=size)}
SL.nnet <- function(Y.temp, X.temp, newX.temp, family, obsWeights, size=2, ...){
	tryCatch(require(nnet), warning = function(...){ stop("you have selected nnet as a library algorithm but do not have the nnet package installed")})
	if(family$family=="gaussian"){
		fit.nnet <- nnet(x=X.temp, y=Y.temp, size=size, linout=TRUE, trace=FALSE, maxit=500, weights = obsWeights)
		out <- predict(fit.nnet, newdata=newX.temp, type="raw")
		fit <- list(object=fit.nnet)
	}
	if(family$family=="binomial"){
		fit.nnet <- nnet(x=X.temp, y=Y.temp, size=size, trace=FALSE, maxit=500, linout=FALSE, weights = obsWeights)
		out <- predict(fit.nnet, newdata=newX.temp, type="raw")
		fit <- list(object=fit.nnet)
	}
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.nnet")
	return(foo)
}

predict.SL.nnet <- function(object, newdata,...){
	tryCatch(require(nnet), warning = function(...){ stop("you have selected nnet as a library algorithm but do not have the nnet package installed")})
	out <- predict(object$object, newdata=newdata,type="raw")
	out
}

SL.nnet.3 <- function(...,size=3){SL.nnet(...,size=size)}
SL.nnet.4 <- function(...,size=4){SL.nnet(...,size=size)}
SL.nnet.5 <- function(...,size=5){SL.nnet(...,size=size)}
