# step.plr{stepPlr}
SL.step.plr <- function(Y.temp, X.temp, newX.temp, family, obsWeights, ...){
	tryCatch(require(stepPlr), warning = function(...){ stop("you have selected step.plr as a library algorithm but do not have the stepPlr package installed")})
	if(family$family=="gaussian"){
		stop("step.plr only works with binomial outcomes")
	}
	if(family$family=="binomial"){
		fit.plr <- step.plr(x=as.matrix(X.temp), y=Y.temp, type="forward", weights = obsWeights)
		out <- predict(fit.plr, x=as.matrix(X.temp), newx=as.matrix(newX.temp), type="response", max.terms=dim(X.temp)[2])
		fit <- list(object=fit.plr)
	}
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.step.plr")
	return(foo)
}

# 
predict.SL.step.plr <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	tryCatch(require(stepPlr), warning = function(...){ stop("you have selected step.plr as a library algorithm but do not have the stepPlr package installed")})
	out <- predict(object$object, x=as.matrix(X), newx=as.matrix(newdata), type="response")
	out
}