# bayesglm{arm}
# Bayesian generalized linear regression

SL.bayesglm <- function(Y.temp, X.temp, newX.temp, family, obsWeights, ...){
	tryCatch(require(arm), warning = function(...){ stop("you have selected bayesglm as a library algorithm but do not have the arm package installed")})
	fit.glm <- bayesglm(Y.temp~., data=X.temp, family=family, weights = obsWeights)
  	out <- predict(fit.glm, newdata=newX.temp, type="response")
	fit <- list(object=fit.glm)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.bayesglm")
	return(foo)
}

predict.SL.bayesglm <- function(object, newdata, ...){
	tryCatch(require(arm), warning = function(...){ stop("you have selected bayesglm as a library algorithm but do not have the arm package installed")})
	out <- predict(object=object$object, newdata=newdata, type="response")
	out
}