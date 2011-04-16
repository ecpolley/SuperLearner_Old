# generalized linear regression

SL.glm <- function(Y.temp, X.temp, newX.temp, family, obsWeights, ...){
	fit.glm <- glm(Y.temp~., data=X.temp, family=family, weights = obsWeights)
  	out <- predict(fit.glm, newdata=newX.temp, type="response")
	fit <- list(object=fit.glm)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.glm")
	return(foo)
}

predict.SL.glm <- function(object, newdata, ...){
	out <- predict(object=object$object, newdata=newdata, type="response")
	out
}


# object$fit.library[[k]]$object
SL.glmFactor <- function (Y.temp, X.temp, newX.temp, family, obsWeights, ...) 
{
    X.tempF <- data.frame(lapply(X.temp, factor))
    newX.tempF <- data.frame(lapply(newX.temp, factor))
    fit.glm <- glm(Y.temp ~ ., data = X.tempF, family = family)
    out <- predict(fit.glm, newdata = newX.tempF, type = "response")
    fit <- list(object = fit.glm)
    foo <- list(out = out, fit = fit)
    class(foo$fit) <- c("SL.glm")
    return(foo)
}