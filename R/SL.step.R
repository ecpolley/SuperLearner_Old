
SL.step <- function(Y.temp, X.temp, newX.temp, family, direction="both", trace=0, k=2,...){
	fit.glm <- glm(Y.temp~., data=X.temp, family=family)
	fit.step <- step(fit.glm, direction=direction, trace=trace, k=k)
	out <- predict(fit.step, newdata=newX.temp, type="response")
	fit <- list(object=fit.step)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.step")
	return(foo)
}

SL.step.forward <- function(Y.temp, X.temp, newX.temp, family, direction="forward", trace=0, k=2,...){
	fit.glm <- glm(Y.temp~., data=X.temp, family=family)
	fit.step <- step(glm(Y.temp~1, data=X.temp, family=family), scope=formula(fit.glm), direction=direction, trace=trace, k=k)
	out <- predict(fit.step, newdata=newX.temp, type="response")
	fit <- list(object=fit.step)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.step")
	return(foo)
}

SL.step.interaction <- function(Y.temp, X.temp, newX.temp, family, direction="both", trace=0, k=2,...){
	fit.glm <- glm(Y.temp~., data=X.temp, family=family)
	fit.step <- step(fit.glm, scope=Y.temp~.^2, direction=direction, trace=trace, k=k)
	out <- predict(fit.step, newdata=newX.temp, type="response")
	fit <- list(object=fit.step)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.step")
	return(foo)
}

# 
predict.SL.step <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	out <- predict(object=object$object, newdata=newdata, type="response")
	out
}

SL.stepAIC <- function(Y.temp, X.temp, newX.temp, family, direction="both", steps=30, k=log(nrow(X.temp)),...){
	tryCatch(require(MASS), warning = function(...){ stop("you have selected stepAIC as a library algorithm but do not have the MASS package installed")})
	g0 <- glm(Y.temp~1, data=X.temp, family=family)
	upper <- formula(paste("~", paste(colnames(X.temp), collapse="+")))
	lower <- formula("~1")
	fit.step <- stepAIC(g0, scope=list(upper=upper, lower=lower), direction=direction, k=k, trace=0, steps=steps)
	out <- predict(fit.step, newdata=newX.temp, type="response")
	fit <- list(object=fit.step)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.stepAIC")
	return(foo)
}

predict.SL.stepAIC <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	tryCatch(require(MASS), warning = function(...){ stop("you have selected stepAIC as a library algorithm but do not have the MASS package installed")})
	out <- predict(object=object$object, newdata=newdata, type="response")
	out
}