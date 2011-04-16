## lm.ridge{MASS}
# may want to change range lambda searches over
# will only work with guassian
SL.ridge <- function(Y.temp, X.temp, newX.temp, family, ...){
	tryCatch(require(MASS), warning = function(...){ stop("you have selected lm.ridge as a library algorithm but do not have the MASS package installed")})
	if(family$family=="binomial"){
		stop("Currently only works with gaussian data")
	}
	fit.ridge <- lm.ridge(Y.temp~., data=X.temp, lambda=seq(1, 20, .1))
	out <- coef(fit.ridge)
	final <- as.matrix(out[which.min(fit.ridge$GCV), ])
	m <- dim(newX.temp)[1]
	newx.ridge <- as.matrix(cbind(rep(1, m), newX.temp))
	out <- newx.ridge%*%final
	fit <- list(final=final)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.ridge")
	return(foo)
}

predict.SL.ridge <- function(object, newdata,...){
	tryCatch(require(MASS), warning = function(...){ stop("you have selected lm.ridge as a library algorithm but do not have the MASS package installed")})
	m <- dim(newdata)[1]
	newx.ridge <- as.matrix(cbind(rep(1, m), newdata))
	out <- newx.ridge %*% object$final
	out
}