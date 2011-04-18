# earth {earth}
SL.earth <- function(Y.temp, X.temp, newX.temp, family, obsWeights, id, degree = 2, penalty = 3, nk = max(21, 2*ncol(X.temp) + 1), ...){
	tryCatch(require(earth), warning = function(...) {
		stop("you have selected earth as a library algorithm but do not have the earth package installed")
	})
	if(family$family=="gaussian"){
		fit.earth <- earth::earth(x = X.temp, y = Y.temp, degree = degree, nk = nk, penalty = penalty)
	}
	if(family$family=="binomial"){
		fit.earth <- earth::earth(x = X.temp, y = Y.temp, degree = degree, nk = nk, penalty = penalty, glm = list(family = binomial))
	}
	out <- predict(fit.earth, newdata = newX.temp, type = "response")
	fit <- list(object = fit.earth)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.earth")
	return(foo)
}

# 
predict.SL.earth <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	tryCatch(require(earth), warning = function(...) {
		stop("you have selected earth as a library algorithm but do not have the earth package installed")
	})
	out <- predict(object$object, newdata = newdata, type = "repsonse")
	return(out)
}