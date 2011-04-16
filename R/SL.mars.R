# mars {mda}
SL.mars <- function(Y.temp, X.temp, newX.temp, family, obsWeights, id, degree = 2, penalty = 2, nk = max(21, 2*ncol(X.temp) + 1), ...){
	tryCatch(require(mda), warning = function(...) {
		stop("you have selected mars as a library algorithm but do not have the mda package installed")
	})
	if(family$family=="gaussian"){
		fit.mars <- mda::mars(x = as.matrix(X.temp), y = Y.temp, degree = degree, nk = nk, penalty = penalty)
	}
	if(family$family=="binomial"){
		stop("mars not available for binomial data")
	}
	out <- predict(fit.mars, newdata = newX.temp)
	fit <- list(object = fit.mars)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.mars")
	return(foo)
}

# 
predict.SL.mars <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	tryCatch(require(mda), warning = function(...) {
		stop("you have selected mars as a library algorithm but do not have the mda package installed")
	})
	out <- predict(object$object, newdata = newdata)
	return(out)
}