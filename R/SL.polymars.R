## polymars{polspline}
# in the binomial case, drop the cv=5 selects model based on AIC
SL.polymars <- function(Y.temp, X.temp, newX.temp, family, obsWeights, ...){
	tryCatch(require(polspline), warning = function(...){ stop("you have selected polymars or polyclass as a library algorithm but do not have the polspline package installed")})

	if(family$family=="gaussian") { 
		fit.mars <- polymars(Y.temp, X.temp, weights = obsWeights)
 		out <- predict.polymars(fit.mars, x=as.matrix(newX.temp))
		fit <- list(object=fit.mars)
	}
	if(family$family=="binomial") {
		fit.mars <- polyclass(Y.temp, X.temp, cv=5, weight = obsWeights)
		out <- ppolyclass(cov=newX.temp, fit=fit.mars)[, 2]
		fit <- list(fit=fit.mars)
	}
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.polymars")
	return(foo)
}

predict.SL.polymars <- function(object, newdata, family,...){
	tryCatch(require(polspline), warning = function(...){ stop("you have selected polymars or polyclass as a library algorithm but do not have the polspline package installed")})
	if(family$family=="gaussian"){ 
 		out <- predict.polymars(object=object$object, x=as.matrix(newdata))
	}
	if(family$family=="binomial"){
		out <- ppolyclass(cov=newdata, fit=object$fit)[, 2]
	}
	out
}