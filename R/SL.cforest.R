# cforest {party}
SL.cforest <- function(Y.temp, X.temp, newX.temp, family, obsWeights, ...){
	tryCatch(require(party), warning = function(...){ stop("you have selected cforest as a library algorithm but do not have the party package installed")})
	if(family$family=="gaussian"){
		fit.cforest <- cforest(Y.temp~., data=data.frame(Y.temp, X.temp), controls = cforest_unbiased(ntree=1000, mtry=max(floor(ncol(X.temp)/3), 1)), weights = obsWeights)
	}
	if(family$family=="binomial"){
		stop("Currently only works with gaussian data \ncforest can not return predicted probabilities")
	}
	out <- predict(object = fit.cforest, newdata=newX.temp)
	fit <- list(object=fit.cforest)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.cforest")
	return(foo)
}

# 
predict.SL.cforest <- function(object, newdata, family, X=NULL, Y=NULL, ...) {
	tryCatch(require(party), warning = function(...){ stop("you have selected cforest as a library algorithm but do not have the party package installed")})
	out <- predict(object = object$object, newdata=newdata)
	out 
}