# loess {stats}
# 

SL.loess <- function(Y.temp, X.temp, newX.temp, family, obsWeights, span = 0.75, l.family="gaussian",...){
	if(family$family=="gaussian"){
		fit.loess <- loess(as.formula(paste("Y.temp~", names(X.temp))), data = X.temp, family=l.family, span=span, control = loess.control(surface = "direct"), weights = obsWeights)
	}
	if(family$family=="binomial"){
				fit.loess <- loess(as.formula(paste("Y.temp~", names(X.temp))), data = X.temp, family=l.family, span=span, control = loess.control(surface = "direct"), weights = obsWeights)
	}
	out <- predict(fit.loess, newdata=newX.temp[, 1])
	fit <- list(object = fit.loess)
	foo <- list(out=out,fit=fit)
	class(foo$fit) <- c("SL.loess")
	return(foo)
}

# 
predict.SL.loess <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	out <- predict(object = object$object, newdata=newdata[, 1])
	return(out)
}

