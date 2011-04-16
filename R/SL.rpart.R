# rpart {rpart}
SL.rpart <- function(Y.temp, X.temp, newX.temp, family, obsWeights, cp =0.01, minsplit = 20, xval=10, maxdepth=30,...) {
	tryCatch(require(rpart), warning = function(...){ stop("you have selected rpart as a library algorithm but do not have the rpart package installed")})
	if(family$family=="gaussian"){
		fit.rpart <- rpart(Y.temp~., data=data.frame(Y.temp, X.temp), control = rpart.control(cp=cp, minsplit=minsplit, xval=xval, maxdepth=maxdepth), method="anova", weights = obsWeights)
		out <- predict(fit.rpart, newdata=newX.temp)
	}
	if(family$family=="binomial"){
		fit.rpart <- rpart(Y.temp~., data=data.frame(Y.temp, X.temp), control = rpart.control(cp=cp, minsplit=minsplit, xval=xval, maxdepth=maxdepth), method="class", weights = obsWeights)
		out <- predict(fit.rpart, newdata=newX.temp)[, 2]
	}
	fit <- list(object=fit.rpart)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.rpart")
	return(foo)
}

# 
predict.SL.rpart <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	tryCatch(require(rpart), warning = function(...) { stop("you have selected rpart as a library algorithm but do not have the rpart package installed")})
	out <- predict(object, newdata=newdata)
	return(out)
}

