# bagging {ipred}
# 

SL.bagging <- function(Y.temp, X.temp, newX.temp, family, nbagg=100, control = rpart.control(xval = 0, maxsurrogate = 0, minsplit = 20, cp = 0.01, maxdepth = 30), ...){
	tryCatch(require(ipred), warning = function(...){ stop("you have selected bagging as a library algorithm but do not have the ipred package installed")})

	if(family$family=="gaussian"){
		fit.bag <- ipredbagg(y=Y.temp, X=X.temp, nbagg= nbagg, control = control)
		out <- predict(fit.bag, newdata=newX.temp, aggregation = "average")
	}
	if(family$family=="binomial"){
		fit.bag <- ipredbagg(y=factor(Y.temp), X=X.temp, nbagg= nbagg, control = control)
		out <- predict(fit.bag, newdata=newX.temp, type="prob", aggregation = "average")[, 2]
	}
	fit <- list(object=fit.bag)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.bagging")
	return(foo)
}

# 
predict.SL.bagging <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	tryCatch(require(ipred), warning = function(...){ stop("you have selected bagging as a library algorithm but do not have the ipred package installed")})
	if(family$family=="gaussian"){
		out <- predict(object = object$object, newdata=newdata, aggregation = "average")
	}
	if(family$family=="binomial"){
		out <- predict(object = object$object, newdata=newdata, type="prob", aggregation = "average")[, 2] 
	}
	return(out)
}

