# randomForest{randomForest}
# ntree may be increased
# should additional tuning parameters be added? (i.e. mtry)

SL.randomForest <- function(Y.temp, X.temp, newX.temp, family, mtry = ifelse(family$family=="gaussian", floor(sqrt(ncol(X.temp))), max(floor(ncol(X.temp)/3), 1)), ntree=1000, nodesize = ifelse(family$family=="gaussian", 5, 1), ...){
	tryCatch(require(randomForest), warning = function(...){ stop("you have selected randomForest as a library algorithm but do not have the randomForest package installed")})
	if(family$family=="gaussian"){
		fit.rf <- randomForest(Y.temp~., data=X.temp, ntree=ntree, xtest=newX.temp, keep.forest=TRUE, mtry = mtry, nodesize = nodesize)
		out <- fit.rf$test$predicted
		fit <- list(object=fit.rf)
	}
	if(family$family=="binomial"){
		fit.rf <- randomForest(y=as.factor(Y.temp), x=X.temp, ntree=ntree, xtest=newX.temp, keep.forest=TRUE, mtry = mtry, nodesize = nodesize)
		out <- fit.rf$test$votes[,2]
		fit <- list(object=fit.rf)
	}
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.randomForest")
	return(foo)
}

predict.SL.randomForest <- function(object, newdata, family,...){
	tryCatch(require(randomForest), warning = function(...){ stop("you have selected randomForest as a library algorithm but do not have the randomForest package installed")})
	if(family$family=="gaussian"){
		out <- predict(object$object, newdata=newdata, type="response")
	}
	if(family$family=="binomial"){
		out <- predict(object$object, newdata=newdata, type="vote")[,2]
	}
	out
}


SL.rF <- function(Y.temp, X.temp, newX.temp, ntree=1000, family, mtry=ncol(X.temp), nodesize = ifelse(family$family=="gaussian",5,1), ...){
	tryCatch(require(randomForest), warning = function(...){ stop("you have selected randomForest as a library algorithm but do not have the randomForest package installed")})
	if(family$family=="gaussian"){
		fit.rf <- randomForest(Y.temp~., data=X.temp, ntree=ntree, xtest=newX.temp, keep.forest=TRUE, mtry=mtry, nodesize=nodesize)
		out <- fit.rf$test$predicted
		fit <- list(object=fit.rf)
	}
	if(family$family=="binomial"){
		fit.rf <- randomForest(y=as.factor(Y.temp), x=X.temp, ntree=ntree, xtest=newX.temp, keep.forest=TRUE, mtry=mtry, nodesize=nodesize)
		out <- fit.rf$test$votes[,2]
		fit <- list(object=fit.rf)
	}
	foo <- list(out=out,fit=fit)
	class(foo$fit) <- c("SL.randomForest")
	return(foo)
}
