screen.randomForest <- function (Y.temp, X.temp, family, nVar = 10, ntree = 1000, ...) 
{
    tryCatch(require(randomForest), warning = function(...) {
        stop("you have selected randomForest as a library algorithm but do not have the randomForest package installed")
	})
	if (family$family == "gaussian") {
		rank.rf.fit <- randomForest(Y.temp~., data=X.temp, ntree=ntree)
    }
    if (family$family == "binomial") {
		rank.rf.fit <- randomForest(y=as.factor(Y.temp), x=X.temp, ntree=ntree)
    }

	whichVariable <- (rank(-rank.rf.fit$importance) <= nVar)
    return(whichVariable)
}

