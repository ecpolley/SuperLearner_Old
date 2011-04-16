# gbm{gbm}
# generalized boosting algorithm
# can alter number ot trees in intial fit
# also alter interaction depth (SL.gbm.1 and SL.gbm.2)
SL.gbm.1 <- function(Y.temp, X.temp, newX.temp, family, obsWeights, gbm.trees=10000, ...){
	tryCatch(require(gbm), warning = function(...){ stop("you have selected gbm as a library algorithm but do not have the gbm package installed")})
	if(family$family=="gaussian"){  
		gbm.model <- as.formula(paste("Y.temp~", paste(colnames(X.temp), collapse="+")))
		fit.gbm1 <- gbm(formula=gbm.model, data=X.temp, distribution="gaussian", n.trees=gbm.trees, interaction.depth=1, cv.folds=5, keep.data=TRUE, weights = obsWeights, verbose=FALSE)
		best.iter1 <- gbm.perf(fit.gbm1, method="cv", plot.it=FALSE)
 		out <- predict(fit.gbm1, newdata=newX.temp,best.iter1, type="response")
		fit <- list(object=fit.gbm1, n.trees=best.iter1)
	}
	if(family$family=="binomial"){
		gbm.model <- as.formula(paste("Y.temp~", paste(colnames(X.temp), collapse="+")))
		fit.gbm1 <- gbm(formula=gbm.model, data=X.temp, distribution="bernoulli", n.trees=gbm.trees, interaction.depth=1, cv.folds=5, keep.data=TRUE, verbose=FALSE, weights = obsWeights)
		best.iter1 <- gbm.perf(fit.gbm1, method="cv", plot.it=FALSE)
		out <- predict(fit.gbm1, newdata=newX.temp, best.iter1, type="response")
		fit <- list(object=fit.gbm1, n.trees=best.iter1)
	}
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.gbm")
	return(foo)
}

SL.gbm.2 <- function(Y.temp, X.temp, newX.temp, family, obsWeights, gbm.trees=10000,...){
	tryCatch(require(gbm), warning = function(...){ stop("you have selected gbm as a library algorithm but do not have the gbm package installed")})
	if(family$family=="gaussian"){  
		gbm.model <- as.formula(paste("Y.temp~", paste(colnames(X.temp), collapse="+")))
		fit.gbm1 <- gbm(formula=gbm.model, data=X.temp, distribution="gaussian",n.trees=gbm.trees, interaction.depth=2, cv.folds=5, keep.data=TRUE, weights = obsWeights, verbose=FALSE)
		best.iter1 <- gbm.perf(fit.gbm1, method="cv", plot.it=FALSE)
 		out <- predict(fit.gbm1, newdata=newX.temp, best.iter1, type="response")
		fit <- list(object=fit.gbm1, n.trees=best.iter1)
	}
	if(family$family=="binomial"){
		gbm.model <- as.formula(paste("Y.temp~", paste(colnames(X.temp),collapse="+")))
		fit.gbm1 <- gbm(formula=gbm.model, data=X.temp, distribution="bernoulli", n.trees=gbm.trees, interaction.depth=2, cv.folds=5, keep.data=TRUE, weights = obsWeights, verbose=FALSE)
		best.iter1 <- gbm.perf(fit.gbm1, method="cv", plot.it=FALSE)
		out <- predict(fit.gbm1, newdata=newX.temp, best.iter1, type="response")
		fit <- list(object=fit.gbm1, n.trees=best.iter1)	
	}
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.gbm")
	return(foo)
}

predict.SL.gbm <- function(object, newdata,...){
	tryCatch(require(gbm), warning = function(...) { stop("you have selected gbm as a library algorithm but do not have the gbm package installed")})
	out <- predict(object$object, newdata=newdata, n.trees=object$n.trees, type="response")
	out
}

