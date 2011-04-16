# bagTree {rpart}
# check out ipredbagg {ipred} for a possibly faster method
SL.bagTree <- function(Y.temp, X.temp, newX.temp, family, cp =0.01, minsplit = 20, xval=0, maxdepth=30, ntree=500, weights = "oob", ...) {
	tryCatch(require(rpart), warning = function(...){ 
		stop("you have selected bagTree as a library algorithm but do not have the rpart package installed")
	})

	nn <- length(Y.temp)
	if(family$family=="gaussian"){
		predTrees <- matrix(NA, nrow=(nn + nrow(newX.temp)), ncol=ntree)
		coefTrees <- rep(NA, ntree)
		fit.rpart <- vector("list", ntree)
		for(mm in seq(ntree)) {
			n.oob <- 0
			while(n.oob < 1) {
				whichRow <- sample(1:nn, replace=TRUE)
				oob <- (1:nn)[!(1:nn %in% whichRow)]
				n.oob <- length(oob)
			}
			bootY <- Y.temp[whichRow]
			bootX <- X.temp[whichRow, ,drop=FALSE]
			fit.rpart[[mm]] <- rpart(Y.temp~., data=data.frame(Y.temp=bootY, bootX), control = rpart.control(cp=cp, minsplit=minsplit, xval=xval, maxdepth=maxdepth), method="anova")
			predTrees[, mm] <- predict(fit.rpart[[mm]], newdata=rbind(X.temp, newX.temp))
			if(weights == "oob") {
				coefTrees[mm] <- mean((Y.temp[oob] - predTrees[oob, mm])^2)
			} else {
				coefTrees[mm] <- 1
			}
		}

	}
	if(family$family=="binomial"){
		predTrees <- matrix(NA, nrow=(nn + nrow(newX.temp)), ncol=ntree)
		coefTrees <- rep(NA, ntree)
		fit.rpart <- vector("list", ntree)
		for(mm in seq(ntree)){
			n.oob <- 0
			while(n.oob < 1) {
				whichRow <- sample(1:nn, replace=TRUE)
				oob <- (1:nn)[!(1:nn %in% whichRow)]
				n.oob <- length(oob)
			}
			bootY <- Y.temp[whichRow]
			bootX <- X.temp[whichRow, ,drop=FALSE]
			fit.rpart[[mm]] <- rpart(Y.temp~., data=data.frame(Y.temp=bootY, bootX), control = rpart.control(cp=cp, minsplit=minsplit, xval=xval, maxdepth=maxdepth), method="class")
			predTrees[, mm] <- predict(fit.rpart[[mm]], newdata=rbind(X.temp, newX.temp), type="prob")[, 2]
			if(weights == "oob") {
				# currently using L2 risk
				coefTrees[mm] <- mean((Y.temp[oob] - predTrees[oob, mm])^2)
			} else {
				coefTrees[mm] <- 1
			}
			
		}
		
		# fit.rpart <- rpart(Y.temp~., data=data.frame(Y.temp, X.temp), control = rpart.control(cp=cp, minsplit=minsplit, xval=xval, maxdepth=maxdepth), method="class")
	}
	
	out <- crossprod(t(predTrees[-c(1:nn), ]), ((1/coefTrees)/sum((1/coefTrees))))

	fit <- list(object = fit.rpart, coefTrees = coefTrees, ntree = ntree)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.bagTree")
	return(foo)
}

# 
predict.SL.bagTree <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	tryCatch(require(rpart), warning = function(...){ stop("you have selected bagTree as a library algorithm but do not have the rpart package installed")})
	
	trees <- matrix(NA, nrow=nrow(newdata), ncol = object$ntree)
	if(family$family=="binomial"){
		for(mm in seq(object$ntree)) {
			trees[, mm] <- predict(object$object[[mm]], newdata=newdata, type="prob")[, 2]
		}
	}
	if(family$family=="gaussian") {
		for(mm in seq(object$ntree)) {
			trees[, mm] <- predict(object$object[[mm]], newdata=newdata)
		}
	}
	out <- crossprod(t(trees), ((1/object$coefTrees)/sum((1/object$coefTrees))))
	return(out)
}


SL.bagTree.unit <- function(..., weights = "unit") {
	SL.bagTree(..., weights=weights)
}
