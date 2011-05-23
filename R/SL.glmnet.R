## glmnet{glmnet}
#  0 < glmnet.alpha <= 1 (with 1 == lasso penalty)
# grab family from convex.SL call.  either binomial or gaussian
# easy to change alpha and create new functions for the library with:
# SL.glmnet.alpha05 <- function(...,alpha=.5){SL.glmnet(...,glmnet.alpha=alpha)}

SL.glmnet <- function(Y.temp, X.temp, newX.temp, family, obsWeights, glmnet.alpha=1, K = 10, ...){
	tryCatch(require(glmnet), warning = function(...){ stop("you have selected glmnet as a library algorithm but do not have the glmnet package installed")})
	fit.first <- glmnet(x=as.matrix(X.temp), y=Y.temp, family=as.character(family)[1], alpha=glmnet.alpha, weights = obsWeights)
	cv.net.fit <- SuperLearnerOld:::.cv.glmnet(x=as.matrix(X.temp), y=Y.temp, K=K, lambda=fit.first$lambda, alpha=glmnet.alpha, family=as.character(family)[1], obsWeights = obsWeights)
	min.lambda <- cv.net.fit$lambda[which.min(cv.net.fit$cv)]
	out <- predict(fit.first, newx=as.matrix(newX.temp), s=min.lambda, type="response")
	fit <- list(object=fit.first, s=min.lambda, cv.net.fit=cv.net.fit)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.glmnet")
	return(foo)
}

predict.SL.glmnet <- function(object, newdata, ...){
	tryCatch(require(glmnet), warning = function(...){ stop("you have selected glmnet as a library algorithm but do not have the glmnet package installed")})
	out <- predict(object$object, newx=as.matrix(newdata), s=object$s, type="response")
	out
}

SL.glmnet.alpha25 <- function(..., alpha=.25){ SL.glmnet(..., glmnet.alpha=alpha)}
SL.glmnet.alpha50 <- function(..., alpha=.50){ SL.glmnet(..., glmnet.alpha=alpha)}
SL.glmnet.alpha75 <- function(..., alpha=.75){ SL.glmnet(..., glmnet.alpha=alpha)}

## required functions
.cv.glmnet <- function(x, y, K, lambda, alpha, family, obsWeights = rep.int(1, length(y))) 
{
    all.folds <- split(sample(1:length(y)), rep(1:K, length = length(y)))
    residmat <- matrix(NA, length(lambda), K)
    for (i in seq(K)) {
        omit <- all.folds[[i]]
        fit <- glmnet(x[-omit, , drop = FALSE], y[-omit], family=as.character(family)[1], alpha=alpha, lambda=lambda, weights = obsWeights[-omit])
        fit <- predict(fit, newx=x[omit, , drop = FALSE], type="response")
        if (length(omit) == 1) 
            fit <- matrix(fit, nrow = 1)
        foo <- apply((obsWeights[omit]*(y[omit] - fit)^2), 2, mean)
		residmat[seq(along=foo), i] <- foo
    }
    cv <- apply(residmat, 1, mean)
    cv.error <- sqrt(apply(residmat, 1, var)/K)
    object <- list(lambda=lambda, cv = cv, cv.error = cv.error)
    invisible(object)
}