# screen.glmnet {glmnet}
# runs glmnet and returns all non-zero beta coefficients
screen.glmnet <- function (Y.temp, X.temp, family, glmnet.alpha = 1, minscreen = 2, ...) 
{
	tryCatch(require(glmnet), warning = function(...){ stop("you have selected glmnet as a library algorithm but do not have the glmnet package installed")})
	fit.first <- glmnet(x = as.matrix(X.temp), y=Y.temp, family=as.character(family)[1], alpha=glmnet.alpha)
	cv.net.fit <- SuperLearner:::.cv.glmnet(x = as.matrix(X.temp), y=Y.temp, K=10, lambda=fit.first$lambda, alpha=glmnet.alpha, family=as.character(family)[1])
    whichVariable <- (fit.first$beta[, which.min(cv.net.fit$cv)] != 0)
    if (sum(whichVariable) < minscreen) {
        warning("fewer than minscreen variables passed the glmnet screen, increased lambda to allow minscreen variables")
		sumCoef <- apply(fit.first$beta, 2, function(x) sum((x != 0)))
		newCut <- which.max(sumCoef >= minscreen)
		whichVariable <- (fit.first$beta[, newCut] != 0)
    }
    return(whichVariable)
}