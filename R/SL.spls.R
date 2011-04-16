#spls{spls}
# cv.spls{spls}
# may want to alter the search region for K and eta in the SL.cv.spls function.
# also note that I adapted the cv.spls code to turn the plot and printed feedback off

SL.cv.spls <- function(Y.temp, X.temp, newX.temp, family, K=c(2:10), eta=seq(0.1,0.9,0.1),...){
	tryCatch(require(spls), warning = function(...){ stop("you have selected spls as a library algorithm but do not have the spls package installed")})
	if(family$family=="gaussian"){
		fit.cv <- .cv.spls(x=X.temp, y=Y.temp, K=K, eta=eta)
		fit.spls <- spls(x=X.temp, y=Y.temp, K=fit.cv$K.opt, eta=fit.cv$eta.opt)
	}
	if(family$family=="binomial"){
		stop("spls is for gaussian family")
	}
	out <- predict(fit.spls, newx=newX.temp, type="fit")
	fit <- list(object=fit.spls, k.opt=fit.cv$K.opt, eta.opt=fit.cv$eta.opt)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.spls")
	return(foo)
}

SL.spls <- function(Y.temp, X.temp, newX.temp, family, K=8, eta=0.7,...){
	tryCatch(require(spls), warning = function(...){ stop("you have selected spls as a library algorithm but do not have the spls package installed")})
	if(family$family=="gaussian"){
		fit.spls <- spls(x=X.temp, y=Y.temp, K=K, eta=eta)
	}
	if(family$family=="binomial"){
		stop("spls is for gaussian family")
	}
	out <- predict(fit.spls, newx=newX.temp, type="fit")
	fit <- list(object=fit.spls)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.spls")
	return(foo)
}

# 
predict.SL.spls <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	tryCatch(require(spls), warning = function(...){ stop("you have selected spls as a library algorithm but do not have the spls package installed")})
	out <- predict(object$object,newx=newdata,type="fit")
	out
}

.cv.spls <- function (x, y, fold = 10, K, eta, kappa = 0.5, select = "pls2", 
    fit = "simpls", scale.x = TRUE, scale.y = FALSE, plot.it=FALSE, verbose=FALSE) 
{
    # adapted from cv.spls{spls} by D. Chung, H. Chun, S. Keles
	# added options to not plot the heatmap and to not print the eta values
	x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    ip <- c(1:p)
    y <- as.matrix(y)
    q <- ncol(y)
    type <- correctp(x, y, eta, K, kappa, select, fit)
    eta <- type$eta
    K <- type$K
    kappa <- type$kappa
    select <- type$select
    fit <- type$fit
    foldi <- split(sample(1:n), rep(1:fold, length = n))
    mspemat <- matrix(0, length(eta), length(K))
    for (i in 1:length(eta)) {
        if(verbose){cat(paste("eta =", eta[i], "\n"))}
        mspemati <- matrix(0, fold, length(K))
        for (j in 1:fold) {
            omit <- foldi[[j]]
            object <- spls(x[-omit, ], y[-omit, ], eta = eta[i],kappa = kappa, K = max(K), select = select, fit = fit,scale.x = scale.x, scale.y = scale.y, trace = FALSE)
            newx <- x[omit, ]
            newx <- scale(newx, object$meanx, object$normx)
            betamat <- object$betamat
            for (k in K) {
                pred <- newx %*% betamat[[k]] + matrix(1, nrow(newx),1) %*% object$mu
                mspemati[j, (k - min(K) + 1)] <- mean(apply((y[omit,] - pred)^2, 2, mean))
            }
        }
        mspemat[i, ] <- apply(mspemati, 2, mean)
    }
    minpmse <- min(mspemat)
    rownames(mspemat) <- eta
    colnames(mspemat) <- K
    mspecol <- apply(mspemat, 2, min)
    msperow <- apply(mspemat, 1, min)
    K.opt <- min(K[mspecol == minpmse])
    eta.opt <- max(eta[msperow == minpmse])
    if(verbose){cat(paste("\nOptimal parameters: eta = ", eta.opt, ", ",sep = ""))
    cat(paste("K = ", K.opt, "\n", sep = ""))
	}

    if(plot.it){ heatmap.spls(t(mspemat), xlab = "K", ylab = "eta", main = "CV MSPE Plot", coln = 16, as = "n")}
    rownames(mspemat) <- paste("eta=", eta)
    colnames(mspemat) <- paste("K =", K)
    cv <- list(mspemat = mspemat, eta.opt = eta.opt, K.opt = K.opt)
    invisible(cv)
}
