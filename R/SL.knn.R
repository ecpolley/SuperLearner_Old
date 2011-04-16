# knn{class}
# will only work with binomial
# to create additional algorithms with different values of k, for example k=20
# SL.knn20 <- function(...,k=20){ SL.knn(...,k=k)}

SL.knn <- function(Y.temp, X.temp, newX.temp, family, k=10, ...){
	tryCatch(require(class), warning = function(...){ stop("you have selected knn as a library algorithm but do not have the class package installed")})
	if(family$family=="gaussian") {  
		stop("SL.knn only available for family = binomial()")
	}
	fit.knn <- knn(train=X.temp, test=newX.temp, k=k, cl=Y.temp, prob=TRUE)
	out <- (as.numeric(fit.knn)-1)*attr(fit.knn,"prob") + (1-(as.numeric(fit.knn)-1))*(1-attr(fit.knn,"prob"))
	fit <- list(k=k)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.knn")
	return(foo)
}

# will need original Y and X data for this
predict.SL.knn <- function(object, newdata, X, Y, ...){
	tryCatch(require(class), warning = function(...){ stop("you have selected knn as a library algorithm but do not have the class package installed")})
		fit.knn <- knn(train=X, test=newdata, k=object$k, cl=Y, prob=TRUE)
		out <- (as.numeric(fit.knn)-1)*attr(fit.knn,"prob") + (1-(as.numeric(fit.knn)-1))*(1-attr(fit.knn,"prob"))
		return(out)
}

SL.knn20 <- function(...,k=20){ SL.knn(...,k=k)}
SL.knn30 <- function(...,k=30){ SL.knn(...,k=k)}
SL.knn40 <- function(...,k=40){ SL.knn(...,k=k)}
SL.knn50 <- function(...,k=50){ SL.knn(...,k=k)}
SL.knn60 <- function(...,k=60){ SL.knn(...,k=k)}
SL.knn70 <- function(...,k=70){ SL.knn(...,k=k)}
SL.knn80 <- function(...,k=80){ SL.knn(...,k=k)}
SL.knn90 <- function(...,k=90){ SL.knn(...,k=k)}
SL.knn100 <- function(...,k=100){ SL.knn(...,k=k)}
SL.knn200 <- function(...,k=200){ SL.knn(...,k=k)}
