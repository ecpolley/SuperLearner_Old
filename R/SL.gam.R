## gam{gam}
## generalized additive models (degree = 2)
# functions considers any variable with more than 4 unique values to be continuous and able to be in smoothing splines. 
# easy to add additional algorithms with different degrees
# SL.gam.3 <- function(...,deg.gam=3){SL.gam(...,deg.gam=deg.gam)}

SL.gam <- function(Y.temp, X.temp, newX.temp, family, obsWeights, deg.gam=2,...){
	tryCatch(require(gam), warning = function(...){ stop("you have selected gam as a library algorithm but do not have the gam package installed")})
	cts.x <- apply(X.temp, 2, function(x){ (length(unique(x)) > 4)})
	if (sum(!cts.x) > 0) { 
		gam.model <- as.formula(paste("Y.temp~", paste(paste("s(",colnames(X.temp[,cts.x,drop=FALSE]),",",deg.gam,")",sep=""), collapse="+"),"+",paste(colnames(X.temp[,!cts.x,drop=FALSE]), collapse="+")))
	} else {
		gam.model <- as.formula(paste("Y.temp~", paste(paste("s(",colnames(X.temp[,cts.x,drop=FALSE]),",",deg.gam,")",sep=""), collapse="+")))
	}
	# fix for when all variables are binomial
	if (sum(!cts.x)==length(cts.x)) {
		gam.model <- as.formula(paste("Y.temp~", paste(colnames(X.temp), collapse="+"), sep=""))
	}
	
	fit.gam <- gam::gam(gam.model, data=X.temp, family=family, control=gam.control(maxit=50, bf.maxit=50), weights = obsWeights)
	out <- predict(fit.gam, newdata=newX.temp, type="response")
	fit <- list(object=fit.gam)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.gam")
	return(foo)
}

predict.SL.gam <- function(object, newdata, ...){
	tryCatch(require(gam), warning = function(...){ stop("you have selected gam as a library algorithm but do not have the gam package installed")})
	out <- gam::predict.gam(object=object$object, newdata=newdata, type="response")
	out
}

SL.gam.3 <- function(..., deg.gam=3){SL.gam(..., deg.gam=deg.gam)}
SL.gam.4 <- function(..., deg.gam=4){SL.gam(..., deg.gam=deg.gam)}
SL.gam.5 <- function(..., deg.gam=5){SL.gam(..., deg.gam=deg.gam)}
