# DSA{DSA}
# may want to change:
# maxsize
# maxorderint
# maxsumofpow
# vfold
# SL.DSA.2 <- function(...,maxsize=2*ncol(X.temp),maxorderint=2,maxsumofpow=2,Smove=FALSE,vfold=10){SL.DSA(...,maxsize=maxsize,maxorderint=maxorderint,maxsumofpow=maxsumofpow,Smove=Smove,vfold=vfold)}
SL.DSA <- function(Y.temp, X.temp, newX.temp, family, obsWeights, maxsize=ncol(X.temp), maxorderint=1, maxsumofpow=1, Dmove=TRUE, Smove=TRUE, vfold=5, ...){
	tryCatch(require(DSA), warning = function(...){ stop("you have selected DSA as a library algorithm but do not have the DSA package installed")})
	dsaweights <- matrix(obsWeights, nrow = (vfold +1), ncol = nrow(X.temp), byrow = TRUE)
	fit.DSA <- DSA(Y.temp~1, data=data.frame(Y.temp,X.temp), family=family, maxsize=maxsize, maxorderint=maxorderint, maxsumofpow=maxsumofpow, Dmove=Dmove, Smove=Smove, vfold=vfold, weights = dsaweights)
	out <- predict(fit.DSA, newdata=newX.temp)
	if(family$family=="binomial"){ out <- 1 / (1 + exp(-out))}
	fit <- list(object=fit.DSA)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.DSA")
	return(foo)
}

# 
predict.SL.DSA <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	tryCatch(require(DSA), warning = function(...){ stop("you have selected DSA as a library algorithm but do not have the DSA package installed")})
	out <- predict(object=object$object, newdata=newdata)
	if(family$family=="binomial"){ out <- 1 / (1 + exp(-out))}
	return(out)
}

SL.DSA.2 <- function(..., X.temp, maxsize=2*ncol(X.temp), maxorderint=2, maxsumofpow=2, Smove=FALSE,vfold=10) {
	SL.DSA(..., X.temp=X.temp, maxsize=maxsize, maxorderint=maxorderint, maxsumofpow=maxsumofpow, Smove=Smove, vfold=vfold)
}
