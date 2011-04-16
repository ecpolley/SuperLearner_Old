# bart {BayesTree}
SL.bart <- function(Y.temp, X.temp, newX.temp, family, ntree=300, sigdf=3, sigquant=0.90, k=2, power=2, base=0.95, binaryOffset=0, ndpost=1000, nskip=100, ...) { 
	tryCatch(require(BayesTree), warning = function(...) {
		stop("you have selected bart as a library algorithm but do not have the BayesTree package installed")
		} )

	if(family$family=="gaussian"){
		fitBart <- bart(x.train=X.temp, y.train=Y.temp, x.test=newX.temp, ntree=ntree, sigdf=sigdf, sigquant=sigquant, k=k, power=power, base=base, binaryOffset=binaryOffset, ndpost=ndpost, nskip=nskip, verbose=FALSE)
	}
	if(family$family=="binomial"){
		fitBart <- bart(x.train=X.temp, y.train=as.factor(Y.temp), x.test=newX.temp, ntree=ntree, sigdf=sigdf, sigquant=sigquant, k=k, power=power, base=base, binaryOffset=binaryOffset, ndpost=ndpost, nskip=nskip, verbose=FALSE)
	}
	if(family$family=="gaussian"){
		out <- fitBart$yhat.test.mean
	}
	if(family$family=="binomial"){
		out <- pnorm(apply(fitBart$yhat.test, 2, mean))
	}
	fit <- list(object = fitBart)
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.bart")
	return(foo)
}

# 
predict.SL.bart <- function(object, newdata, family, X=NULL, Y=NULL, ...) {
	tryCatch(require(BayesTree), warning = function(...){ stop("you have selected bart as a library algorithm but do not have the BayesTree package installed")})

	stop("no predict method currently available for bart")
}