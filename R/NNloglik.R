.NNloglik <- function(x, y, wt=rep(1, length(y)), start=rep(0,ncol(x)),...){
	# adapted from MASS pg 445
	fmin <- function(beta, X, y, w) {
		p <- plogis(crossprod(t(X), beta))
		-sum(2 * w * ifelse(y, log(p), log(1-p)))
	}
	gmin <- function(beta, X, y, w) {
		eta <- X %*% beta
		p <- plogis(eta)
		-2 * t(w * dlogis(eta) * ifelse(y, 1/p, -1/(1-p))) %*% X
	}
	fit <- optim(start, fmin, gmin, X=x, y=y, w=wt, method="L-BFGS-B",lower=0,...)
	invisible(fit)
}

.NNLS <- function(x, y, wt=rep(1, length(y)), ...) {
	require(quadprog)
	wX <- sqrt(wt) * x
	wY <- sqrt(wt) * y
	D <- t(wX) %*% wX
	d <- t(t(wY) %*% wX)
	A <- diag(ncol(wX))
	b <- rep(0, ncol(wX))
	fit <- solve.QP(D=D, d=d, A=t(A), b=b, meq=0)
	invisible(fit)
}