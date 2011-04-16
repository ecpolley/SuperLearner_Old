#  rank.p.glm.R
#  SuperLearner
#  
#  Created by Eric Polley on 2009-03-03.
# rank.p.glm <- function(Y, X, fam=gaussian){
# 	p <- dim(X)[2]
# 	list.p<-rep(NA, p)
# 	names(list.p) <- names(X)
# 	for(jj in 1:p) {
# 		if(var(X[, jj]) <= 0) { list.p[jj] <- 1 ; next}
# 		foo <- glm(Y ~ X[,jj], family=fam)
# 		coef.table <- summary(foo)$coefficients
# 		list.p[jj] <- coef.table[2, 4]
# 	}
# 	return(list.p)
# }

.listPvalueGLM <- function(Y, X, family=gaussian()) {
	p <- ncol(X)
	list.p <- rep(NA, p)
	names(list.p) <- names(X)
	if (family$family == "gaussian") {
		list.p <- apply(X, 2, function(x) { 
			ifelse(var(x) <= 0, 1, cor.test(x, Y)$p.value)
		})
	} else {
		list.p <- apply(X, 2, function(x) { 
			ifelse(var(x) <= 0, 1, summary(glm(Y~x, family=family))$coefficients[2,4])
		})
	}
	return(list.p)
} 

.listPvalueCor <- function(Y, X, method) {
	p <- ncol(X)
	list.p <- rep(NA, p)
	names(list.p) <- names(X)
	list.p <- apply(X, 2, function(x) { 
		ifelse(var(x) <= 0, 1, cor.test(x, Y, method = method)$p.value)
	})
	return(list.p)
}

screen.glmP <- function(Y.temp, X.temp, family, minPvalue = 0.1, minscreen = 2, ...) {
	listp <- .listPvalueGLM(Y=Y.temp, X=X.temp, family=family)
	whichVariable <- (listp <= minPvalue)
	if (sum(whichVariable) <= minscreen) {
		whichVariable[rank(listp) <= minscreen] <- TRUE
	}
	return(whichVariable)
}

screen.glmRank <- function(Y.temp, X.temp, family, rank = 2, ...) {
	if(rank > ncol(X.temp)) {
		rank <- ncol(X.temp)
	}
	listp <- .listPvalueGLM(Y=Y.temp, X=X.temp, family=family)
	whichVariable <- (rank(listp) <= rank)
	return(whichVariable)
}

screen.corP <- function(Y.temp, X.temp, method = "pearson", minPvalue = 0.1, minscreen = 2, ...) {
	listp <- .listPvalueCor(Y=Y.temp, X=X.temp, method = method)
	whichVariable <- (listp <= minPvalue)
	if (sum(whichVariable) <= minscreen) {
		whichVariable[rank(listp) <= minscreen] <- TRUE
	}
	return(whichVariable)
}

screen.corRank <- function(Y.temp, X.temp, method = "pearson", rank = 2, ...) {
	if(rank > ncol(X.temp)) {
		rank <- ncol(X.temp)
	}
	listp <- .listPvalueCor(Y=Y.temp, X=X.temp, method = method)
	whichVariable <- (rank(listp) <= rank)
	return(whichVariable)
}