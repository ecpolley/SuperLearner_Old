.computeCoef <- function(newZ, Y, cand.names, method, trim, verbose, obsWeights){
	if(method=="NNLS"){
		require(nnls)
		cv.risk <- apply(newZ, 2, function(x) { mean(obsWeights * (x-Y)^2)})
		names(cv.risk) <- cand.names
		
		fit.nnls <- nnls(sqrt(obsWeights) * newZ, sqrt(obsWeights) * Y)
		if(verbose) {
			print(paste("Non-Negative least squares convergence: ", fit.nnls$mode==1))
		}
		init.coef <- coef(fit.nnls)
		init.coef[is.na(init.coef)] <- 0.0
		if(sum(init.coef) > 0) {
			update.coef <- init.coef/sum(init.coef)
		} else {
			warning("All algorithms have zero weight", call. = FALSE)
			update.coef <- init.coef
		}
		update.risk <- mean(obsWeights * (crossprod(t(newZ), update.coef) - Y)^2)
		if(update.risk > min(cv.risk)) {
			message(paste("Cross-validated risk from convex combination is greater than cross-validated risk from one of the algorithms in the library:", cand.names[which.min(cv.risk)], ", You Should check the risk estimate of the SuperLearner using the CV.SuperLearner function"))
		}
	}
	if(method=="NNLS2"){
		cv.risk <- apply(newZ, 2, function(x) { mean(obsWeights * (x-Y)^2)})
		names(cv.risk) <- cand.names
		
		fit.nnls <- .NNLS(x = newZ, y = Y, wt = obsWeights)
		# How to check convergence of .NNLS?
		# if(verbose) {
		# 	print(paste("Non-Negative least squares convergence: ", fit.nnls$mode==1))
		# }
		init.coef <- fit.nnls$solution
		init.coef[init.coef <= 0] <- 0.0
		init.coef[is.na(init.coef)] <- 0.0
		if(sum(init.coef) > 0) {
			update.coef <- init.coef/sum(init.coef)
		} else {
			warning("All algorithms have zero weight", call. = FALSE)
			update.coef <- init.coef
		}
		update.risk <- mean(obsWeights * (crossprod(t(newZ), update.coef) - Y)^2)
		if(update.risk > min(cv.risk)) {
			message(paste("Cross-validated risk from convex combination is greater than cross-validated risk from one of the algorithms in the library:", cand.names[which.min(cv.risk)], ", You Should check the risk estimate of the SuperLearner using the CV.SuperLearner function"))
		}
	}
	if(method=="NNloglik"){
		cv.risk <- apply(newZ, 2, function(x) { -sum(2 * obsWeights * ifelse(Y, log(x), log(1-x))) } )
		names(cv.risk) <- cand.names
		temp.newZ <- trimLogit(newZ, trim=trim)
		fit.nnloglik <- .NNloglik(x=temp.newZ, y=Y, wt = obsWeights)
		if(verbose) {
			print(paste("Non-Negative log-likelihood convergence: ", fit.nnloglik$convergence==0))
		}
		init.coef <- fit.nnloglik$par
		init.coef[init.coef <= 0] <- 0.0
		init.coef[is.na(init.coef)] <- 0
		if(sum(init.coef) > 0) {
			update.coef <- init.coef/sum(init.coef)
		} else {
			warning("All algorithms have zero weight", call. = FALSE)
			update.coef <- init.coef
		}
		update.risk <- -sum(2 * obsWeights * ifelse(Y, log(plogis(crossprod(t(temp.newZ), update.coef))), log(1-plogis(crossprod(t(temp.newZ), update.coef)))))
		if(update.risk > min(cv.risk)) {
			message(paste("Cross-validated risk from convex combination is greater than cross-validated risk from one of the algorithms in the library:",cand.names[which.min(cv.risk)], ", You Should check the risk estimate of the SuperLearner using the CV.SuperLearner function"))
		}
	}
	out <- list(cv.risk=cv.risk, init.coef=init.coef, update.coef=update.coef)
	return(out)
}