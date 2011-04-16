# Convex Super Learner function.
# 
#  SuperLearner.R
#  SuperLearner
#  
#  Created by Eric Polley on 2009-03-03.

SuperLearner <- function(Y, X, newX = NULL, SL.library, V = 20, shuffle=TRUE, verbose=FALSE, family=gaussian(), method="NNLS", id = NULL, save.fit.library = TRUE, trim.logit = 0.001, obsWeights = NULL, stratifyCV = FALSE){
	require(nnls)
	require(quadprog)
	library <- .createLibrary(SL.library)
	.check.SL.library(SL.library = c(library$predAlgorithm$library, library$screenAlgorithm))
	if(!(method %in% c("NNLS", "NNLS2", "NNloglik"))) {
		stop("Method not currently supported.  Should be NNLS, NNLS2, or NNloglik")
	}
	call <- match.call()
	X <- as.data.frame(X)
	var.names <- names(X)
	N.all <- length(Y)
	p <- dim(X)[2L]
	k.cand <- nrow(library$predAlgorithm)
	k.screen <- length(library$screenAlgorithm)
	newZ <- matrix(NA, N.all, k.cand)
	
	cand.names <- rep(NA, k.cand)
	for(jj in seq(k.cand)) {
		cand.names[jj] <- paste(library$predAlgorithm[jj, 1], library$screenAlgorithm[library$predAlgorithm[jj, 2]], sep="_")
	}
	errorsInCVLibrary <- rep(0, k.cand)
	errorsInLibrary <- rep(0, k.cand)
	
	if(is.null(newX)) {
		newX <- X
	}
	
	fit.library <- vector("list", k.cand)
	names(fit.library) <- cand.names
	
	if(is.character(family))
		family <- get(family, mode="function", envir=parent.frame())
	if(is.function(family))
		family <- family()
	if (is.null(family$family)) {
		print(family)
		stop("'family' not recognized")
	}
	DATA.split <- CVFolds(V=V, N.all = N.all, shuffle = shuffle, id = id, stratifyCV = stratifyCV, Y = Y)
	# if(shuffle) {
	# 	DATA.split<- split(sample(1:N.all), rep(1:V, length=N.all))
	# } else {
	# 	DATA.split<- split(1:N.all, rep(1:V, length=N.all))
	# }
	# if(!is.null(id)) {
	# 	n.id <- length(unique(id))
	# 	id.split <- split(sample(1:n.id),rep(1:V, length=n.id))
	# 	DATA.split <- vector("list", V)
	# 	for(v in seq(V)) {
	# 		DATA.split[[v]] <- which(id %in% unique(id)[id.split[[v]]])
	# 	}
	# } else {
	# 	id <- seq(N.all)
	# }
	
	if(is.null(id)) {
		id <- seq(N.all)
	}
	if(!identical(length(id), N.all)) {
		stop("id vector must have the same dimension as Y")
	}
	if(is.null(obsWeights)) {
		obsWeights <- rep(1, N.all)
	}
	if(!identical(length(obsWeights), N.all)) {
		stop("obsWeights vector must have the same dimension as Y")
	}
	X <- as.data.frame(X)
	newX <- as.data.frame(newX)
	namesX <- names(X)
	if(!identical(colnames(X), colnames(newX))) {
		stop("The variable names and order in newX must be identical to the variable names and order in X")
	}
	# test for character or factor variables
	if (sum(!sapply(X, is.numeric)) > 0) {
		stop("Currently, only numeric variables are allowed.  Please convert any character or factor variables to numeric.")
	}
	# test for missing values
	if (sum(is.na(X)) > 0) {
		stop("missing data is currently not supported")
	}
	if (!is.numeric(Y)) {
		stop("the outcome Y must be a numeric vector")
	}
	
	## now for the candidates for the super learner
	vv <- 1
	for(bb in DATA.split){
		tempLearn <- X[-(bb), , drop=FALSE]
		tempOutcome <- Y[-(bb)]
		tempValid <- X[bb, , drop=FALSE]
		tempWhichScreen <- matrix(NA, nrow = k.screen, ncol = p)
		tempid <- id[-(bb)]
		tempobsWeights <- obsWeights[-(bb)]
		
		for(s in seq(k.screen)){
			testScreen <- try(do.call(library$screenAlgorithm[s], list(Y.temp = tempOutcome, X.temp = tempLearn, family = family, id = tempid, obsWeights = tempobsWeights)))
			if(inherits(testScreen, "try-error")) {
				warning(paste("replacing failed screening,", library$screenAlgorithm[s], ", algorithm with All() in fold", vv ,"\n ")) 
				tempWhichScreen[s, ] <- TRUE
				next
			} else {
				tempWhichScreen[s, ] <- testScreen
			}
			if(verbose) {
				print(paste("Number of covariates in ", library$screenAlgorithm[s], " is: ", sum(tempWhichScreen[s, ]), sep = ""))
			}
		}
		
		for(k in 1:k.cand){
			testAlg <- try(do.call(library$predAlgorithm[k, 1], list(Y.temp = tempOutcome, X.temp = subset(tempLearn, select = tempWhichScreen[library$predAlgorithm[k, 2], ], drop=FALSE), newX.temp = subset(tempValid, select = tempWhichScreen[library$predAlgorithm[k, 2], ], drop=FALSE), family = family, id = tempid, obsWeights = tempobsWeights)))
			if(inherits(testAlg, "try-error")) {
				warning(paste("Error in algorithm", library$predAlgorithm[k, 1], " on fold", vv, "\n  The algorithm will be removed from the SuperLearner library (i.e. given weight 0) \n" )) 
				errorsInCVLibrary[k] <- 1
				next
			} else {
				newZ[bb, k] <- testAlg$out
			}
			if(verbose) {print(paste("CV", cand.names[k]))}
		}#end library
		if(verbose) {print(paste("V-fold:", vv))}
		vv <- vv + 1
	}#end V-fold
	
	if(sum(errorsInCVLibrary) > 0) {
		newZ[, as.logical(errorsInCVLibrary)] <- 0 
	}
	if(all(newZ==0)) {
		stop("All algorithms dropped from library")
	}
	getweights <- .computeCoef(newZ=newZ, Y=Y, cand.names=cand.names, method=method, trim=trim.logit, verbose=verbose, obsWeights = obsWeights)
	init.coef <- getweights$init.coef
	update.coef <- getweights$update.coef
	names(update.coef) <- cand.names
	names(init.coef) <- cand.names
		
	## now fit all candidate on entire training set and predict on newX
	m <- dim(newX)[1L]
	predY <- matrix(NA, m, k.cand)	
	whichScreen <- matrix(NA, nrow = k.screen, ncol = p)
	
	for(s in seq(k.screen)){
		testScreen <- try(do.call(library$screenAlgorithm[s], list(Y.temp = Y, X.temp = X, family = family, id = id, obsWeights = obsWeights)))
		if(inherits(testScreen, "try-error")) {
			warning(paste("replacing failed screening,", library$screenAlgorithm[s], ", algorithm with All() in full data", "\n ")) 
			whichScreen[s, ] <- TRUE
			next
		} else {
			whichScreen[s, ] <- testScreen
		}
		if(verbose) {
			print(paste("Number of covariates in ", library$screenAlgorithm[s], " is: ", sum(whichScreen[s, ]), sep = ""))
		}
	}
	
	for(k in 1:k.cand){
		testAlg <- try(do.call(library$predAlgorithm[k, 1], list(Y.temp=Y, X.temp = subset(X, select=whichScreen[library$predAlgorithm[k, 2], ], drop=FALSE), newX.temp=subset(newX, select=whichScreen[library$predAlgorithm[k, 2], ], drop=FALSE), family=family, id = id, obsWeights = obsWeights)))
		if(inherits(testAlg, "try-error")) {
			warning(paste("Error in algorithm", library$predAlgorithm[k, 1], " on full data", "\n  The algorithm will be removed from the SuperLearner library (i.e. given weight 0) \n" )) 
			errorsInLibrary[k] <- 1
			next
		} else {
			predY[, k] <- testAlg$out
		}
		if(save.fit.library) {
			fit.library[[k]] <- testAlg$fit
		}
		if(verbose) {
			print(paste("full", cand.names[k]))
		}
	}#end library	
	
	if(sum(errorsInLibrary) > 0) {
		if(sum(update.coef[as.logical(errorsInLibrary)]) > 0) {
			warning(paste("re-running estimation of coefficients removing failed algorithm(s) \n Orignial coefficients are: \n", update.coef, "\n"))
			newZ[, as.logical(errorsInLibrary)] <- 0
			if(all(newZ==0)) {
				stop("All algorithms dropped from library")
			}
			getweights <- .computeCoef(newZ=newZ, Y=Y, cand.names=cand.names, method=method, trim=trim.logit, verbose=verbose, obsWeights = obsWeights)
			init.coef <- getweights$init.coef
			update.coef <- getweights$update.coef
			names(update.coef) <- cand.names
			names(init.coef) <- cand.names
		} else {
			warning("coefficients already 0 for all failed algorithm(s)")
		}
	}
	getPred <- .computePred(predY=predY, init.coef=init.coef, update.coef=update.coef, method=method, trim=trim.logit)
	
	# add names of algorithms to the predictions
	colnames(predY) <- cand.names
	
	# clean up when errors in library
	if(sum(errorsInCVLibrary) > 0) {
		getweights$cv.risk[, as.logical(errorsInCVLibrary)] <- NA
	}
	final <- list(call=call, cand.names=cand.names, SL.library=library, SL.predict=getPred$SL.pred.update, init.coef=init.coef, coef=update.coef, library.predict=predY, newZ=newZ, cv.risk=getweights$cv.risk, family=family, fit.library=fit.library, id=id, namesX=namesX, DATA.split=DATA.split, method=method, whichScreen=whichScreen, trim.logit=trim.logit, errorsInCVLibrary = errorsInCVLibrary, errorsInLibrary = errorsInLibrary, obsWeights = obsWeights)
	class(final) <- c("SuperLearner")
	return(final)
}