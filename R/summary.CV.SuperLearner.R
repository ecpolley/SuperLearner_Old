summary.CV.SuperLearner <- function(object, ...) {
	library.names <- object$CV.fit.SL[[1]]$cand.names
	V <- object$V[[1]]
	n <- length(object$Y)
	Risk.SL <- rep(NA, length = V)
	Risk.dSL <- rep(NA, length = V)
	Risk.library <- matrix(NA, nrow = length(library.names), ncol = V)
	rownames(Risk.library) <- library.names
	
	if (object$method %in% c("NNLS", "NNLS2")) {
		for (ii in seq_len(V)) {
			Risk.SL[ii] <- mean(object$obsWeights[object$folds[[ii]]] * (object$Y[object$folds[[ii]]] - object$pred.SL[object$folds[[ii]]])^2)
			Risk.dSL[ii] <- mean(object$obsWeights[object$folds[[ii]]] * (object$Y[object$folds[[ii]]] - object$pred.discreteSL[object$folds[[ii]]])^2)
			Risk.library[, ii] <- apply(object$pred.library[object$folds[[ii]], ], 2, function(x) mean(object$obsWeights[object$folds[[ii]]] * (object$Y[object$folds[[ii]]] - x)^2))	
		}
		# se <- rep.int(NA, (length(library.names) + 2))
		se <- (1 / sqrt(n)) * c(sd(object$obsWeights * (object$Y - object$pred.SL)^2), sd(object$obsWeights * (object$Y - object$pred.discreteSL)^2), apply(object$pred.library, 2, function(x) sd(object$obsWeights * (object$Y - x)^2)))
	} else if (object$method %in% c("NNloglik")) {
		for (ii in seq_len(V)) {
			Risk.SL[ii] <- -mean(object$obsWeights[object$folds[[ii]]] * ifelse(object$Y[object$folds[[ii]]], log(object$pred.SL[object$folds[[ii]]]), log(1-object$pred.SL[object$folds[[ii]]])))
			Risk.dSL[ii] <- -mean(object$obsWeights[object$folds[[ii]]] * ifelse(object$Y[object$folds[[ii]]], log(object$pred.discreteSL[object$folds[[ii]]]), log(1 - object$pred.discreteSL[object$folds[[ii]]])))
			Risk.library[, ii] <- apply(object$pred.library[object$folds[[ii]], ], 2, function(x) {
				-mean(object$obsWeights[object$folds[[ii]]] * ifelse(object$Y[object$folds[[ii]]], log(x), log(1-x)))
			})	
		}
		se <- rep.int(NA, (length(library.names) + 2))
	} else {
		stop("summary function not available for SuperLearner with loss function/method used")
	}
	
	Table <- data.frame(Algorithm = c("Super Learner", "Discrete SL", library.names), Ave = c(mean(Risk.SL), mean(Risk.dSL), apply(Risk.library, 1, mean)), se = se, Min = c(min(Risk.SL), min(Risk.dSL), apply(Risk.library, 1, min)), Max = c(max(Risk.SL), max(Risk.dSL), apply(Risk.library, 1, max)))
	out <- list(call = object$call, method = object$method, V=V, Risk.SL = Risk.SL, Risk.dSL = Risk.dSL, Risk.library = Risk.library, Table = Table)
	class(out) <- "summary.CV.SuperLearner"
	return(out)
}

print.summary.CV.SuperLearner <- function(x, digits = max(2, getOption("digits") - 2), ...) {
	cat("\nCall: ", deparse(x$call, width.cutoff = .9*getOption("width")), "\n", fill = getOption("width"))
	cat("Risk is based on: ")
	if(x$method %in% c("NNLS", "NNLS2")) {
		cat("Least Squares (Mean Squared Error)")
	} else if (x$method %in% c("NNloglik")) {
		cat("Negative Log Likelihood (-2*log(L))")
	} else {
		stop("summary method not available")
	}
	cat("\n\nAll risk estimates are based on V = ", x$V, "\n\n")
	print(x$Table, digits = digits, row.names = FALSE)
}