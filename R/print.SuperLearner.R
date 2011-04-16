print.SuperLearner <- function(x, ...) {
	cat("\nCall: ", deparse(x$call, width.cutoff = .9*getOption("width")), "\n\n", fill = getOption("width"))
	print(cbind(Risk = x$cv.risk, Coef = x$coef))
}

coef.SuperLearner <- function(object, ...) {
	object$coef
}

print.CV.SuperLearner <- function(x, ...) {
	cat("\nCall: ", deparse(x$call, width.cutoff = .9*getOption("width")), "\n\n", fill = getOption("width"))
	cat("Cross-validated predictions from the SuperLearner:  pred.SL \n\nCross-validated predictions from the discrete super learner (cross-validation selector):  pred.discreteSL \n\nWhich library algorithm was the discrete super learner:  whichDiscreteSL \n\nCross-validated prediction for all algorithms in the library:  pred.library\n")
}

coef.CV.SuperLearner <- function(object, ...) {
	object$coef.SL
}
