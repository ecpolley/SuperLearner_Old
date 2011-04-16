# svm{e1071}
# two types for regression: "nu-regression" and "eps-regression".
# two types for classification: "nu-classification" and "C-classification"
# many other tuning parameters to consider
SL.svm <- function(Y.temp, X.temp, newX.temp, family, type.reg="nu-regression", type.class="nu-classification", nu=0.5,...){
	tryCatch(require(e1071), warning = function(...){ stop("you have selected svm as a library algorithm but do not have the e1071 package installed")})
	if(family$family=="gaussian"){
		fit.svm <- svm(y=Y.temp, x=X.temp, nu=nu, type=type.reg, fitted=FALSE)
		out <- predict(fit.svm, newdata=newX.temp)
		fit <- list(object=fit.svm)
	}
	if(family$family=="binomial"){
		fit.svm <- svm(y=as.factor(Y.temp), x=X.temp, nu=nu, type=type.class, fitted=FALSE, probability=TRUE)
		out <- attr(predict(fit.svm, newdata=newX.temp, probability=TRUE), "prob")[, 2]
		fit <- list(object=fit.svm)
	}
	foo <- list(out=out, fit=fit)
	class(foo$fit) <- c("SL.svm")
	return(foo)
}

predict.SL.svm <- function(object, newdata, family,...){
	tryCatch(require(e1071), warning = function(...) {
		stop("you have selected svm as a library algorithm but do not have the e1071 package installed")
	})
	if(family$family=="gaussian") {
		out <- predict(object$object, newdata=newdata)
	}
	if(family$family=="binomial") {
		out <- attr(predict(object$object, newdata=newdata,probability=TRUE), "prob")[, 2]
	}
	out
}

SL.svm.eps <- function (Y.temp, X.temp, newX.temp, family, type.reg = "eps-regression", 
    type.class = "C-classification", ...) 
{
    tryCatch(require(e1071), warning = function(...) {
        stop("you have selected svm as a library algorithm but do not have the e1071 package installed")
    })
    if (family$family == "gaussian") {
        fit.svm <- svm(y = Y.temp, x = X.temp, type = type.reg, 
            fitted = FALSE)
        out <- predict(fit.svm, newdata = newX.temp)
        fit <- list(object = fit.svm)
    }
    if (family$family == "binomial") {
        fit.svm <- svm(y = as.factor(Y.temp), x = X.temp,
            type = type.class, fitted = FALSE, probability = TRUE)
        out <- attr(predict(fit.svm, newdata = newX.temp, probability = TRUE), 
            "prob")[, 2]
        fit <- list(object = fit.svm)
    }
    foo <- list(out = out, fit = fit)
    class(foo$fit) <- c("SL.svm")
    return(foo)
}
