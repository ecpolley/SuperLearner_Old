SL.caret <- function(Y.temp, X.temp, newX.temp, family, obsWeights, method = "rf", tuneLength = 3, trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE), ...) {
	tryCatch(require(caret), warning = function(...) {
	    stop("you have selected caret as a library algorithm but do not have the caret package installed")
	})
    if (family$family == "gaussian") {
		fit.train <- caret::train(x = X.temp, y = Y.temp, weights = obsWeights, metric = "RMSE", method = method, tuneLength = tuneLength, trControl = trControl)
		out <- predict(fit.train, newdata = newX.temp, type = "raw")
    }
    if (family$family == "binomial") {
		# outcome must be factor, and have real labels
		Y.temp.f <- as.factor(Y.temp)
		levels(Y.temp.f) <- c("A0", "A1")
		fit.train <- caret::train(x = X.temp, y = Y.temp.f, weights = obsWeights, metric = "Accuracy", method = method, tuneLength = tuneLength, trControl = trControl)
		out <- predict(fit.train, newdata = newX.temp, type = "prob")[, 2]
    }
    fit <- list(object = fit.train)
    foo <- list(out = out, fit = fit)
    class(foo$fit) <- c("SL.caret")
    return(foo)
}

predict.SL.caret <- function(object, newdata, ...) {
	tryCatch(require(caret), warning = function(...) {
	    stop("you have selected caret as a library algorithm but do not have the caret package installed")
	})
	if (object$object$modelType == "Regression") {
		out <- predict(object$object, newdata = newdata, type = "raw")
	} else if (object$object$modelType == "Classification") {
		out <- predict(object$object, newdata = newdata, type = "prob")[, 2]
	}
	return(out)
}

# how to change to a different method:
SL.caret.rpart <- function(..., method = "rpart") {
	SL.caret(..., method = method)
}

