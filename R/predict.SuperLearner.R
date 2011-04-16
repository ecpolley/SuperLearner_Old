predict.SuperLearner <- function(object, newdata, X=NULL, Y=NULL, ...){
	if(missing(newdata)) {
		return(object$SL.predict)
	}
	newdata <- as.data.frame(newdata)
	if(!identical(object$namesX, colnames(newdata))) {
		stop("The variable names and order in newdata must be identical to the variable names and order in X from the original Super Learner model\n")
	}
	cand.names <- object$cand.names
	k.cand <- length(cand.names)
	fit.library <- matrix(NA, nrow=nrow(newdata), ncol=k.cand)
	
	for(k in 1:k.cand){
		fit.library[, k]  <- do.call(predict, 
			list(object = object$fit.library[[k]], 
				newdata = subset(newdata, select=object$whichScreen[object$SL.library$predAlgorithm[k, 2], ], drop=FALSE), 
				family=object$family, 
				X = if(is.null(X)) NULL else subset(X, select = object$whichScreen[object$SL.library$predAlgorithm[k, 2], ], drop=FALSE), 
				Y=Y))
	}
	
	if(object$method == "NNLS"){
		fit <- crossprod(t(fit.library), object$coef)
	}
	if(object$method == "NNloglik"){
		fit <- plogis(crossprod(t(trimLogit(fit.library, trim=object$trim.logit)), object$coef))
	}
	foo <- list(fit=fit, fit.library=fit.library, cand.names=cand.names)
	return(foo)
		
}

##################################################################################################
# predict.SuperLearner <- function(object, newdata, X=NULL, Y=NULL, ...){
# 	if(class(object)!="SuperLearner") {
# 		stop("object is not a Super Learner object")
# 	}
# 	object
# 	if(missing(newdata)) {
# 		return(object$SL.predict)
# 	}
# 	newdata <- as.data.frame(newdata)
# 	if(!identical(object$namesX, colnames(newdata))) {
# 		stop("The variable names and order in newdata must be identical to the variable names and order in X from the original Super Learner model\n")
# 	}
# 	if(object$method=="NNloglik") {
# 		stop("prediction method currently not implemented for NNloglik method")
# 	}
# 	
# 	predict.SL.library <- paste("predict.",object$SL.library,sep="")
# 	predict.cand.names <- predict.SL.library
# 
# 	k.cand <- length(predict.SL.library)
# 	out.library <- matrix(NA, nrow=dim(newdata)[1], ncol=length(predict.cand.names))
# 	out <- rep(NA, dim(newdata)[1])
# 		
# 	for(k in 1:k.cand){
# 		out.library[,k] <- eval(call(name=predict.SL.library[k], newdata=newdata, object=object$fit.library[[k]], family=object$family, X=X, Y=Y))
# 	}
# 
# 	out <- crossprod(t(out.library), object$coef)
# 	foo <- list(out=out, out.library=out.library, predict.cand.names=predict.cand.names)
# 	return(foo)
# }