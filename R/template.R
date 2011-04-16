# 
SL.template <- function(Y.temp, X.temp, newX.temp, family, obsWeights, id, ...){
		if(family$family=="gaussian"){
		# insert estimation and prediction function 
		}
		if(family$family=="binomial"){
		# insert estimation and prediction function 
		}
		# out returns predicted responses (on the scale of the outcome)
		out <- numeric()
		# fit returns all objects needed for predict.SL.template
		# fit <- list(object = )
		fit <- vector("list", length=0)
		foo <- list(out=out, fit=fit)
		class(foo$fit) <- c("SL.template")
		return(foo)
}

# 
predict.SL.template <- function(object, newdata, family, X=NULL, Y=NULL,...) {
	# insert prediction function
	out <- numeric()
	return(out)
}