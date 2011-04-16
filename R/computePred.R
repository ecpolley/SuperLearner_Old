
.computePred <- function(predY, init.coef, update.coef, method, trim){
	if(method=="NNLS"){
		SL.pred.init <- crossprod(t(predY), init.coef)
		SL.pred.update <- crossprod(t(predY), update.coef)
		# SL.pred.init <- predY %*% init.coef
		# SL.pred.update <- predY %*% update.coef
	}
	if(method=="NNLS2"){
		SL.pred.init <- crossprod(t(predY), init.coef)
		SL.pred.update <- crossprod(t(predY), update.coef)
	}
	if(method=="NNloglik"){
		SL.pred.init <- plogis(crossprod(t(trimLogit(predY, trim=trim)), init.coef))
		SL.pred.update <- plogis(crossprod(t(trimLogit(predY, trim=trim)), update.coef))
	}
	out <- list(SL.pred.init=SL.pred.init, SL.pred.update=SL.pred.update)
	return(out)
}