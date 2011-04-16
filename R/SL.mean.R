SL.mean <- function (Y.temp, X.temp, newX.temp, family, obsWeights, id, 
    ...) 
{
    meanY <- weighted.mean(Y.temp, w = obsWeights)
	out <- rep.int(meanY, times = nrow(newX.temp))
    fit <- list(object = meanY)
    foo <- list(out = out, fit = fit)
    class(foo$fit) <- c("SL.mean")
    return(foo)
}

predict.SL.mean <- function (object, newdata, family, X = NULL, Y = NULL, ...) 
{
    out <- rep.int(object$object, times = nrow(newdata))
    return(out)
}
