SL.polymars.dimreduce <- function (Y.temp, X.temp, newX.temp, family, min.p = 0.1,  ...) 
{
    tryCatch(require(polspline), warning = function(...) {
        stop("you have selected polymars or polyclass as a library algorithm but do not have the polspline package installed")
    })
    if (family$family == "gaussian") {
		pValues <- .listPvalueGLM(Y=Y.temp, X=X.temp, fam=gaussian())
		X.temp.red <- X.temp[, (pValues < min.p)]
        fit.mars <- polymars(Y.temp, X.temp.red)
        out <- predict.polymars(fit.mars, x = as.matrix(newX.temp[, (pValues < min.p)]))
        fit <- list(object = fit.mars)
    }
    if (family$family == "binomial") {
		pValues <- .listPvalueGLM(Y=Y.temp, X=X.temp, fam=binomial())
		X.temp.red <- X.temp[, (pValues < min.p)]
        fit.mars <- polyclass(Y.temp, X.temp.red, cv = 5)
        out <- ppolyclass(cov = newX.temp[, (pValues < min.p)], fit = fit.mars)[, 
            2]
        fit <- list(fit = fit.mars)
    }
    foo <- list(out = out, fit = fit, pValues = pValues, min.p=min.p)
    class(foo$fit) <- c("SL.polymars.dimreduce")
    return(foo)
}
