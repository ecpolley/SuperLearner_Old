# screening template
All <- function(...) {
	return(TRUE)
}

# screen functions must return a logical vector of length ncol(X)
screen.template <-function (Y.temp, X.temp, family, obsWeights, id, ...) 
{
    if (family$family == "gaussian") {
	
    }
    if (family$family == "binomial") {
	
    }
	whichVariable <- rep(TRUE, ncol(X.temp))
    return(whichVariable)
}