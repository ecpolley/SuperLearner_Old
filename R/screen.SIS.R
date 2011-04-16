screen.SIS <- function (Y.temp, X.temp, family, nsis = NULL, tune.method="BIC", inittype="NoPen", vartype="second", useISISind = TRUE, minScreen = 5,...) 
{
    tryCatch(require(SIS), warning = function(...) {
        stop("you have selected SIS as a library algorithm but do not have the SIS package installed")
	})
	if (family$family == "gaussian") {
		fitSIS <- GLMvarISISscad(x=X.temp, y=Y.temp, family=family, nsis=nsis, tune.method=tune.method, inittype=inittype, vartype=vartype)
    }
    if (family$family == "binomial") {
		fitSIS <- GLMvarISISscad(x=X.temp, y=Y.temp, family=family, nsis=nsis, tune.method=tune.method, inittype=inittype, vartype=vartype)
    }
	if(useISISind) { 
    	whichVariable <- (1:ncol(X.temp) %in% fitSIS$ISISind)
	} else {
		whichVariable <- (1:ncol(X.temp) %in% fitSIS$SISind)
	}
	# check more than minScreen in screened set
	if(sum(whichVariable) < minScreen) {
		warning("fewer than minScreen variables in screen.SIS, using initial ranking")
		whichVariable <- (fitSIS$initRANKorder1 < minScreen)
	}
    return(whichVariable)
}

# tune.method:
# CV
# AIC
# BIC
# 