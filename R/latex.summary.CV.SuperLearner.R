# create a latex table based on the summary(CV.SuperLearner) object
# object <- summary(fitCV)

latex.summary.CV.SuperLearner <- function(object, subset = TRUE, report = c("Risk", "SE", "Min", "Max"), addNamesTable = NULL, booktabs = TRUE, dec = 3, rowname = NULL, ...) {
	if(class(object) != "summary.CV.SuperLearner") stop("object must be from summary.CV.SuperLearner")
	require(Hmisc)
 	# need to split the names and subsets, but treat SuperLearner and discrete SL separate:
	getList <- strsplit(as.character(object$Table$Algorithm)[-c(1, 2)], split = "_")
	getNames <- matrix(unlist(getList), ncol = 2, nrow = length(getList), byrow = TRUE)

	# remove the "screen." for column two
	getNames[!(getNames[, 2] == "All"), 2] <- substring(getNames[!(getNames[, 2] == "All"), 2], first = 8)

	# rules for changing names 
	namesTable <- matrix( c("SL.knn", "SL.knn(10)",
						"SL.knn20", "SL.knn(20)",
						"SL.knn30", "SL.knn(30)",
						"SL.knn40", "SL.knn(40)",
						"SL.knn50", "SL.knn(50)",
						"SL.knn60", "SL.knn(60)",
						"SL.knn70", "SL.knn(70)",
						"SL.knn80", "SL.knn(80)",
						"SL.knn90", "SL.knn(90)",
						"SL.knn100", "SL.knn(100)",
						"SL.knn200", "SL.knn(200)",
						"SL.gam", "SL.gam(df = 2)",
						"SL.gam.3", "SL.gam(df = 3)",
						"SL.gam.4", "SL.gam(df = 4)",
						"SL.gam.5", "SL.gam(df = 5)",
						"SL.glmnet", "SL.glmnet($\\alpha = 1.0$)",
						"SL.glmnet.alpha75", "SL.glmnet($\\alpha = 0.75$)",
						"SL.glmnet.alpha50", "SL.glmnet($\\alpha = 0.50$)",
						"SL.glmnet.alpha25", "SL.glmnet($\\alpha = 0.25$)",
						"SL.nnet", "SL.nnet(size = 2)",
						"SL.nnet.3", "SL.nnet(size = 3)",
						"SL.nnet.4", "SL.nnet(size = 4)",
						"SL.nnet.5", "SL.nnet(size = 5)",
						"corRank", "cor (rank = 2)",
						"corP", "cor ($p$~\\textless~0.1)",
						"corP.01", "cor ($p$~\\textless~0.01)",
						"glmRank", "glm (rank = 2)",
						"glmP", "glm ($p$~\\textless~0.1)"
						), ncol = 2, byrow = TRUE)
	colnames(namesTable) <- c("pre", "post")
	namesTable <- rbind(namesTable, addNamesTable)
	
	for(ii in seq(nrow(namesTable))) {
		# first the prediction algorithms
		getNames[getNames[, 1] == namesTable[ii, 1], 1] <- namesTable[ii, 2]
		# second the screen algorithms
		getNames[getNames[, 2] == namesTable[ii, 1], 2] <- namesTable[ii, 2]
	}

	final <- data.frame(rbind(c("SuperLearner", "--"), c("Discrete SL", "--"), getNames), stringsAsFactors = FALSE)
	colnames(final) <- c("Algorithm", "subset")
	final <- data.frame(final, Risk = object$Table$Ave, SE = object$Table$se, Min = object$Table$Min, Max = object$Table$Max)
	if(!identical(report, c("Risk", "SE", "Min", "Max"))) {
		final <- subset(final, select = c("Algorithm", "subset", report))
	}
	if(!subset) {
		final <- subset(final, select = -subset)
	}
	latex(object = final, booktabs = booktabs, dec = dec, rowname = rowname, ...)
}