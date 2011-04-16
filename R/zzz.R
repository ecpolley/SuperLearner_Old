.onAttach <- function(...) {
	cat("Super Learner \n")
	cat("Version: ", utils::packageDescription("SuperLearner")$Version)
	cat("\npackage created on",utils::packageDescription("SuperLearner")$Date," \n")
	cat("\nUse SuperLearnerNews() to see changes from previous versions and latest news\n")
	cat("\nSuggested packages to install for the Super Learner library:\n")
	cat(unlist(strsplit(utils::packageDescription("SuperLearner")$Suggests, ",")), fill=TRUE, sep=",")
	cat("\n")
}

