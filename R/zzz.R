.onAttach <- function(...) {
	cat("Super Learner \n")
	cat("Version: ", utils::packageDescription("SuperLearnerOld")$Version)
	cat("\npackage created on",utils::packageDescription("SuperLearnerOld")$Date," \n")
	cat("\nUse SuperLearnerNews() to see changes from previous versions and latest news\n")
	cat("\nSuggested packages to install for the Super Learner library:\n")
	cat(unlist(strsplit(utils::packageDescription("SuperLearnerOld")$Suggests, ",")), fill=TRUE, sep=",")
	cat("\n")
}

