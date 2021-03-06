listWrappers <- function(what = "both") {
	if(what == "both") {
		cat("All prediction algorithm wrappers in SuperLearner:\n")
		print(ls("package:SuperLearnerOld", pattern="^[S]L"))
		cat("\nAll screening algorithm wrappers in SuperLearner:\n")
		print("All")
		print(ls("package:SuperLearnerOld", pattern="screen"))
	} else if(what == "SL") {
		cat("All prediction algorithm wrappers in SuperLearner:\n")
		print(ls("package:SuperLearnerOld", pattern="^[S]L"))
	} else if(what == "screen") {
		cat("All screening algorithm wrappers in SuperLearner:\n")
		print("All")
		print(ls("package:SuperLearnerOld", pattern="screen"))
	} else {
		cat("All functions in SuperLearner:\n")
		print(ls("package:SuperLearnerOld"))
	}
}