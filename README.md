# SuperLearner_Old

This is the old version of the SuperLearner R package (version 1.1-20). This version will no longer be updated. The new version of the R package is now available at [SuperLearner](https://github.com/ecpolley/SuperLearner)

## How to install from Github ##

The folders here are the source documents for creating the SuperLearner R package. Follow these steps to build and install the package on your computer:

1.  Download the entire repository using the **Downloads** link in the upper right.
2.  Unzip the downloaded file. [Of course, if you have git, you could replace steps 1 and 2 with: `git clone git://github.com/ecpolley/SuperLearner_Old.git`]
3.  Run `R CMD build /path/to/folder`
4.  You should now have a file called SuperLearnerOld\_1.1-20.tar.gz
5.  Run `R CMD INSTALL SuperLearnerOld_1.1-20.tar.gz`
6.  Alternative to step 5, in R use the command `install.packages('SuperLearnerOld_1.1-20.tar.gz', repos = NULL, type = 'source')` assuming you are in the correct directory for the *.tar.gz file (or add full path to the command)

The package should now exist in your R library and can be loaded with the command `library(SuperLearnerOld)`.