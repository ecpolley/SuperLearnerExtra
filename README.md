# SuperLearnerExtra #

This package contains a collection of wrappers that may be helpful for running `SuperLearner`. The package mostly contains wrappers from previous projects that are not general enough to be added to the main `SuperLearner` package but might be helpful as a reference. The package contains 3 folders, `SL`, `screen` and `method`. The `SL` folder contains prediction algorithms, the `screen` folder contains screening algorithms, and the `method` folder contains alternative methods (like NNLS()).

You can directly access each individual file or if you prefer to work with the files inside an R package, the folder `SuperLearnerExtra` contains all the functions and can be built as a valid R package by:

1.  Click the downloads link and select the file `SuperLearnerExtra_1.0-0.tar.gz`
2.  Run `R CMD INSTALL SuperLearnerExtra_1.0-0.tar.gz`
3.  Open R and load the package with `library(SuperLearnerExtra)`

Note that the above steps assume your system is setup to install packages from sources. A Windows binary package is currently not available for `SuperLearnerExtra`.
