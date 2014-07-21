# Details on the SL folder #

Functions in this folder can be used to create additional prediction algorithms to be used with the current version of the `SuperLearner` R package. This file contains a short description of each function.

## `create.SL.gam` ##

Creates additional `gam` wrappers with different values for the `deg.gam` parameter. These new wrappers will be located in the global environment after running the function. The default degree for `SL.gam` is 2. With this function, the argument `deg.gam` is a numeric vector and a new `SL.gam.*` function will be created for each element of the vector.

## `create.SL.knn` ##

Creates additional `knn` wrappers with different values for the `k` parameter. These new wrappers will be located in the global environment after running the function. The default value for `SL.knn` is 10. With this function, the argument `k` is a numeric vector and a new `SL.knn.*` function will be created for each element of the vector.

## `create.SL.nnet` ##

Creates additional `nnet` wrappers with different values for the `size` parameter. These new wrappers will be located in the global environment after running the function. The default size for `SL.nnet` is 2. With this function, the argument `size` is a numeric vector and a new `SL.nnet.*` function will be created for each element of the vector.

## `create.SL.glmnet` ##

Creates additional `glmnet` wrappers with different values for the `alpha` parameter. These new wrappers will be located in the global environment after running the function. The default alpha for `SL.glmnet` is 1. With this function, the argument `alpha` is a numeric vector and a new `SL.alpha.*` function will be created for each element of the vector. `alpha` varies between 0 and 1.

## `create.SL.randomForest` ##

Creates additional `randomForest` wrappers changing both `mtry` and `nodesize`.

## `write.SL.template` ##

This function can be used to generate a new file with the template `SL.*` information. The only argument is to specify a connection (usually a file location) for the output. Any additional arguments will be passed to `cat` within the function. For example, you might want to add `append = TRUE` if adding to an existing file.

## `SL.bart` ##

This function implements the `bart` function from the `BayesTree` package (no longer on CRAN)

## `SL.blackboost` ##

This function implements the `blackboost` function from the `mboost` package.

## `SL.DSA` ##

This function implements the `DSA` algorithm from the `DSA` package available at [http://www.stat.berkeley.edu/~laan/Software/index.html](http://www.stat.berkeley.edu/~laan/Software/index.html).

## `SL.leekasso` ##

This function implements the `Leekasso`.

## `SL.naiveBayes` ##

This function implements the `naiveBayes` algorithm from `e1071`.