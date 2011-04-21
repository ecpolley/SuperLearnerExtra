# Details on the SL folder #

Functions in this folder can be used to create additional prediction algorithms to be used with the current version of the `SuperLearner` R package. This file contains a short description of each function.

## `create.SL.gam.R` ##

Creates additional `gam` wrappers with different values for the `deg.gam` parameter. These new wrappers will be located in the global environment after running the function. The default degree for `SL.gam` is 2. With this function, the argument `deg.gam` is a numeric vector and a new `SL.gam.*` function will be created for each element of the vector.

## `create.SL.knn.R` ##

Creates additional `knn` wrappers with different values for the `k` parameter. These new wrappers will be located in the global environment after running the function. The default value for `SL.knn` is 10. With this function, the argument `k` is a numeric vector and a new `SL.knn.*` function will be created for each element of the vector.

## `create.SL.nnet.R` ##

Creates additional `nnet` wrappers with different values for the `size` parameter. These new wrappers will be located in the global environment after running the function. The default size for `SL.nnet` is 2. With this function, the argument `size` is a numeric vector and a new `SL.nnet.*` function will be created for each element of the vector.

## `write.SL.template.R` ##

This function can be used to generate a new file with the template `SL.*` information. The only argument is to specify a connection (usually a file location) for the output. Any additional arguments will be passed to `cat` within the function. For example, you might want to add `append = TRUE` if adding to an existing file.