# Details on the SL-fda folder

Functions in this folder contain prediction algorithms for regressing continuous scalar 
outcomes on a functional covariate to be used with `SuperLearner` package. 
This file contains a short description of each function.  Note that the supplied wrappers do 
not define `predict` methods. 
More detailed descriptions of the methods defined here and experiments on some benchmark data sets
can be found in  Jeff Goldsmith, Fabian Scheipl (2013): “Estimator Selection and Combination in Scalar-on-Function Regression”

### `SL.fgam`
Functional generalized additive model as implemented in the `fgam()` function in the `refund` package.

M. W. McLean, G. Hooker, A.-M. Staicu, F. Scheipl, D. Ruppert (to appear). 
[*Functional Generalized Additive Models*](http://www.tandfonline.com/eprint/jBTSxuMkDyX4N5IqFcpT/full).
Journal of Computational and Graphical Statistics,

### `SL.flasso`
FPC-based LASSO model using `cv.glmnet()`  in package `glmnet`.

<!-- 
### `SL.flirti`
Wraps the code supplied with 

Gareth M. James, Jing Wang, Ji Zhu (2009).
[*Functional linear regression that's interpretable*](http://arxiv.org/abs/0908.2918).
-->

### `SL.flm`
Spline-based functional linear model with locally adaptive penalty and REML based smoothing
parameter selection, using `gam()` from package `mgcv`. 
 
### `SL.fpcr`
Functional principal component regression, wraps `fpcr()` from package `refund`. 

Reiss, P. T., and Ogden, R. T. (2007). *Functional principal component regression and functional partial least squares*. Journal of the American Statistical Association, 102, 984--996.

### `SL.fregrenp`
Kernel-based functional regression, wraps `fregrenp()` from package `fda.usc`. 


Manuel Febrero-Bande, Manuel Oviedo de la Fuente (2012) [*Statistical Computing in Functional Data Analysis: The R Package `fda.usc`.*](http://www.jstatsoft.org/v51/i04/) Journal of Statistical Software, 51(4)

### `SL.pclm`
FPC-based linear model with bootstrap based sub-model selection.

### `SL.pfr`
Spline based penalized functional regression using `pfr()` from package `refund`.


Goldsmith, J., Bobb, J., Crainiceanu, C., Caffo, B., and Reich, D. (2011). 
*Penalized functional regression.* Journal of Computational and Graphical Statistics, 20(4), 830--851.

### `SL.plsreg`
Penalized partial least squares regression using `penalized.pls.cv()` from package `ppls`. 

### `SL.sisr`
Single-index signal regression using locally adaptive penalties and REML based smoothing
parameter selection, implemented with `gam()` from package `mgcv`. 

### `SL.spikeslabgam`
FPC-based bayesian model averaging for functional additive models, using `spikeSlabGAM()`  from package 
`spikeSlabGAM()`.

F. Scheipl, L. Fahrmeir, T. Kneib (2012): 
[*Spike-and-Slab Priors for Function Selection in Structured Additive Regression Models*.](http://arxiv.org/abs/1105.5250)
Journal of the American Statistical Association, 107(500):1518-1532

### `SL.wnet`
Functional regression with generalized elastic net in the wavelet domain, implemented using `wnet()`  from package 
`refund()`.

Zhao, Y., Ogden, R. T., and Reiss, P. T. (2013). *Wavelet-based LASSO in functional linear regression.* Journal of Computational and Graphical Statistics
