stick
========

An [R package](http://www.r-project.org/) to create stickmen and stick women on a plot.

how to use this code
--------

To install this package you will need:
* R
* Rtools

Note that you must manually update your environmental variable PATH to include these utilities.

This package includes comments in **roxygen2** format. 
From R (and when located one level above folder saturate) run the command 
`roxygenise("stick")` to build the help files. 
To build the package on Windows use the command `R CMD INSTALL --build stick` from the command window.
This package also includes tests in **testthat** format. From R run the call `test_package("stick")`.

what is this code for?
--------

This R package contains functions for adding stickpeople to a plot.

```R
set.seed(68331)
plotStick(x = runif(100), y = runif(100))
```

![random uniform placement of stickmen](https://raw.githubusercontent.com/EconometricsBySimulation/R-Graphics/master/plotStick_runif100.png "plotStick(x = runif(100), y = runif(100))")

```R
plotStick(x = 1:10, y = cos(1:10),
    hat = c("none", "shapka", "beanie", "fedora"), 
    col = rainbow(10), cex = 2)
```

![cos placement of stickmen in hats](https://raw.githubusercontent.com/EconometricsBySimulation/R-Graphics/master/plotStick_cos_1_10.png "plotStick(x = 1:10, y = cos(1:10),
    hat = c('none', 'shapka', 'beanie', 'fedora'), 
    col = rainbow(10), cex = 2)")

