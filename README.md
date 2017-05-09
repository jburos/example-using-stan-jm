# example-using-stan-jm

Worked examples using stan-jm code on public datasets

This repository contains example analyses using `stan_jm`, a development branch of [rstanarm](https://github.com/stan-dev/rstanarm) 
available at [sambrilleman/rstanarm](https://github.com/sambrilleman/rstanarm).

# Installation

Since this code references what are often bleeding-edge development versions of rstanarm, and 
because [packrat](https://github.com/rstudio/packrat) is known to have issues installing development versions
of rstanarm (see rstudio/packrat#356, for example), this code uses a custom dedicated library to isolate these packages from
those elsewhere on your system.

This is maintained by two files:

* `.Rprofile`: creates an `Rlib` directory if it doesn't exist, updates .libPaths() to include it, and sets the `cran` repo
* `setup.R`: will install necessary packages (likely into your custom `Rlib`)

To configure your local setup to run this code, you should therefore only have to do the following:

```r
cd /path/to/example-using-stan-jm
Rscript setup.R
```

