# Overview

rr sim is a package for a robust regression simulation. More in details it checks all possible combinations of the right hand side variables and its impact on the dependent variable. It accompagnies the paper *Robustness and Trade*. This paper builds on the methodology for assessing robustness of variables in the empirical growth literature (Levine and Renelt, 1992; Sala-i-Martin, 1997). The approach involves the partitioning of a dataset into fixed variables that are included in each and every regression, a variable of interest, and other controlling (or switch) variables; and the subsequent regression analysis on the data for varying combinations of variables. The quasi-experiments generate parameter estimates from a wide range of possible regression specifications. 

# Installation

`rrsim` is not currently available from CRAN, but you can install the development version from github with (note that you need to install the package `devtools` to be able to install packages from GitHub:

```{r}
# install.packages("devtools")
# library("devtools")
devtools::install_github("thdegraaff/rrsim")
```

Once installation is completed, the package can be loaded.

```{r}
library(rrsim)
```

# Components 

At the moment, there are only the following two functions (checked and worked) in this package. One to do the simulation of regressions and the other to analyse the results: 

- `rrs((formula, fixed = NA, fe = NA, data, k = 4)` - performs all regressions
- `rrs_analysis(coef, t-values)` - analyses the simulation results
- `rrs_bp(analysis, rm_constant = FALSE)` - create a barplot of the results

## Note

Note that this package at the moment is **work in progress** and that the data it uses is the well-know `mtcars` dataset, which is built in in `R`. The data that is used for the paper is proprietary.

## What is still to be incorporated (...at least)?

- Create a function that displays the distribution of (standardized) coefficients in a sort of funnel plot (or the forest plot of the `meta` package)
  * Know how, but need to create the function
- Create a function that analyses the impact of the **presence** of control variables by using fixed effects on every variable
- Create and use a trade dataset for the examplew
- Update the vignette 
  * Not so important for now, but useful for documentation and first draft of the report

# Usage


# Getting help

If you need assistance using `rrsim`, you can get in touch by emailing [t.de.graaff@vu.nl](t.de.graaff@vu.nl).



