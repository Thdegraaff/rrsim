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

- `rrs(code_name, Z_names, X_names, data, print_detail = 3)` - Estimates the first stage of a. sorting model



# Usage


# Getting help

If you need assistance using `rrsim`, you can get in touch by emailing [t.de.graaff@vu.nl](t.de.graaff@vu.nl).



