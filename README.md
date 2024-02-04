
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GradeStat

<!-- badges: start -->

<!-- badges: end -->

The GradeStat software is a statistical package that enables the independent use of methods used in exploratory data analysis.

## Installation

You can install the released version of GradeStat from
[GitHub](https://github.com/) with:

``` r
library(remotes)
install_github("KrzyGajow/GradeStat")
```

## Example

This is a basic example which shows you how to solve a common problem:


```r
library("GradeStat")

X <- matrix( c(0.25,0.1,0.1,0.05,0.2,0.05,0.04,0.03,0.03,0.06,0.02,0.07), 3, 4 )
colnames(X) <- c("j1","j2","j3","j4")
rownames(X) <- c("i1","i2","i3")
X <- AddClWe( X )
XGrade <- TableGCA( X )
GCA( RemoveClWe( XGrade ), 100 )

# Run Shiny web application 
runShinyGradeStat()
```
