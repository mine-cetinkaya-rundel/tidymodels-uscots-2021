A *very* simple multivariable model
================

## Load packages

``` r
library(DAAG)
library(tidyverse)
library(tidymodels)
```

## Take a peek at the data

``` r
allbacks <- as_tibble(allbacks)
allbacks
```

    ## # A tibble: 15 x 4
    ##    volume  area weight cover
    ##     <dbl> <dbl>  <dbl> <fct>
    ##  1    885   382    800 hb   
    ##  2   1016   468    950 hb   
    ##  3   1125   387   1050 hb   
    ##  4    239   371    350 hb   
    ##  5    701   371    750 hb   
    ##  6    641   367    600 hb   
    ##  7   1228   396   1075 hb   
    ##  8    412     0    250 pb   
    ##  9    953     0    700 pb   
    ## 10    929     0    650 pb   
    ## 11   1492     0    975 pb   
    ## 12    419     0    350 pb   
    ## 13   1010     0    950 pb   
    ## 14    595     0    425 pb   
    ## 15   1034     0    725 pb

## Single predictor

1.  Fit a model predicting weight from volume

``` r
m_simple <- linear_reg() %>%
  set_engine("lm") %>%
  fit(___ ~ ___, data = ___)
```

2.  Print a tidy summary of the model

``` r
___(m_simple)
```

## Multiple predictors

1.  Fit a model predicting weight from volume and cover

``` r
m_multi <- ___ %>%
  ___ %>%
  fit(___ ~ ___, data = ___)
```

2.  Print a tidy summary of the model

## Comparing models

Find the R-squared and the adjusted R-squared of the two models and
compare them.

Hint: You’ll want the `glance()` function to access these statistics.

## Discuss

What felt natural? What seems confusing so far?
