
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `cdsampler`: Sampling from correlated discrete variables

This repository provides an extremely basic way to sample from a set of
correlated discrete variables.

It’s provided with no guarantees of correctness, but it still might be
helpful to you.

The basic task is as follows:

> Sample from a population consistent with a set of two-way contingency
> tables.

The approach is to do the following:

-   Sample data directly from a distribution in which all covariates are
    independent (but which matches the marginals of the desired
    distribution).
-   Calculate, for a given row, a likelihood ratio where the target
    likelihood is a pseudo likelihood constructed from the marginal
    pairwise distributions (see [Cox and
    Reid (2004)](https://www.jstor.org/stable/20441134)). In short,
    multiply the probability of observing each pair of covariates.
    Divide this by the product of univariate marginal probabilities for
    each covariate.
-   Perform rejection sampling, where the normalizing constant is
    estimated as a little larger than the largest observed likelihood
    ratio. I only do this estimation with a frequency scaling with the
    log of samples.

# FAQ

### Is this perfectly correct?

It doesn’t seem to be exactly right, but it isn’t too far off. Check the
simulation in this README for a sense of the accuracy you should expect.

### Should I use this?

Use this if you’re debugging something and you just want something that
will give you some reasonable samples (i.e. “give you some discrete
variables correlated in approximately the way you specify”) without too
much setup.

Don’t use this if you need to be confident that your samples obey
exactly the specified distribution. One would need to do more validation
for that.

### Why might this be wrong?

I think it might be possible to specify a distribution in terms of
pairwise probabilities that isn’t actually possible as a valid joint
distribution. In the examples below, you can see that errors are a bit
larger when I just choose some random pairwise correlations and sample
relative to when I construct a full joint distribution and sample
according to observed pairwise probabilities. In the latter case, of
course, it is definitely a feasible joint distribution.

I haven’t done enough of a simulation study to say whether this is
actually right, though.

### Why does this exist?

It exists because I needed to debug a problem I was seeing in a
statistical method that only came out on some real data. I didn’t have
access to the full data (and it couldn’t be made public), so debugging
on simulated data was all that I could do.

### Is this efficient?

It’s probably fine. The main reason it is as slow as it is is just
because it’s rejection sampling, which isn’t super efficient. It will
probably be most efficient when correlations are low (and therefore the
product distribution does a good job of approximation making the
rejection rate low). As far as rejection samplers in pure R go, I don’t
think this should be too bad, though.

### Will this break?

I’m sure you can find a way. To keep things as copacetic, don’t name
your variables anything weird. The covariate names and levels need to
abide by the rules of R’s variable naming because I’m using environments
in lieu of real hashmaps internally.

The error messages might not be super-informative. Sorry.

# Installation

This project will not live on CRAN. Install from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ddimmery/cdsampler")
```

# Examples

## Starting from a set of pairwise contingency tables

``` r
library(tidyr)
library(cdsampler)
suppressMessages(library(dplyr))

set.seed(100)

pr1 <- expand_grid(
    x = letters[1:3],
    y = letters[1:2]
) %>% mutate(n = sample(n()), prob = n / sum(n))

pr2 <- expand_grid(
    x = letters[1:3],
    z = letters[1:4]
) %>% mutate(n = sample(n()), prob = n / sum(n))

pr3 <- expand_grid(
    y = letters[1:2],
    z = letters[1:4]
) %>% mutate(n = sample(n()), prob = n / sum(n))

mm <- get_marginals_from_pairwise(pr1, pr2, pr3)

sdf <- sample_correlated_discrete(1000, pr1, pr2, pr3)

tab <- calculate_two_way_contingencies(sdf, x, y, z)

inner_join(
    pr1,
    tab[[1]],
    by = c("x", "y"),
    suffix = c(".target", ".sampled")
) %>% mutate(pp_diff = 100 * (prob.sampled - prob.target))
#> # A tibble: 6 × 7
#>   x     y     n.target prob.target n.sampled prob.sampled pp_diff
#>   <chr> <chr>    <int>       <dbl>     <int>        <dbl>   <dbl>
#> 1 a     a            2      0.0952       111        0.111   1.58 
#> 2 a     b            3      0.143        191        0.191   4.81 
#> 3 b     a            1      0.0476        58        0.058   1.04 
#> 4 b     b            6      0.286        272        0.272  -1.37 
#> 5 c     a            5      0.238        186        0.186  -5.21 
#> 6 c     b            4      0.190        182        0.182  -0.848

inner_join(
    pr2,
    tab[[2]],
    by = c("x", "z"),
    suffix = c(".target", ".sampled")
) %>% mutate(pp_diff = 100 * (prob.sampled - prob.target))
#> # A tibble: 12 × 7
#>    x     z     n.target prob.target n.sampled prob.sampled pp_diff
#>    <chr> <chr>    <int>       <dbl>     <int>        <dbl>   <dbl>
#>  1 a     a            7      0.0897        78        0.078 -1.17  
#>  2 a     b            6      0.0769        70        0.07  -0.692 
#>  3 a     c           11      0.141        107        0.107 -3.40  
#>  4 a     d            4      0.0513        47        0.047 -0.428 
#>  5 b     a           12      0.154        137        0.137 -1.68  
#>  6 b     b           10      0.128        131        0.131  0.279 
#>  7 b     c            2      0.0256        23        0.023 -0.264 
#>  8 b     d            3      0.0385        39        0.039  0.0538
#>  9 c     a            9      0.115        118        0.118  0.262 
#> 10 c     b            8      0.103        164        0.164  6.14  
#> 11 c     c            1      0.0128         9        0.009 -0.382 
#> 12 c     d            5      0.0641        77        0.077  1.29

inner_join(
    pr3,
    tab[[3]],
    by = c("y", "z"),
    suffix = c(".target", ".sampled")
) %>% mutate(pp_diff = 100 * (prob.sampled - prob.target))
#> # A tibble: 8 × 7
#>   y     z     n.target prob.target n.sampled prob.sampled pp_diff
#>   <chr> <chr>    <int>       <dbl>     <int>        <dbl>   <dbl>
#> 1 a     a            3      0.0833       104        0.104  2.07  
#> 2 a     b            8      0.222        189        0.189 -3.32  
#> 3 a     c            2      0.0556        34        0.034 -2.16  
#> 4 a     d            1      0.0278        28        0.028  0.0222
#> 5 b     a            6      0.167        229        0.229  6.23  
#> 6 b     b            7      0.194        176        0.176 -1.84  
#> 7 b     c            4      0.111        105        0.105 -0.611 
#> 8 b     d            5      0.139        135        0.135 -0.389
```

Differences between target and sampled pairwise inclusion probabilities
are all within 5pp or so.

## Starting from some example data

``` r
library(purrr)

df <- expand_grid(
    x = letters[1:3],
    y = letters[1:2],
    z = letters[1:4]
) %>%
mutate(wt = runif(n(), 0.05, 1)) %>%
slice_sample(n = 1000, replace = TRUE, weight_by = wt) %>%
select(-wt)

tab_target <- calculate_two_way_contingencies(df, x, y, z)

sdf <- sample_correlated_discrete(size = 1000, !!!tab_target)

tab_sample <- calculate_two_way_contingencies(sdf, x, y, z)

for (idx in 1:3) {
    diff <- inner_join(
        tab_target[[idx]],
        tab_sample[[idx]],
        by = names(tab_target[[idx]])[1:2],
        suffix = c(".target", ".sampled")
    ) %>% mutate(pp_diff = 100 * (prob.sampled - prob.target))

    print(diff)
}
#> # A tibble: 6 × 7
#>   x     y     n.target prob.target n.sampled prob.sampled pp_diff
#>   <chr> <chr>    <int>       <dbl>     <int>        <dbl>   <dbl>
#> 1 a     a          139       0.139       141        0.141   0.200
#> 2 a     b          198       0.198       211        0.211   1.30 
#> 3 b     a          172       0.172       191        0.191   1.90 
#> 4 b     b          160       0.16        151        0.151  -0.900
#> 5 c     a          140       0.14        135        0.135  -0.500
#> 6 c     b          191       0.191       171        0.171  -2    
#> # A tibble: 12 × 7
#>    x     z     n.target prob.target n.sampled prob.sampled pp_diff
#>    <chr> <chr>    <int>       <dbl>     <int>        <dbl>   <dbl>
#>  1 a     a           74       0.074        83        0.083   0.900
#>  2 a     b          111       0.111       114        0.114   0.300
#>  3 a     c           92       0.092        83        0.083  -0.900
#>  4 a     d           60       0.06         72        0.072   1.2  
#>  5 b     a           95       0.095        89        0.089  -0.600
#>  6 b     b          117       0.117       120        0.12    0.300
#>  7 b     c           72       0.072        78        0.078   0.600
#>  8 b     d           48       0.048        55        0.055   0.7  
#>  9 c     a           97       0.097        80        0.08   -1.7  
#> 10 c     b           30       0.03         31        0.031   0.100
#> 11 c     c          109       0.109       100        0.1    -0.900
#> 12 c     d           95       0.095        95        0.095   0    
#> # A tibble: 8 × 7
#>   y     z     n.target prob.target n.sampled prob.sampled pp_diff
#>   <chr> <chr>    <int>       <dbl>     <int>        <dbl>   <dbl>
#> 1 a     a          115       0.115       111        0.111  -0.400
#> 2 a     b          134       0.134       144        0.144   1.00 
#> 3 a     c          120       0.12        114        0.114  -0.600
#> 4 a     d           82       0.082        98        0.098   1.6  
#> 5 b     a          151       0.151       141        0.141  -1.00 
#> 6 b     b          124       0.124       121        0.121  -0.300
#> 7 b     c          153       0.153       147        0.147  -0.600
#> 8 b     d          121       0.121       124        0.124   0.300
```

Differences between the target and sampled pairwise inclusion
probabilities are all under a couple percentage points.
