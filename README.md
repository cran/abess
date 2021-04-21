# abess: An R package for Best-Subset Selection in Polynomial Time

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/abess-team/abess.svg?branch=master)](https://travis-ci.com/abess-team/abess)
[![codecov](https://codecov.io/gh/abess-team/abess/branch/master/graph/badge.svg?token=LK56LHXV00)](https://codecov.io/gh/abess-team/abess)
<!-- badges: end -->

Best-subset selection aims to find a small subset of predictors such that the resulting linear model is expected to have the most desirable prediction accuracy. This project implements a polynomial algorithm proposed by Zhu et al (2020) to solve the problem. It supports:
<!-- Moreover, the softwares includes helpful features for high-dimensional data analysis. -->

- various model:
  - linear regression
  - classification (binary or multi-class)
  - counting-response modeling
  - censored-response modeling
  - multi-response modeling (multi-tasks learning)
- sure independence screening
- nuisance penalized regression

## Installation

You can install the newest version of abess from [github](https://github.com/) with:

``` r
remotes::install_github("abess-team/abess")
```

## Reference
A polynomial algorithm for best-subset selection problem. Junxian Zhu, Canhong Wen, Jin Zhu, Heping Zhang, Xueqin Wang. Proceedings of the National Academy of Sciences Dec 2020, 117 (52) 33117-33123; DOI: 10.1073/pnas.2014241117    
Fan, J. and Lv, J. (2008), Sure independence screening for ultrahigh dimensional feature space. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 70: 849-911. https://doi.org/10.1111/j.1467-9868.2008.00674.x    
Qiang Sun & Heping Zhang (2020) Targeted Inference Involving High-Dimensional Data Using Nuisance Penalized Regression, Journal of the American Statistical Association, DOI: 10.1080/01621459.2020.1737079
