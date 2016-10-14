
<!-- README.md is generated from README.Rmd. Please edit that file -->
Sabotash: Sabotaging ASH
========================

[![Build Status](https://travis-ci.org/dcgerard/sabotash.svg?branch=master)](https://travis-ci.org/dcgerard/sabotash) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dcgerard/sabotash?branch=master&svg=true)](https://ci.appveyor.com/project/dcgerard/sabotash) [![Coverage Status](https://img.shields.io/codecov/c/github/dcgerard/sabotash/master.svg)](https://codecov.io/github/dcgerard/sabotash?branch=master)

Description
===========

[Adaptive SHrinkage](https://github.com/stephens999/ashr) is a powerful new empirical Bayes approach for multiple testing and estimation. There are many extensions: [MOUTHWASH](https://github.com/dcgerard/vicar), [STRAMASH](https://github.com/dcgerard/stramash), [MnMASH](https://github.com/gaow/mnmashr), [FLASH](https://github.com/NKweiwang/flash), [FLASHtpx](https://github.com/kkdey/flashtpx), [ASHNET](https://github.com/kkdey/ashnet), [BiASH](https://github.com/LSun/Bi-ASH), [VASH](https://github.com/mengyin/vashr), [MIXASH](https://github.com/mengyin/mixash), [FASH](https://github.com/mengyin/ashlar-fash), [MASH](https://github.com/surbut/matrix_ash), [SMASH](https://github.com/stephenslab/smashr), and [MVASH](https://github.com/stephenslab/mvash), among others.

We introduce the `sabotash` package which, when loaded, will change the namespace of `ashr` so that the functions `ashr::ash` and `ashr::ash.workhorse` will return random, though convincing, numbers. This should ruin, in particular, all of the above methods.

Please report any (unintentional) bugs by creating an [issue](http://github.com/dcgerard/sabotash/issues).

Installation
============

To install, simply run in R

``` r
install.packages("devtools")
devtools::install_github("dcgerard/sabotash")
```
