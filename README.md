CEACT Package
================

# CEACT: Cost-Effectiveness Analysis for Clinical Trials

## Overview

**CEACT** *(Cost-Effectiveness Analysis for Clinical Trials)* is a tool
designed to produce a cost-effectiveness analysis of two interventions,
specifically comparing treatment and control groups in clinical trials.
This package aims to assist researchers and analysts in making informed
decisions based on the cost-effectiveness of different health
interventions.

**CEACT** provides a complete suite of functions for computing and
visualizing: - Incremental Cost-Effectiveness Ratio (ICER), -
Cost-effectiveness planes, - Cost-effectiveness acceptability curves
(CEAC), - Net monetary benefit (NMB) metrics.

------------------------------------------------------------------------

## Installation

``` r
# install.packages("devtools")
devtools::install_github("ielbadisy/CEACT")
```

    ## Using GitHub PAT from the git credential store.

    ## Downloading GitHub repo ielbadisy/CEACT@HEAD

    ## pillar  (1.10.1 -> 1.10.2) [CRAN]
    ## stringi (1.8.4  -> 1.8.7 ) [CRAN]

    ## Installing 2 packages: pillar, stringi

    ## Installing packages into '/home/imad-el-badisy/R/x86_64-pc-linux-gnu-library/4.4'
    ## (as 'lib' is unspecified)

    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ##      checking for file ‘/tmp/Rtmp9Zy54p/remotes8b2b5efd8151/ielbadisy-CEACT-3bdbbd5/DESCRIPTION’ ...  ✔  checking for file ‘/tmp/Rtmp9Zy54p/remotes8b2b5efd8151/ielbadisy-CEACT-3bdbbd5/DESCRIPTION’
    ##   ─  preparing ‘CEACT’:
    ##      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
    ##   ─  checking for LF line-endings in source and make files and shell scripts
    ## ─  checking for empty or unneeded directories
    ##   ─  creating default NAMESPACE file
    ## ─  building ‘CEACT_0.5.0.tar.gz’
    ##      
    ## 

    ## Installing package into '/home/imad-el-badisy/R/x86_64-pc-linux-gnu-library/4.4'
    ## (as 'lib' is unspecified)

------------------------------------------------------------------------

## Key Features

- `cea()`: Estimate the ICER and provide summary statistics.
- `boot_icer()`: Perform non-parametric bootstrapping of ICERs.
- `plot_ceplane()`: Plot the cost-effectiveness plane.
- `plot_ceac()`: Plot the cost-effectiveness acceptability curve.
- `compute_nmb_ceac()`: Compute net monetary benefits (NMB) and CEAC
  tables.

------------------------------------------------------------------------

## Example Usage

``` r
# Load CEACT package
library(CEACT)
```

    ## Loading required package: ggplot2

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: tidyr

    ## Loading required package: boot

``` r
# Simulate example data from a clinical trial
set.seed(123)
control <- data.frame(
  cost = rnorm(200, 500, 100),
  effect = rnorm(200, 0.4, 0.05),
  group = "control"
)

treatment <- data.frame(
  cost = rnorm(200, 550, 100),
  effect = rnorm(200, 0.3, 0.06),
  group = "treatment"
)

data <- rbind(control, treatment)
```

``` r
# Run cost-effectiveness analysis
res_cea <- cea(cost + effect ~ group, data = data, ref = "control")
res_cea
```

    ##       Outcome           Control         Treatment  Delta            CI p.value
    ## 1   Mean Cost 499.14 (sd 94.32) 553.18 (sd 96.48) 54.035 [35.28;72.79]  <0.001
    ## 2 Mean Effect     0.4 (sd 0.05)     0.3 (sd 0.06) -0.103 [-0.11;-0.09]  <0.001

``` r
# Bootstrap ICER distribution
res_boot <- boot_icer(cost + effect ~ group, data = data, ref = "control", R = 300)
res_boot$summary
```

    ##         Metric Estimate Observed StdError   Bias                  CI
    ## 1   Delta Cost   52.553   54.035    8.983 -1.481     [37.634;73.698]
    ## 2 Delta Effect   -0.104   -0.103    0.006  0.000     [-0.114;-0.092]
    ## 3         ICER -508.609 -522.481   88.503 13.873 [-718.413;-366.257]

``` r
# Plot Cost-Effectiveness Plane
plot_ceplane(res_boot, k = 1000)
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Plot Cost-Effectiveness Acceptability Curve

plot_ceac(res_boot, wtp_range = seq(0, 2000, 100))

# Compute Net Monetary Benefit and CEAC Table

nmb_table \<- compute_nmb_ceac( data = data.frame(c =
res_boot$boot_dist[, 1], e = res_boot$boot_dist\[, 2\]), wtp_range =
seq(0, 2000, 100) ) print(nmb_table)


    ---

    ## Documentation

    You can access documentation for each function using:

    ```r
    ?cea
    ?boot_icer
    ?plot_ceplane
    ?plot_ceac
    ?compute_nmb_ceac

------------------------------------------------------------------------

## Feedback & contributions

We welcome feedback and contributions.  
Submit suggestions, feature requests, or report issues via [GitHub
Issues](https://github.com/ielbadisy/CEACT/issues).

------------------------------------------------------------------------

## TODO

\[ \] document functions \[ \] review the all package \[ \] add tests \[
\] build a pdf vignette \[ \] submit to CRAN
