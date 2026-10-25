# CEACT 0.6.0

* New `micea()`: cost-effectiveness analysis on multiply imputed data
  (`mimar_imputation`, `mids`, or a list of completed data frames), pooled with
  Rubin's rules and Barnard-Rubin degrees of freedom, including INMB and a
  CEAC table at each willingness-to-pay threshold.
* New `miboot()`: bootstrap-then-impute resampling with a user-supplied
  imputation function; returns a `boot_icer`-compatible object for
  `compute_nmb_ceac()`, `plot_ceplane()`, and `plot_ceac()`.
* Both functions accept an optional case-weight column (for example matching
  or inverse probability weights computed within each completed data set).
