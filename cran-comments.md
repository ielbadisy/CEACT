## Update submission (0.5.0 -> 0.6.2)

This update adds `micea()`, cost-effectiveness analysis on multiply imputed
data pooled with Rubin's rules, and `miboot()`, bootstrap-then-impute
resampling. Both accept an optional case-weight column. Result tables are
now 'basetable' objects (requires basetable >= 1.4.5).

Exported functions are renamed to glued lowercase (for example
`boot_icer()` becomes `booticer()`). The old names remain as deprecated
wrappers for one release.

`booticer()` and `miboot()` now report the observed ICER, with a percentile
interval only when all bootstrap replicates fall in one quadrant. See NEWS.md.

## Test environments

* Local: Ubuntu 24.04, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 1 note

* New maintainer: only the casing of the maintainer name changes, from
  "Imad EL BADISY" to "Imad El Badisy". The person and the email address are
  the same.

## Reverse dependencies

There are no reverse dependencies on CRAN.
