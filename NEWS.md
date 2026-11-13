# CEACT 0.6.2

* `booticer()` and `miboot()`: the ICER row now reports the observed ratio of mean differences, with no bootstrap mean, standard error or bias (a mean of ratios is unstable near zero incremental effect).
* `booticer()` and `miboot()`: the ICER interval is the percentile interval of the bootstrap ratios only when all replicates lie in the NE or in the SW quadrant; otherwise it is `NA` and a `note` points to the CEAC/INMB. The share of replicates per quadrant is returned in `quadrants` and shown by the new `print()` method.
* `miboot()`: the `Observed` column is now the Rubin point estimate from the original data completed by `imputefun`, not the bootstrap mean, so `Bias` is meaningful.
* README checked for current (glued lowercase) function names; a test guards it.

# CEACT 0.6.1

Exported functions are renamed to glued lowercase (no underscores). The old names remain as deprecated wrappers for one release (see `?"CEACT-deprecated"`). The `boot_icer` and `nmb_ceac` class names are unchanged. `plot_tornado()` was already a deprecated alias and now points to `plotdsa()`. Requires `basetable (>= 1.4.5)`.

| Old | New |
|---|---|
| `boot_icer()` | `booticer()` |
| `compute_nmb_ceac()` | `computenmbceac()` |
| `dsa_icer()` | `dsaicer()` |
| `plot_ceac()` | `plotceac()` |
| `plot_ceplane()` | `plotceplane()` |
| `plot_dsa()` | `plotdsa()` |
| `simulate_ce_trial()` | `simulatecetrial()` |

