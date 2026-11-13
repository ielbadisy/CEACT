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

