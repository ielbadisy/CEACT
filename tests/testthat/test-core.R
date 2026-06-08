test_that("simulate_ce_trial creates valid two-arm data", {
  trial <- simulate_ce_trial(n = 20, seed = 1)

  expect_s3_class(trial, "data.frame")
  expect_equal(nrow(trial), 40)
  expect_equal(names(trial), c("cost", "effect", "group"))
  expect_equal(levels(trial$group), c("control", "treatment"))
  expect_true(all(trial$cost >= 0))
  expect_true(all(trial$effect >= 0))
})

test_that("trial_cea contains cost, QALY, and group fields", {
  data("trial_cea", package = "CEACT")

  expect_s3_class(trial_cea, "data.frame")
  expect_equal(nrow(trial_cea), 500)
  expect_true(all(c("cost", "qaly", "group") %in% names(trial_cea)))
  expect_equal(levels(trial_cea$group), c("control", "treatment"))
})

test_that("cea computes observed incremental quantities", {
  trial <- data.frame(
    cost = c(100, 120, 200, 220),
    effect = c(1.0, 1.2, 1.5, 1.7),
    group = rep(c("control", "treatment"), each = 2)
  )

  res <- cea(cost + effect ~ group, data = trial, ref = "control")

  expect_s3_class(res, "cea")
  expect_equal(attr(res, "delta_cost"), 100)
  expect_equal(attr(res, "delta_effect"), 0.5)
  expect_equal(attr(res, "ICER"), 200)
})

test_that("boot_icer returns stratified bootstrap distribution", {
  set.seed(42)
  trial <- simulate_ce_trial(n = 40, seed = 2)
  res <- boot_icer(cost + effect ~ group, data = trial, ref = "control",
                   R = 50, ci.type = "perc")

  expect_s3_class(res, "boot_icer")
  expect_equal(dim(res$boot_dist), c(50, 3))
  expect_equal(names(res$observed), c("DeltaCost", "DeltaEffect", "ICER"))
  expect_equal(nrow(summary(res)), 3)
})

test_that("compute_nmb_ceac uses incremental net benefit", {
  boot_obj <- list(
    boot_dist = matrix(
      c(100, 0.01, 10000,
        100, 0.02, 5000,
        100, -0.01, -10000),
      ncol = 3,
      byrow = TRUE
    ),
    observed = c(DeltaCost = 100, DeltaEffect = 0.01, ICER = 10000),
    formula = cost + effect ~ group,
    ref = "control"
  )
  class(boot_obj) <- "boot_icer"

  tab <- compute_nmb_ceac(boot_obj, wtp_range = c(0, 10000, 20000))

  expect_s3_class(tab, "nmb_ceac")
  expect_equal(tab$ENMB, c(-100, 0, 100))
  expect_equal(tab$Prob_CE, c(0, 1 / 3, 2 / 3))
})

test_that("dsa_icer and plot functions return expected objects", {
  set.seed(42)
  trial <- simulate_ce_trial(n = 40, seed = 3)
  boot_res <- boot_icer(cost + effect ~ group, data = trial, ref = "control",
                        R = 30, ci.type = "perc")
  dsa <- dsa_icer(cost + effect ~ group, data = trial, param = "effect",
                  range = c(0.75, 0.80), ref = "control", metric = "INMB",
                  k = 20000)

  expect_equal(names(dsa), c("Parameter", "INMB"))
  expect_s3_class(plot_ceplane(boot_res, k = 20000), "ggplot")
  expect_s3_class(plot_ceac(boot_res, wtp_range = c(0, 10000)), "ggplot")
  expect_s3_class(plot_tornado(dsa, metric = "INMB"), "ggplot")
})
