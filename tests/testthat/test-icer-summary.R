hotdeck_cost <- function(d) {
  for (g in levels(d$group)) {
    i <- d$group == g
    miss <- i & is.na(d$cost)
    d$cost[miss] <- sample(d$cost[i & !is.na(d$cost)], sum(miss),
                           replace = TRUE)
  }
  d
}

test_that("booticer ICER row: observed ratio, no bootstrap mean or bias", {
  df <- simulatecetrial(n = 100, seed = 11)
  res <- booticer(cost + effect ~ group, data = df, ref = "control", R = 100)
  obs <- cea(cost + effect ~ group, data = df, ref = "control")
  icer <- res$summary[res$summary$Metric == "ICER", ]
  expect_equal(icer$Observed, round(attr(obs, "ICER"), 3))
  expect_true(is.na(icer$Bias))
  expect_true(is.na(icer$BootstrapMean))
  expect_true(is.na(icer$StdError))
  expect_false(any(is.na(res$summary$Bias[1:2])))
})

test_that("booticer gives no ICER interval when replicates span quadrants", {
  df <- simulatecetrial(n = 60, mean_effect = c(0.72, 0.725), seed = 1)
  res <- booticer(cost + effect ~ group, data = df, ref = "control", R = 200)
  expect_equal(sum(res$quadrants$Share), 1)
  expect_true(sum(res$quadrants$Share > 0) > 1)
  expect_true(is.na(res$summary$CI[3]))
  expect_match(res$note, "CEAC")
  expect_message(summary(res), "quadrants")
  expect_output(print(res), "NE")
})

test_that("booticer gives a percentile ICER interval in one quadrant", {
  df <- simulatecetrial(n = 200, mean_effect = c(0.6, 0.9),
                        mean_cost = c(5000, 8000), seed = 3)
  res <- booticer(cost + effect ~ group, data = df, ref = "control", R = 100)
  expect_equal(res$quadrants$Share[res$quadrants$Quadrant == "NE"], 1)
  expect_null(res$note)
  ci <- stats::quantile(res$boot_dist[, 3], c(0.025, 0.975), names = FALSE)
  expect_equal(res$summary$CI[3], CEACT:::format_interval(ci))
})

test_that("miboot Observed is the Rubin estimate on the original imputed data", {
  df <- simulatecetrial(n = 60, seed = 5)
  # Complete data with identity imputation: observed equals the plain estimate
  res <- miboot(cost + effect ~ group, data = df, ref = "control",
                imputefun = function(d) list(d, d), R = 30, seed = 1)
  obs <- cea(cost + effect ~ group, data = df, ref = "control")
  expect_equal(unname(res$observed[1:2]),
               c(attr(obs, "delta_cost"), attr(obs, "delta_effect")))
  expect_false(isTRUE(all.equal(unname(res$observed[1]),
                                mean(res$boot_dist[, 1]))))
  expect_equal(res$summary$Observed[3], round(attr(obs, "ICER"), 3))
  expect_true(is.na(res$summary$Bias[3]))

  inc <- df
  inc$cost[1:20] <- NA
  fill <- function(d, f) {
    for (g in levels(d$group)) {
      i <- d$group == g
      d$cost[i & is.na(d$cost)] <- f(d$cost[i], na.rm = TRUE)
    }
    d
  }
  fixed <- function(d) list(fill(d, mean), fill(d, median))
  r2 <- miboot(cost + effect ~ group, data = inc, ref = "control",
               imputefun = fixed, R = 30, seed = 1)
  pooled <- micea(cost + effect ~ group, data = fixed(inc), ref = "control")
  expect_equal(unname(r2$observed[1:2]), pooled$summary$Estimate[1:2])
})

test_that("miboot reports quadrant shares and drops the ICER interval", {
  df <- simulatecetrial(n = 60, mean_effect = c(0.72, 0.725), seed = 1)
  df$cost[1:20] <- NA
  res <- miboot(cost + effect ~ group, data = df, ref = "control",
                imputefun = hotdeck_cost, R = 100, seed = 1)
  expect_equal(sum(res$quadrants$Share), 1)
  expect_true(is.na(res$summary$CI[3]))
  expect_true(is.na(res$summary$BootstrapMean[3]))
  expect_match(res$note, "quadrants")
})

test_that("README uses only the current function names", {
  rmd <- testthat::test_path("..", "..", "README.Rmd")
  skip_if_not(file.exists(rmd))
  txt <- readLines(rmd)
  old <- c("boot_icer(", "compute_nmb_ceac(", "dsa_icer(", "plot_ceac(",
           "plot_ceplane(", "plot_dsa(", "plot_tornado(", "simulate_ce_trial(")
  for (o in old) expect_false(any(grepl(o, txt, fixed = TRUE)), info = o)
})
