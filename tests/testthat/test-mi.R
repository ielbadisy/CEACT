make_incomplete <- function(seed = 1) {
  df <- simulate_ce_trial(n = 80, seed = seed)
  set.seed(seed)
  df$cost[sample(nrow(df), 40)] <- NA
  df$effect[sample(nrow(df), 30)] <- NA
  df$age <- stats::rnorm(nrow(df), 60, 8)
  df
}

hotdeck <- function(d) {
  for (v in c("cost", "effect")) {
    for (g in levels(d$group)) {
      miss <- d$group == g & is.na(d[[v]])
      pool <- d[[v]][d$group == g & !is.na(d[[v]])]
      d[[v]][miss] <- pool[sample.int(length(pool), sum(miss), replace = TRUE)]
    }
  }
  d
}

test_that("micea reduces to the complete-data analysis when imputations agree", {
  df <- simulate_ce_trial(n = 60, seed = 2)
  res <- micea(cost + effect ~ group, data = list(df, df, df), ref = "control",
               wtp_range = c(0, 20000))
  obs <- cea(cost + effect ~ group, data = df, ref = "control")
  welch <- t.test(df$cost[df$group == "treatment"],
                  df$cost[df$group == "control"])
  expect_equal(res$summary$Estimate[1], attr(obs, "delta_cost"))
  expect_equal(res$summary$Estimate[2], attr(obs, "delta_effect"))
  expect_equal(res$summary$StdError[1], unname(welch$stderr))
  expect_equal(res$summary$FMI[1:2], c(0, 0))
  expect_equal(res$ceac$ENMB[2], 20000 * attr(obs, "delta_effect") -
                 attr(obs, "delta_cost"))
})

test_that("micea matches mice::pool.scalar", {
  skip_if_not_installed("mice")
  df <- make_incomplete()
  set.seed(10)
  imps <- lapply(1:10, function(i) hotdeck(df))
  res <- micea(cost + effect ~ group, data = imps, ref = "control",
               wtp_range = 20000)
  for (j in 1:2) {
    Q <- res$estimates[, j]
    U <- vapply(imps, function(d) CEACT:::ce_fit(cost + effect ~ group, d,
                                                 "control")$U[j, j], numeric(1))
    ps <- mice::pool.scalar(Q, U, n = res$n, k = 2)
    expect_equal(res$summary$Estimate[j], ps$qbar)
    expect_equal(res$summary$StdError[j]^2, ps$t)
    expect_equal(res$summary$df[j], ps$df)
  }
  a <- c(-1, 20000)
  Qi <- drop(res$estimates %*% a)
  Ui <- vapply(imps, function(d) {
    U <- CEACT:::ce_fit(cost + effect ~ group, d, "control")$U
    drop(t(a) %*% U %*% a)
  }, numeric(1))
  ps <- mice::pool.scalar(Qi, Ui, n = res$n, k = 2)
  expect_equal(res$ceac$ENMB, ps$qbar)
  expect_equal(res$ceac$SE^2, ps$t)
  expect_equal(res$ceac$Prob_CE, pt(ps$qbar / sqrt(ps$t), ps$df))
})

test_that("micea accepts mids and weights", {
  skip_if_not_installed("mice")
  df <- make_incomplete()
  imp <- mice::mice(df, m = 5, method = "pmm", printFlag = FALSE, seed = 3)
  res <- micea(cost + effect ~ group, data = imp, ref = "control")
  expect_s3_class(res, "micea")
  expect_equal(res$m, 5)
  expect_s3_class(plot_ceac(res$ceac), "ggplot")

  df2 <- simulate_ce_trial(n = 50, seed = 4)
  df2$w <- 1
  unw <- micea(cost + effect ~ group, data = list(df2, df2), ref = "control")
  wtd <- micea(cost + effect ~ group, data = list(df2, df2), ref = "control",
               weights = "w")
  expect_equal(unw$summary, wtd$summary)
  dup <- rbind(df2, df2[df2$group == "treatment", ])
  df2$w <- ifelse(df2$group == "treatment", 2, 1)
  wd <- micea(cost + effect ~ group, data = list(df2, df2), ref = "control",
              weights = "w")
  dd <- micea(cost + effect ~ group, data = list(dup, dup), ref = "control")
  expect_equal(wd$summary$Estimate, dd$summary$Estimate)
})

test_that("micea input checks", {
  df <- simulate_ce_trial(n = 20, seed = 1)
  expect_error(micea(cost + effect ~ group, data = list(df), ref = "control"),
               "at least two")
  bad <- df
  bad$cost[1] <- NA
  expect_error(micea(cost + effect ~ group, data = list(bad, bad),
                     ref = "control"), "must not contain missing")
  expect_error(micea(cost + effect ~ group, data = 1:3, ref = "control"),
               "must be a")
})

test_that("miboot works with complete data and with imputation", {
  df <- simulate_ce_trial(n = 50, seed = 5)
  res <- miboot(cost + effect ~ group, data = df, ref = "control",
                imputefun = identity, R = 200, seed = 1)
  expect_s3_class(res, "boot_icer")
  expect_equal(res$failed, 0)
  obs <- cea(cost + effect ~ group, data = df, ref = "control")
  bse <- sd(res$boot_dist[, 1])
  expect_lt(abs(res$observed[1] - attr(obs, "delta_cost")), bse)

  inc <- make_incomplete()
  r1 <- miboot(cost + effect ~ group, data = inc, ref = "control",
               imputefun = hotdeck, R = 40, seed = 7)
  r2 <- miboot(cost + effect ~ group, data = inc, ref = "control",
               imputefun = hotdeck, R = 40, seed = 7)
  expect_equal(r1$boot_dist, r2$boot_dist)
  expect_equal(nrow(r1$boot_dist), 40)
  expect_s3_class(compute_nmb_ceac(r1, wtp_range = c(0, 20000)), "nmb_ceac")
  expect_s3_class(plot_ceplane(r1, k = 20000), "ggplot")

  multi <- function(d) lapply(1:3, function(i) hotdeck(d))
  r3 <- miboot(cost + effect ~ group, data = inc, ref = "control",
               imputefun = multi, R = 20, seed = 7)
  expect_equal(r3$m, 3)
})

test_that("miboot records failed replications", {
  df <- make_incomplete()
  flaky <- function(d) if (stats::runif(1) < 0.3) stop("boom") else hotdeck(d)
  res <- miboot(cost + effect ~ group, data = df, ref = "control",
                imputefun = flaky, R = 50, seed = 2)
  expect_gt(res$failed, 0)
  expect_equal(nrow(res$boot_dist) + res$failed, 50)
  expect_error(miboot(cost + effect ~ group, data = df, ref = "control",
                      imputefun = 1), "must be a function")
})

test_that("micea works with mimar output", {
  skip_if_not_installed("mimar")
  df <- make_incomplete()
  imp <- mimar::impute(df, m = 3, imputer = "pmm", seed = 1)
  res <- micea(cost + effect ~ group, data = imp, ref = "control")
  expect_equal(res$m, 3)
})
