#' @importFrom stats complete.cases rnorm sd terms t.test
#' @importFrom utils head
NULL

parse_ce_formula <- function(formula, data, ref = NULL, require_group = TRUE,
                             na.omit = TRUE) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula.", call. = FALSE)
  }

  vars <- all.vars(stats::terms(formula))
  min_vars <- if (require_group) 3L else 2L
  if (length(vars) < min_vars) {
    stop("Formula must be of the form `cost + effect ~ group`.", call. = FALSE)
  }

  cost <- vars[1]
  effect <- vars[2]
  group <- if (length(vars) >= 3L) vars[3] else NULL
  needed <- c(cost, effect, group)
  missing_vars <- setdiff(needed, names(data))
  if (length(missing_vars) > 0L) {
    stop("Variables not found in `data`: ", paste(missing_vars, collapse = ", "),
         call. = FALSE)
  }

  out <- data[, needed, drop = FALSE]
  names(out) <- c("cost", "effect", if (!is.null(group)) "group")
  if (na.omit) out <- out[stats::complete.cases(out), , drop = FALSE]

  if (!is.numeric(out$cost) || !is.numeric(out$effect)) {
    stop("Cost and effect variables must be numeric.", call. = FALSE)
  }

  if (require_group) {
    out$group <- as.factor(out$group)
    if (is.null(ref)) stop("`ref` must be supplied.", call. = FALSE)
    if (!ref %in% levels(out$group)) {
      stop("`ref` must be one of the observed group levels.", call. = FALSE)
    }
    trt_levels <- setdiff(levels(out$group), ref)
    if (length(trt_levels) != 1L) {
      stop("CEACT currently supports exactly two groups for incremental analysis.",
           call. = FALSE)
    }
    if (any(table(out$group) < 2L)) {
      stop("Each group must contain at least two observations.", call. = FALSE)
    }
  }

  attr(out, "cost_name") <- cost
  attr(out, "effect_name") <- effect
  attr(out, "group_name") <- group
  out
}

ce_deltas <- function(data, ref) {
  trt <- data[data$group != ref, , drop = FALSE]
  ctrl <- data[data$group == ref, , drop = FALSE]
  delta_cost <- mean(trt$cost) - mean(ctrl$cost)
  delta_effect <- mean(trt$effect) - mean(ctrl$effect)
  c(delta_cost = delta_cost, delta_effect = delta_effect,
    ICER = delta_cost / delta_effect)
}

extract_boot_ci <- function(bt, index, type = "bca", conf = 0.95) {
  out <- suppressWarnings(tryCatch(
    boot::boot.ci(bt, type = type, index = index, conf = conf),
    error = function(e) NULL
  ))
  if (is.null(out)) return(c(NA_real_, NA_real_))

  if (!is.null(out$bca)) return(out$bca[4:5])
  if (!is.null(out$percent)) return(out$percent[4:5])
  if (!is.null(out$basic)) return(out$basic[4:5])
  if (!is.null(out$normal)) return(out$normal[2:3])
  c(NA_real_, NA_real_)
}

format_interval <- function(x, digits = 3) {
  if (anyNA(x)) return(NA_character_)
  paste0("[", round(x[1], digits), "; ", round(x[2], digits), "]")
}

round_df <- function(x, digits = 3) {
  numeric_columns <- sapply(x, is.numeric)
  x[numeric_columns] <- round(x[numeric_columns], digits)
  x
}

format_pval <- function(p, digits = 4) {
  ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = digits))
}

#' Simulate a Two-Arm Trial for Cost-Effectiveness Analysis
#'
#' Generates individual-level cost and effect outcomes for a simple two-arm
#' randomized clinical trial. The function is intended for examples, tutorials,
#' tests, and manuscript demonstrations.
#'
#' @param n Number of participants per arm.
#' @param mean_cost Control and treatment mean costs.
#' @param sd_cost Control and treatment cost standard deviations.
#' @param mean_effect Control and treatment mean effects, for example QALYs.
#' @param sd_effect Control and treatment effect standard deviations.
#' @param rho Within-person cost-effect correlation used in a Gaussian
#'   construction.
#' @param seed Optional random seed.
#' @param group_names Character vector naming control and treatment arms.
#'
#' @return A data frame with `cost`, `effect`, and `group`.
#' @export
#'
#' @examples
#' trial <- simulatecetrial(n = 50, seed = 1)
#' head(trial)
simulatecetrial <- function(n = 200,
                              mean_cost = c(5000, 5600),
                              sd_cost = c(900, 1000),
                              mean_effect = c(0.72, 0.78),
                              sd_effect = c(0.10, 0.11),
                              rho = 0.15,
                              seed = NULL,
                              group_names = c("control", "treatment")) {
  if (!is.null(seed)) set.seed(seed)
  if (length(group_names) != 2L) stop("`group_names` must have length 2.")
  if (abs(rho) >= 1) stop("`rho` must be between -1 and 1.")

  arm_data <- lapply(seq_along(group_names), function(i) {
    z1 <- stats::rnorm(n)
    z2 <- rho * z1 + sqrt(1 - rho^2) * stats::rnorm(n)
    data.frame(
      cost = mean_cost[i] + sd_cost[i] * z1,
      effect = mean_effect[i] + sd_effect[i] * z2,
      group = group_names[i]
    )
  })

  out <- .as_bt(basetable::rbindfill(arm_data))
  out$cost <- pmax(out$cost, 0)
  out$effect <- pmax(out$effect, 0)
  out$group <- factor(out$group, levels = group_names)
  out
}

# Result tables are basetable objects.
.as_bt <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  rownames(x) <- NULL
  basetable::asbasetable(x)
}

# Share of bootstrap replicates in each quadrant of the cost-effectiveness
# plane. A zero difference counts as "not positive".
ce_quadrants <- function(dc, de) {
  q <- ifelse(de > 0,
              ifelse(dc > 0, "NE", "SE"),
              ifelse(dc > 0, "NW", "SW"))
  q <- factor(q, levels = c("NE", "SE", "NW", "SW"))
  share <- as.numeric(table(q)) / length(q)
  .as_bt(data.frame(
    Quadrant = c("NE", "SE", "NW", "SW"),
    Description = c("more costly, more effective",
                    "less costly, more effective, dominant",
                    "more costly, less effective, dominated",
                    "less costly, less effective"),
    Share = share,
    stringsAsFactors = FALSE
  ))
}

# ICER row of a bootstrap summary. The point estimate is the observed ratio of
# mean differences. A percentile interval of the ratio is only meaningful when
# all replicates fall in the NE or in the SW quadrant; otherwise it is NA and a
# note points to the CEAC / INMB.
icer_interval <- function(dc, de, quadrants, conf = 0.95) {
  share <- stats::setNames(quadrants$Share, quadrants$Quadrant)
  one <- names(share)[share == 1]
  if (length(one) == 1L && one %in% c("NE", "SW")) {
    probs <- c((1 - conf) / 2, 1 - (1 - conf) / 2)
    return(list(ci = stats::quantile(dc / de, probs, names = FALSE),
                note = NULL))
  }
  note <- if (length(one) == 1L) {
    paste0("All replicates lie in the ", one, " quadrant (",
           if (one == "SE") "treatment dominant" else "treatment dominated",
           "); the ICER interval is not reported.")
  } else {
    paste0("Bootstrap replicates span several quadrants of the ",
           "cost-effectiveness plane; an ICER interval is not meaningful. ",
           "Use the CEAC or INMB from computenmbceac() instead.")
  }
  list(ci = c(NA_real_, NA_real_), note = note)
}

# Summary table shared by booticer() and miboot().
boot_summary_table <- function(observed, boot_dist, ci_dc, ci_de, ci_icer) {
  bm <- colMeans(boot_dist[, 1:2, drop = FALSE])
  data.frame(
    Metric = c("Delta Cost", "Delta Effect", "ICER"),
    Observed = round(unname(observed), 3),
    BootstrapMean = round(c(bm, NA_real_), 3),
    StdError = round(c(apply(boot_dist[, 1:2, drop = FALSE], 2, stats::sd),
                       NA_real_), 3),
    Bias = round(c(bm - observed[1:2], NA_real_), 3),
    CI = c(format_interval(ci_dc), format_interval(ci_de),
           format_interval(ci_icer)),
    stringsAsFactors = FALSE
  )
}
