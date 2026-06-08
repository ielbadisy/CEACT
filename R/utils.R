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
#' trial <- simulate_ce_trial(n = 50, seed = 1)
#' head(trial)
simulate_ce_trial <- function(n = 200,
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

  out <- do.call(rbind, arm_data)
  out$cost <- pmax(out$cost, 0)
  out$effect <- pmax(out$effect, 0)
  out$group <- factor(out$group, levels = group_names)
  out
}
