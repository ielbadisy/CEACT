dsa_icer <- function(base_data, param, range, formula, ref, wtp = NULL) {
  terms_obj <- terms(formula)
  vars <- all.vars(terms_obj)
  cost   <- vars[1]
  effect <- vars[2]
  group  <- vars[3]

  results <- lapply(range, function(val) {
    new_data <- base_data
    new_data[new_data[[group]] != ref, param] <- val

    c_mean <- tapply(new_data[[cost]], new_data[[group]], mean)
    e_mean <- tapply(new_data[[effect]], new_data[[group]], mean)

    delta_c <- c_mean[names(c_mean) != ref] - c_mean[ref]
    delta_e <- e_mean[names(e_mean) != ref] - e_mean[ref]
    icer <- delta_c / delta_e
    nmb <- if (!is.null(wtp)) wtp * delta_e - delta_c else NA

    data.frame(Value = val, ICER = icer, NMB = nmb)
  })

  do.call(rbind, results)
}
