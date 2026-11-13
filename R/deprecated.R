#' Deprecated functions in CEACT
#'
#' These functions were renamed to glued lowercase names and will be removed
#' in a future release. Use the new names instead.
#'
#' @param ... Arguments passed to the new function.
#' @return The result of the new function.
#' @name CEACT-deprecated
#' @keywords internal
NULL

#' @rdname CEACT-deprecated
#' @export
boot_icer <- function(...) {
  .Deprecated("booticer")
  booticer(...)
}

#' @rdname CEACT-deprecated
#' @export
compute_nmb_ceac <- function(...) {
  .Deprecated("computenmbceac")
  computenmbceac(...)
}

#' @rdname CEACT-deprecated
#' @export
dsa_icer <- function(...) {
  .Deprecated("dsaicer")
  dsaicer(...)
}

#' @rdname CEACT-deprecated
#' @export
plot_ceac <- function(...) {
  .Deprecated("plotceac")
  plotceac(...)
}

#' @rdname CEACT-deprecated
#' @export
plot_ceplane <- function(...) {
  .Deprecated("plotceplane")
  plotceplane(...)
}

#' @rdname CEACT-deprecated
#' @export
plot_dsa <- function(...) {
  .Deprecated("plotdsa")
  plotdsa(...)
}

#' @rdname CEACT-deprecated
#' @export
simulate_ce_trial <- function(...) {
  .Deprecated("simulatecetrial")
  simulatecetrial(...)
}
