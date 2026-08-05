#' Attach values observed in a fixed base period
#'
#' @description
#' Adds columns containing the value of `cols` as observed in the single period
#' `at`, matched by an equi-join on `by`. Every observation in a series points
#' at the same base observation.
#'
#' @details
#' # Relationship to [join_lag()]
#'
#' `join_lag()` models a recursive **one-to-one** relationship: each period has
#' at most one predecessor, and unmatched rows at the series edges are normal.
#' `join_at()` models a recursive **many-to-one** relationship: many periods
#' reference one base period, and an unmatched row is *not* normal — it means
#' the series has no observation at all in the base period, i.e. the product
#' did not exist then. Hence `on_missing` defaults to warning, and reports at
#' the level of the series rather than the row.
#'
#' # Where this sits in the OIIA
#'
#' The two joins produce the two different valuations the accounts need, from
#' the same quantities:
#'
#' \describe{
#'   \item{`join_at()` — constant prices, \eqn{q_n p_b}}{The fixed-base series
#'     published nationally in Output, Input and Income in Agriculture.}
#'   \item{`join_lag(k = 1)` — prices of the previous year, \eqn{q_n p_{n-1}}}{
#'     Transmitted to Eurostat (COSAEA_AGR3CON), who chain-link and re-reference
#'     at their end. We do not chain-link.}
#' }
#'
#' A practical consequence: a fixed-base series **is** additive, unlike a
#' chain-linked one. Aggregates at constant prices must equal the sum of their
#' components, so that identity is available as a validation check here — see
#' `check_additive()` in the examples.
#'
#' Rebasing is therefore just a new `at` and a re-run of the whole series, not
#' a re-chaining exercise.
#'
#' # Products absent from the base year
#'
#' With a fixed base, any product not produced in year `at` has no base price,
#' and its quantities cannot be valued — sugar beet (ceased 2005) and turf
#' (ceased 2013) against any later base year, or a product that only starts
#' after `at`. These are structural facts about the Irish series, not data
#' errors, so `on_missing` defaults to `"warn"` rather than `"error"`: the
#' handling (an imputed or spliced base price, or acceptance of the break)
#' is a methodological decision, not something this function should make.
#' Set `on_missing = "error"` for series where you expect full coverage.
#'
#' @param data A data frame. `(by, time)` must uniquely identify rows.
#' @param cols Character vector of columns to carry across from the base period.
#' @param by Character vector of series key columns. `character(0)` for a
#'   single ungrouped series.
#' @param time Length-1 character naming the discrete period column.
#' @param at Length-1 whole number: the base period.
#' @param suffix Suffix for the new columns. Defaults to `"_base<at>"`.
#' @param on_missing What to do for series with no observation in the base
#'   period: `"warn"` (default), `"error"`, or `"ignore"`.
#'
#' @return `data` plus one column per `cols`, in the original row order.
#'   Rows in the base period itself match themselves, so
#'   `price_base2020[year == 2020] == price[year == 2020]` — a cheap invariant
#'   worth asserting after the call.
#'
#' @section Errors:
#' `join_at_bad_input`, `join_at_duplicate_keys`, `join_at_missing_base`.
#'
#' @examples
#' df <- data.frame(
#'   item = c("cattle", "cattle", "milk", "milk"),
#'   year_concerned = c(2020L, 2021L, 2020L, 2021L),
#'   price = c(100, 110, 40, 45)
#' )
#' join_at(df, cols = "price", by = "item", at = 2020)
#'
#' # Fixed-base series are additive, so this identity should hold exactly
#' # (up to rounding) for every year — it does not hold for chain-linked data.
#' \dontrun{
#' check_additive <- function(x, total_col, component_cols, tol = 1e-6) {
#'   diff <- x[[total_col]] - rowSums(x[component_cols])
#'   if (any(abs(diff) > tol, na.rm = TRUE)) {
#'     rlang::abort("Constant-price aggregate does not equal its components.",
#'                  class = "eaa_non_additive")
#'   }
#'   invisible(x)
#' }
#' }
#'
#' @importFrom dplyr left_join
#' @importFrom rlang abort warn
#' @export
join_at <- function(data,
                    cols,
                    by,
                    time = "year_concerned",
                    at,
                    suffix = NULL,
                    on_missing = c("warn", "error", "ignore")) {

  on_missing <- match.arg(on_missing)

  # ---- validate -----------------------------------------------------------

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.", class = "join_at_bad_input")
  }
  if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
    rlang::abort("`cols` must be a non-empty character vector.",
                 class = "join_at_bad_input")
  }
  if (!is.character(by) || anyNA(by)) {
    rlang::abort("`by` must be a character vector.", class = "join_at_bad_input")
  }
  if (!is.character(time) || length(time) != 1L) {
    rlang::abort("`time` must be a single column name.",
                 class = "join_at_bad_input")
  }
  if (!is.numeric(at) || length(at) != 1L || is.na(at) || at != trunc(at)) {
    rlang::abort("`at` must be a single whole number.",
                 class = "join_at_bad_input")
  }

  missing_cols <- setdiff(c(cols, by, time), names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(paste0("Column(s) not found in `data`: ",
                        paste(missing_cols, collapse = ", "), "."),
                 class = "join_at_bad_input")
  }
  if (length(intersect(cols, c(by, time))) > 0L) {
    rlang::abort("`cols` must not include key columns.",
                 class = "join_at_bad_input")
  }

  key_cols <- c(by, time)
  dups <- duplicated(data[key_cols]) | duplicated(data[key_cols], fromLast = TRUE)
  if (any(dups)) {
    rlang::abort(
      paste0("`(", paste(key_cols, collapse = ", "), ")` does not uniquely ",
             "identify rows: ", sum(dups), " row(s) affected."),
      class = "join_at_duplicate_keys"
    )
  }

  if (is.null(suffix)) suffix <- paste0("_base", as.integer(at))
  new_cols <- paste0(cols, suffix)
  clash <- intersect(new_cols, names(data))
  if (length(clash) > 0L) {
    rlang::abort(paste0("New column(s) would overwrite: ",
                        paste(clash, collapse = ", "), "."),
                 class = "join_at_bad_input")
  }

  # ---- the join -----------------------------------------------------------

  in_base <- !is.na(data[[time]]) & data[[time]] == at
  if (!any(in_base)) {
    rlang::abort(
      paste0("No rows at all in the base period ", time, " == ", at, "."),
      class = "join_at_missing_base"
    )
  }

  base <- data[in_base, c(by, cols), drop = FALSE]
  names(base)[match(cols, names(base))] <- new_cols

  if (length(by) == 0L) {
    # One base row; nothing to match on, so recycle it directly rather than
    # letting dplyr perform a cross join.
    out <- data
    for (j in seq_along(new_cols)) out[[new_cols[j]]] <- base[[new_cols[j]]][1L]
  } else {
    out <- dplyr::left_join(
      data, base,
      by = stats::setNames(by, by),
      relationship = "many-to-one",
      na_matches = "never"
    )
  }

  # ---- report series absent from the base period --------------------------

  if (on_missing != "ignore") {
    absent <- is.na(out[[new_cols[1L]]]) & !is.na(data[[cols[1L]]])
    if (any(absent)) {
      series <- if (length(by) == 0L) NULL else unique(out[absent, by, drop = FALSE])
      msg <- paste0(
        sum(absent), " row(s) in ",
        if (is.null(series)) "the series" else paste0(nrow(series), " series"),
        " have no observation in the base period ", at,
        " (product absent from the base basket).",
        if (!is.null(series)) paste0(
          "\nFirst affected:\n",
          paste(utils::capture.output(
            print(utils::head(series, 5L), row.names = FALSE)), collapse = "\n")
        ) else ""
      )
      if (on_missing == "error") {
        rlang::abort(msg, class = "join_at_missing_base")
      } else {
        rlang::warn(msg, class = "join_at_missing_base")
      }
    }
  }

  out
}
