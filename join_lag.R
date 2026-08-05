#' Attach values observed k periods earlier, by explicit key join
#'
#' @description
#' Adds columns containing the value of `cols` as observed `k` periods earlier,
#' matched by an equi-join on `(by, time - k)`.
#'
#' This is the relational alternative to `dplyr::lag()`. Where `lag()` asks
#' *"what is in the row above?"*, `join_lag()` asks *"what is the row whose
#' year is one less?"* — which is the question the accounts actually pose, and
#' which is true regardless of how the rows happen to be sorted.
#'
#' @details
#' # Why a join rather than a lag
#'
#' `lag()` is positional. It returns the correct answer only when three
#' conditions hold simultaneously: the frame is sorted by `time`, it is grouped
#' by `by`, and the series has no gaps. All three failures are silent and
#' produce plausible numbers. `join_lag()` depends on none of them:
#'
#' \describe{
#'   \item{Sorting}{Irrelevant. An equi-join is order-invariant by
#'     construction, and the output preserves the input row order.}
#'   \item{Grouping}{Explicit in `by`, so it cannot be forgotten.}
#'   \item{Gaps}{Return `NA`, which is the truth, and are reported by
#'     `on_gap` rather than being silently filled with the wrong period.}
#'   \item{Duplicates}{Always an error. If `(by, time)` is not unique then the
#'     relationship is not one-to-one and no lag of any kind is well defined.}
#' }
#'
#' # The data model
#'
#' In EER terms this is a *recursive* (unary) one-to-one relationship on the
#' observation entity, with roles "current" and "prior". Both sides are
#' **partial**: the first period of a series has no prior, the last has no
#' successor. The `NA`s at the series edges are that partiality, faithfully
#' represented — which is why this uses a left join and never an inner join.
#'
#' The foreign key is *derived* (`time - k`) rather than stored, so nothing
#' enforces referential integrity for you: `time - k` is always a syntactically
#' valid key whether or not a row bears it. The `on_gap` and duplicate checks
#' below are that missing constraint, relocated into the pipeline.
#'
#' @param data A data frame. `(by, time)` must uniquely identify rows.
#' @param cols Character vector of column names to carry back from the earlier
#'   period.
#' @param by Character vector of grouping key columns (e.g. product, region).
#'   Use `character(0)` for a single ungrouped series.
#' @param time Length-1 character naming the discrete time column. Must be
#'   whole-numbered (integer or double); `time - k` must be meaningful.
#' @param k Whole number offset. `k = 1` gives the previous period, `k = 2` two
#'   periods back. Negative `k` looks *forward* (a lead); `k = 0` is an error.
#' @param suffix Suffix appended to `cols` to name the new columns. Defaults to
#'   `"_lag1"`, `"_lag2"`, ... or `"_lead1"`, ... for negative `k`.
#' @param on_gap What to do when a row is unmatched even though the target
#'   period falls *inside* the observed range of its group — i.e. a genuine
#'   hole rather than the start or end of the series. One of `"warn"`
#'   (default), `"error"` (recommended for production runs), or `"ignore"`.
#'
#' @return `data` with one new column per entry in `cols`, in the original row
#'   order. Unmatched rows carry `NA`.
#'
#' @section Errors:
#' Conditions are classed for programmatic handling in a pipeline:
#' `join_lag_bad_input`, `join_lag_duplicate_keys`, `join_lag_gap`.
#'
#' @examples
#' df <- data.frame(
#'   item = c("cattle", "cattle", "cattle", "milk", "milk"),
#'   year_concerned = c(2021L, 2022L, 2024L, 2022L, 2023L),
#'   price = c(100, 110, 130, 40, 45)
#' )
#'
#' # 2022 gets 2021's price; 2024 gets NA and a gap warning (2023 is missing);
#' # milk 2022 gets NA silently, because it is the start of that series.
#' join_lag(df, cols = "price", by = "item")
#'
#' # Two periods back, several columns, hard failure on holes:
#' \dontrun{
#' join_lag(df, cols = c("price", "quantity"), by = c("item", "region"),
#'          k = 2, on_gap = "error")
#' }
#'
#' @seealso [dplyr::lag()], which remains the right tool when position itself
#'   is the meaning (running differences over a known-complete series).
#'
#' @importFrom dplyr left_join group_by summarise across all_of ungroup
#' @importFrom rlang abort warn .data
#' @export
join_lag <- function(data,
                     cols,
                     by,
                     time = "year_concerned",
                     k = 1L,
                     suffix = NULL,
                     on_gap = c("warn", "error", "ignore")) {

  on_gap <- match.arg(on_gap)
  key_tmp <- ".join_lag_key"
  hit_tmp <- ".join_lag_matched"

  # ---- 1. validate arguments ---------------------------------------------

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.", class = "join_lag_bad_input")
  }
  if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
    rlang::abort("`cols` must be a non-empty character vector.",
                 class = "join_lag_bad_input")
  }
  if (!is.character(by) || anyNA(by)) {
    rlang::abort("`by` must be a character vector (use character(0) for none).",
                 class = "join_lag_bad_input")
  }
  if (!is.character(time) || length(time) != 1L || is.na(time)) {
    rlang::abort("`time` must be a single column name.",
                 class = "join_lag_bad_input")
  }
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k != trunc(k)) {
    rlang::abort("`k` must be a single whole number.",
                 class = "join_lag_bad_input")
  }
  k <- as.integer(k)
  if (k == 0L) {
    rlang::abort("`k` must not be zero; a zero offset joins rows to themselves.",
                 class = "join_lag_bad_input")
  }

  missing_cols <- setdiff(c(cols, by, time), names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      paste0("Column(s) not found in `data`: ",
             paste(missing_cols, collapse = ", "), "."),
      class = "join_lag_bad_input"
    )
  }

  overlap <- intersect(cols, c(by, time))
  if (length(overlap) > 0L) {
    rlang::abort(
      paste0("`cols` must not include key columns: ",
             paste(overlap, collapse = ", "), "."),
      class = "join_lag_bad_input"
    )
  }

  if (any(c(key_tmp, hit_tmp) %in% names(data))) {
    rlang::abort(
      paste0("`data` uses reserved internal name(s): ",
             paste(intersect(c(key_tmp, hit_tmp), names(data)), collapse = ", "),
             ". Rename them before calling `join_lag()`."),
      class = "join_lag_bad_input"
    )
  }

  # The time domain must be discrete and evenly enumerated for `time - k` to
  # denote anything. Doubles are allowed, but only whole-valued ones.
  t_vals <- data[[time]]
  if (!is.numeric(t_vals)) {
    rlang::abort(paste0("`", time, "` must be numeric (a discrete period)."),
                 class = "join_lag_bad_input")
  }
  if (any(t_vals != trunc(t_vals), na.rm = TRUE)) {
    rlang::abort(paste0("`", time, "` must be whole-numbered."),
                 class = "join_lag_bad_input")
  }

  # ---- 2. enforce the one-to-one constraint ------------------------------

  key_cols <- c(by, time)
  dups <- duplicated(data[key_cols]) | duplicated(data[key_cols], fromLast = TRUE)
  if (any(dups)) {
    offending <- unique(data[dups, key_cols, drop = FALSE])
    shown <- utils::head(offending, 5L)
    rlang::abort(
      paste0(
        "`(", paste(key_cols, collapse = ", "), ")` does not uniquely identify ",
        "rows: ", nrow(offending), " duplicated key(s), ", sum(dups),
        " row(s) affected.\nFirst offending keys:\n",
        paste(utils::capture.output(print(shown, row.names = FALSE)),
              collapse = "\n")
      ),
      class = "join_lag_duplicate_keys"
    )
  }

  if (anyNA(t_vals)) {
    rlang::warn(
      paste0(sum(is.na(t_vals)), " row(s) have a missing `", time,
             "` and cannot be matched; they will receive NA."),
      class = "join_lag_na_time"
    )
  }

  # ---- 3. the join --------------------------------------------------------

  if (is.null(suffix)) {
    suffix <- if (k > 0L) paste0("_lag", k) else paste0("_lead", abs(k))
  }
  new_cols <- paste0(cols, suffix)
  clash <- intersect(new_cols, names(data))
  if (length(clash) > 0L) {
    rlang::abort(
      paste0("New column(s) would overwrite existing ones: ",
             paste(clash, collapse = ", "), ". Supply a different `suffix`."),
      class = "join_lag_bad_input"
    )
  }

  prior <- data[c(by, time, cols)]
  names(prior)[match(cols, names(prior))] <- new_cols
  prior[[hit_tmp]] <- TRUE

  x <- data
  x[[key_tmp]] <- t_vals - k

  join_spec <- c(stats::setNames(by, by), stats::setNames(time, key_tmp))

  out <- dplyr::left_join(
    x, prior,
    by = join_spec,
    relationship = "one-to-one",   # belt and braces over the check above
    na_matches   = "never"         # an NA period must NOT match another NA
  )

  # ---- 4. distinguish gaps from series edges ------------------------------

  unmatched <- is.na(out[[hit_tmp]])

  if (on_gap != "ignore" && any(unmatched)) {
    target <- out[[key_tmp]]

    if (length(by) == 0L) {
      lo <- suppressWarnings(min(t_vals, na.rm = TRUE))
      hi <- suppressWarnings(max(t_vals, na.rm = TRUE))
    } else {
      bounds <- dplyr::ungroup(dplyr::summarise(
        dplyr::group_by(data, dplyr::across(dplyr::all_of(by))),
        .lo = min(.data[[time]], na.rm = TRUE),
        .hi = max(.data[[time]], na.rm = TRUE),
        .groups = "drop"
      ))
      idx <- match(
        do.call(paste, c(out[by], sep = "\r")),
        do.call(paste, c(bounds[by], sep = "\r"))
      )
      lo <- bounds$.lo[idx]
      hi <- bounds$.hi[idx]
    }

    # A hole is an unmatched target that lies *inside* the observed range of
    # its group. Outside the range is simply the start or end of the series.
    is_gap <- unmatched & !is.na(target) & target >= lo & target <= hi

    if (any(is_gap)) {
      detail <- utils::head(out[is_gap, key_cols, drop = FALSE], 5L)
      msg <- paste0(
        sum(is_gap), " row(s) unmatched because the period ", time, " - ", k,
        " is absent from the middle of the series (a gap, not a series edge).\n",
        "First affected rows:\n",
        paste(utils::capture.output(print(detail, row.names = FALSE)),
              collapse = "\n")
      )
      if (on_gap == "error") {
        rlang::abort(msg, class = "join_lag_gap")
      } else {
        rlang::warn(msg, class = "join_lag_gap")
      }
    }
  }

  out[[key_tmp]] <- NULL
  out[[hit_tmp]] <- NULL
  out
}
