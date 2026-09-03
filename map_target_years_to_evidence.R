#' Map target years to the year of their supporting evidence
#'
#' Builds a lookup table that connects each year you must estimate to the most
#' recent year for which you hold an observation. Use it when a source is not
#' annual, for example the Census of Agriculture, but the estimate is annual.
#'
#' The rule is last observation carried forward. For each target year, the
#' function takes the evidence years at or below that year, then keeps the
#' largest one. A target year that comes before the first evidence year has no
#' evidence, so the function gives `NA`.
#'
#' Use the result as a join table, not as a fill. A join fails loudly when it
#' does not match. A fill down a column depends on the order of the rows, and
#' it needs a row to exist before it can change it. Rows for the missing years
#' do not exist.
#'
#' The function maps years only. It cannot express interpolation, because
#' interpolation blends the values of two years. That is an operation on
#' values, not on years. Do it as a separate step.
#'
#' @param target_years Integer vector. The years you must produce an estimate
#'   for. Duplicates are kept, so the output has one row for each element.
#' @param evidence_years Integer vector. The years for which you hold an
#'   observation. The function sorts the vector and removes duplicates, so the
#'   order does not matter. Read this from the data, for example from the
#'   distinct `year_concerned` values where `sub_source` is `"CoA"`.
#' @param max_carry Number. The largest permitted distance in years between a
#'   target year and its evidence year. The default `Inf` puts no limit on the
#'   distance. A limit records a judgement about how long the assumption holds.
#'   A 2020 census is good evidence for 2022. It is weak evidence for 2030.
#'   Beyond the limit, the function treats the evidence as absent and gives
#'   `NA`, in the same way as for a year before the first evidence year.
#' @param require_all Logical. When `TRUE`, the default, the function stops if
#'   any target year has no evidence. Set it to `FALSE` only when you intend to
#'   drop those years, and make sure the caller removes the `NA` rows. An
#'   unnoticed `NA` disappears in an inner join, and the state total then leaks
#'   away with no message.
#'
#' @return A tibble with two integer columns:
#'   \describe{
#'     \item{target_year}{The reference year of the estimate.}
#'     \item{evidence_year}{The year the observation comes from. `NA` when no
#'       observation is available, or when the nearest observation is further
#'       away than `max_carry`.}
#'   }
#'   When `evidence_year` equals `target_year`, the estimate rests on a direct
#'   observation. When the two differ, the estimate rests on the assumption
#'   that the distribution did not change between the two years. Carry both
#'   columns through to the output, so that the quality report can state which
#'   figures come from direct data and which come from an assumption.
#'
#' @examples
#' # Census of Agriculture in 2020 only, estimates from 2020 to 2026.
#' map_target_years_to_evidence(2020:2026, 2020L)
#'
#' # Two census rounds. 2015 to 2019 take their evidence from 2010.
#' map_target_years_to_evidence(2010:2023, c(2010L, 2020L))
#'
#' # Refuse to carry a value more than five years forward.
#' map_target_years_to_evidence(
#'   2020:2026, 2020L,
#'   max_carry = 5, require_all = FALSE
#' )
#'
#' @importFrom tibble tibble
#' @export
map_target_years_to_evidence <- function(target_years,
                                         evidence_years,
                                         max_carry   = Inf,
                                         require_all = TRUE) {

  target   <- as.integer(target_years)
  evidence <- sort(unique(as.integer(evidence_years)))

  # An empty source is always a fault, never a valid state. Stop here, because
  # the cause is clear at this point and hard to find later.
  if (length(evidence) == 0L) {
    stop("`evidence_years` holds no years. The source gave no data.",
         call. = FALSE)
  }
  if (anyNA(target)) {
    stop("`target_years` holds NA.", call. = FALSE)
  }
  if (length(max_carry) != 1L || is.na(max_carry) || max_carry < 0) {
    stop("`max_carry` must be one number that is 0 or more.", call. = FALSE)
  }

  # findInterval() gives the position of the largest element of `evidence`
  # that is at or below each target year. It needs a sorted vector, which
  # `evidence` is. The function does a binary search in C, so it needs no
  # loop. It gives 0 when the target year comes before the first evidence
  # year.
  idx <- findInterval(target, evidence)

  out   <- rep(NA_integer_, length(target))
  found <- idx > 0L
  out[found] <- evidence[idx[found]]

  # Apply the limit after the search, not during it. The nearest evidence year
  # stays the nearest one. The limit only decides whether you accept it.
  too_far <- !is.na(out) & (target - out) > max_carry
  out[too_far] <- NA_integer_

  if (require_all && anyNA(out)) {
    before <- unique(target[is.na(out) & !too_far])
    beyond <- unique(target[too_far])
    msg <- character(0)
    if (length(before)) {
      msg <- c(msg, paste0(
        "No evidence at or before: ", paste(before, collapse = ", "),
        ". The earliest evidence year is ", evidence[1L], "."
      ))
    }
    if (length(beyond)) {
      msg <- c(msg, paste0(
        "Further than max_carry = ", max_carry, " years: ",
        paste(beyond, collapse = ", "), "."
      ))
    }
    stop(paste(msg, collapse = " "), call. = FALSE)
  }

  tibble::tibble(
    target_year   = target,
    evidence_year = out
  )
}
