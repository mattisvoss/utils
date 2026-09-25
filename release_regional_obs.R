# regional_obs.R
#
# County mastersheet  -->  regional observations.
#
# All outputs (release tables, PxStat, Eurostat) read the regional
# observations. No output reads the mastersheet directly.
#
# Regional observations have one row for each number:
#   measure  mastersheet column that the value came from ("val", "val_pyp")
#   level    "NUTS3", "NUTS2" or "State"
#   region   region name ("Border", "Southern", "State", ...)
#   item     PxStat statistic label ("All Livestock", "Cattle", ...)
#   year     integer
#   value    numeric


#' Get the NUTS 2 region of each NUTS 3 region
#'
#' @param nuts3 Character vector of NUTS 3 names, as county_to_nuts() gives them.
#'   "Dublin & Mid-East" is accepted for mid_east_dub = TRUE.
#' @return Character vector of NUTS 2 names. Stops if a name is not known.
nuts3_to_nuts2 <- function(nuts3) {
  lookup <- c(
    "Border"            = "Northern and Western",
    "West"              = "Northern and Western",
    "Mid-West"          = "Southern",
    "South-East"        = "Southern",
    "South-West"        = "Southern",
    "Dublin"            = "Eastern and Midland",
    "Mid-East"          = "Eastern and Midland",
    "Dublin & Mid-East" = "Eastern and Midland",
    "Midland"           = "Eastern and Midland"
  )
  unknown <- setdiff(nuts3, names(lookup))
  if (length(unknown) > 0) {
    stop("nuts3_to_nuts2(): unknown NUTS 3 region: ", paste(unknown, collapse = ", "))
  }
  unname(lookup[nuts3])
}


#' Add up the county mastersheet to NUTS 3, NUTS 2 and State
#'
#' @param mastersheet  The regional pipeline output (mastersheet_output_regional).
#' @param value_col    Mastersheet column to add up. Use only additive measures:
#'   "val" and "val_pyp" are additive. Chain-linked volumes and indices are not.
#' @param item_col     Mastersheet column that identifies one item. It must give
#'   exactly one value for each county, item and year.
#' @param mid_east_dub TRUE: Dublin and Mid-East are one region (release, PxStat).
#'   FALSE: two regions (Eurostat).
#' @return Regional observations (see the top of this file).
to_regional_obs <- function(mastersheet,
                            value_col    = "val",
                            item_col     = "px_stat_label",
                            mid_east_dub = TRUE) {

  # 1. Keep only the columns that we need, with fixed names.
  obs <- tibble(
    county = mastersheet$county,
    item   = mastersheet[[item_col]],
    year   = as.integer(mastersheet$year_concerned),
    value  = mastersheet[[value_col]]
  )

  # 2. Check the data. Stop at the first problem.
  dups <- count(obs, county, item, year) |> filter(n > 1)
  if (nrow(dups) > 0) {
    print(dups)
    stop("More than one value for the same county, item and year. ",
         "The column '", item_col, "' does not identify one item.")
  }
  na_items <- unique(obs$item[is.na(obs$value)])
  if (length(na_items) > 0) {
    stop("'", value_col, "' is NA for these items: ", paste(na_items, collapse = ", "))
  }

  # 3. Add the NUTS 3 and NUTS 2 region of each county.
  obs$nuts3 <- vapply(obs$county, county_to_nuts, character(1),
                      mid_east_dub = mid_east_dub, USE.NAMES = FALSE)
  obs$nuts2 <- nuts3_to_nuts2(obs$nuts3)

  # 4. Add up the counties at each level.
  nuts3 <- obs |>
    group_by(region = nuts3, item, year) |>
    summarise(value = sum(value), .groups = "drop") |>
    mutate(level = "NUTS3")

  nuts2 <- obs |>
    group_by(region = nuts2, item, year) |>
    summarise(value = sum(value), .groups = "drop") |>
    mutate(level = "NUTS2")

  state <- obs |>
    group_by(item, year) |>
    summarise(value = sum(value), .groups = "drop") |>
    mutate(region = "State", level = "State")

  bind_rows(nuts3, nuts2, state) |>
    mutate(measure = value_col) |>
    select(measure, level, region, item, year, value)
}


#' Add a new item that is the sum of other items
#'
#' The sum is made in each region and year.
#' Example: Net Subsidies = Net Subsidies on Products + Other Subsidies less Taxes.
#'
#' @param obs      Regional observations.
#' @param new_item Name of the new item.
#' @param items    Names of the items to add up. All must be in obs.
#' @return obs with the new rows added.
add_sum_item <- function(obs, new_item, items) {
  new <- obs |>
    filter(item %in% items) |>
    group_by(measure, level, region, year) |>
    summarise(value = sum(value), n_found = n(), .groups = "drop")

  if (any(new$n_found != length(items))) {
    stop("add_sum_item(): not all of these items are in obs: ", paste(items, collapse = ", "))
  }

  bind_rows(obs, new |> mutate(item = new_item) |> select(-n_found))
}


#' Add a new item that is a ratio of two items
#'
#' The ratio is made from the region totals in each region and year.
#' Do not add up ratios: the sum of two ratios is not the ratio of the sums.
#'
#' @param obs         Regional observations.
#' @param new_item    Name of the new item.
#' @param numerator   Name of the item on top.
#' @param denominator Name of the item below.
#' @param scale       Multiplier. 100 gives a percentage.
#' @return obs with the new rows added.
add_ratio_item <- function(obs, new_item, numerator, denominator, scale = 100) {
  num <- obs |> filter(item == numerator)
  den <- obs |> filter(item == denominator) |> select(measure, level, region, year, den = value)

  new <- inner_join(num, den, by = c("measure", "level", "region", "year")) |>
    mutate(item = new_item, value = scale * value / den) |>
    select(-den)

  if (nrow(new) != nrow(num) || nrow(new) == 0) {
    stop("add_ratio_item(): '", numerator, "' and '", denominator,
         "' do not have the same regions and years.")
  }
  bind_rows(obs, new)
}


#' Add the share of the State total for each region
#'
#' Share = 100 * region value / State value, for the same measure, item and year.
#' The shares get a new measure name: for example "val" becomes "val_pct_of_state".
#' So a table finds a share by its measure, and the other values do not change.
#'
#' @param obs Regional observations.
#' @return obs with the share rows added.
add_share_of_state <- function(obs) {
  state <- obs |>
    filter(level == "State") |>
    select(measure, item, year, state_value = value)

  share <- inner_join(obs, state, by = c("measure", "item", "year")) |>
    mutate(measure = paste0(measure, "_pct_of_state"),
           value   = 100 * value / state_value) |>
    select(-state_value)

  bind_rows(obs, share)
}
