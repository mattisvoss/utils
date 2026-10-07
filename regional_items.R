# regional_items.R
#
# Collects the regional facts for the Eurostat path into one long table.
#
#   regional line targets  -->  regional_items  -->  eaa_obs  -->  eurostat_files
#
# regional_items has one row for each fact:
#   source     line target that the fact came from (for messages and checks)
#   item       Eurostat item code, for example "AM111000"
#   component  "value"   = value at producer prices, or the only value of a
#                          non-output item (CFC, interest, GFCF, ...)
#              "subsidy" = subsidies on products (template sheet DATA_02)
#              "tax"     = taxes on products (template sheet DATA_03)
#   county     county name
#   year       integer, from year_concerned
#   val        value at current prices (template CURR)
#   val_pyp    value at previous-year prices (template CONR). NA means "no value".
#
# WHICH ITEMS GO IN HERE
#   Only the items that you estimate, at the most detailed level where you
#   estimate them. Use the most detailed code that describes the number exactly.
#   - Totals do NOT go in here (for example AM010000 Cereals, AM011000 Wheat and
#     spelt, AM180000 Output). eaa_obs adds them up from the hierarchy.
#   - Balancing items do NOT go in here (GVA, NVA, factor income, operating
#     surplus, entrepreneurial income). eaa_obs calculates them.
#   - Items that are zero do NOT go in here (for example AM011200 Durum wheat).
#     The item table (zero_for_IE) writes them.
#   Example: you estimate common wheat, and durum wheat is zero. Then only
#   AM011100 goes in here. AM011000 = AM011100 + AM011200 (0), and AM010000 is
#   the sum of all cereals.
#
# In _targets.R, name each line target in the command. Targets finds the
# dependencies by reading the command, so it must see each target name:
#
#   tar_target(regional_items, assemble_regional_items(
#     cattle_levy_regional_value = cattle_levy_regional_value,
#     subsidy_regional_value     = subsidy_regional_value,
#     ...
#   ))


# ==== Part 1: general functions. You do not need to change these. ==============

#' Take rows from a line target and give them one Eurostat item code
#'
#' @param source    A regional line target. It must have the columns county,
#'   year_concerned, val and val_pyp.
#' @param ...       Conditions that select the rows, as in dplyr::filter().
#'   No conditions = all rows.
#' @param item      Eurostat item code, for example "AM111000".
#' @param component "value", "subsidy" or "tax".
#' @param add_up    FALSE: the rows must have one row for each county and year.
#'   TRUE: add up the rows for the same county and year into one value (for
#'   example four animals into one GFCF item).
#' @param again     FALSE: normal use. TRUE: use rows that another take()
#'   already uses, a second time, for a different item. Use it only for a value
#'   that the EAA records two times, for example contract work, which is output
#'   of agricultural services (AM150000) and also intermediate consumption of
#'   agricultural services (AM209100).
#' @return The selected rows. combine_regional_items() checks them.
take <- function(source, ..., item, component = "value", add_up = FALSE, again = FALSE) {
  name <- deparse(substitute(source))

  need <- c("county", "year_concerned", "val", "val_pyp")
  missing <- setdiff(need, names(source))
  if (length(missing) > 0) {
    stop(name, " has no column: ", paste(missing, collapse = ", "))
  }
  if (!grepl("^AM[0-9]{6}$", item)) {
    stop(name, ": '", item, "' is not a Eurostat item code (AM and 6 digits).")
  }
  if (!component %in% c("value", "subsidy", "tax")) {
    stop(name, ": component must be 'value', 'subsidy' or 'tax', not '", component, "'.")
  }

  rows <- source |> mutate(source_row = row_number()) |> filter(...)

  # No rows usually means a spelling error in a condition, for example "sheeps".
  if (nrow(rows) == 0) {
    stop(name, ": no rows match the conditions for ", item, " (", component, ").")
  }

  # n first: inside tibble(), the name "source" is the new text column.
  n <- nrow(source)
  tibble(source = name, source_row = rows$source_row, source_nrow = n,
         item = item, component = component, add_up = add_up, again = again,
         county = as.character(rows$county),
         year = as.integer(rows$year_concerned),
         val = rows$val, val_pyp = rows$val_pyp)
}


#' Mark rows of a line target as not used, with a reason
#'
#' Every row of a line target must be used by take() or skip(). Use skip() for
#' rows that are not a fact for Eurostat, for example a State total row.
#'
#' @param source A regional line target.
#' @param ...    Conditions that select the rows, as in dplyr::filter().
#' @param reason Why these rows are not used. It documents the decision.
#' @return The selected row numbers. combine_regional_items() removes them.
skip <- function(source, ..., reason) {
  name <- deparse(substitute(source))
  if (missing(reason) || !nzchar(reason)) stop(name, ": skip() needs a reason.")

  rows <- source |> mutate(source_row = row_number()) |> filter(...)
  if (nrow(rows) == 0) stop(name, ": no rows match the conditions of skip().")

  n <- nrow(source)
  tibble(source = name, source_row = rows$source_row, source_nrow = n,
         item = NA_character_, again = FALSE)
}


#' Check the parts and combine them into regional_items
#'
#' @param parts List of results of take() and skip().
#' @return regional_items (see the top of this file).
combine_regional_items <- function(parts) {
  all <- bind_rows(parts)

  # 1. Each row of each line target is used exactly one time (not counting
  #    take(again = TRUE)). Nothing is lost, and nothing is counted two times.
  first <- all[!all$again, ]
  twice <- count(first, source, source_row) |> filter(n > 1)
  if (nrow(twice) > 0) {
    print(twice)
    stop("The rows above are selected by more than one take() or skip(). ",
         "If a value must be used for two items, use take(again = TRUE) for the second item.")
  }
  for (s in unique(first$source)) {
    n <- first$source_nrow[first$source == s][1]
    not_used <- setdiff(seq_len(n), first$source_row[first$source == s])
    if (length(not_used) > 0) {
      stop(s, ": ", length(not_used), " rows are not used (rows ",
           paste(head(not_used, 10), collapse = ", "), if (length(not_used) > 10) ", ...",
           "). Give them an item with take(), or remove them with skip().")
    }
  }

  # A second use must be of a row that a normal take() already uses.
  # Otherwise it is a first use, and again = TRUE hides it from check 1.
  used <- paste(first$source, first$source_row)[!is.na(first$item)]
  second <- all[all$again, ]
  if (any(!paste(second$source, second$source_row) %in% used)) {
    print(distinct(second[!paste(second$source, second$source_row) %in% used, ], source, item))
    stop("take(again = TRUE) selects rows that no normal take() uses (see above).")
  }

  facts <- all[!is.na(all$item), ]

  # 2. Each county is a real county, and each value at current prices is known.
  counties <- unique(facts$county)
  nuts <- vapply(counties, county_to_nuts, character(1), USE.NAMES = FALSE)
  if (anyNA(nuts)) {
    stop("Unknown county: ", paste(counties[is.na(nuts)], collapse = ", "))
  }
  if (anyNA(facts$val)) {
    print(distinct(facts[is.na(facts$val), ], source, item, component))
    stop("The items above have NA in val.")
  }

  # 3. One take() gives one row for each county and year, unless add_up = TRUE.
  combined <- facts |>
    group_by(source, item, component, county, year) |>
    summarise(n = n(), add_up = any(add_up),
              val = sum(val), val_pyp = sum(val_pyp), .groups = "drop")
  not_unique <- filter(combined, n > 1 & !add_up)
  if (nrow(not_unique) > 0) {
    print(distinct(not_unique, source, item, component))
    stop("These take() calls select more than one row for the same county and year. ",
         "Make the conditions more exact, or use add_up = TRUE if the rows must be added.")
  }

  # 4. Each item and component comes from one line target only.
  #    This catches two take() calls that give the same item by mistake.
  dup <- combined |>
    group_by(item, component, county, year) |>
    filter(n() > 1) |>
    ungroup()
  if (nrow(dup) > 0) {
    print(distinct(dup, item, component, source))
    stop("More than one line target gives the same item and component (see above).")
  }

  combined |>
    select(source, item, component, county, year, val, val_pyp) |>
    arrange(item, component, county, year)
}


# ==== Part 2: the template. Fill this in. ======================================
#
# How to fill it in:
# 1. Add each regional line target as an argument, with its target name.
#    Add the same name to the tar_target() command in _targets.R.
# 2. For each line target, write one take() for each Eurostat item that it gives.
#    Select the rows with conditions, as in dplyr::filter().
# 3. Every row of every line target must be used exactly one time, by one take()
#    or by one skip() with a reason. If a row is not used, or is used two times,
#    the code stops and tells you the line target and the rows.
# 4. Use add_up = TRUE only where several rows for the same county and year must
#    be added into one item.
# 5. Read the LAST long target of each line. Do not read a target that feeds a
#    target that you already read (for example motor_tax_regional_value, which is
#    in production_tax_regional_value). Do not read a target that is only a sum of
#    other targets that you read (for example product_tax_regional_value). Then
#    no fact is counted two times.
#
# The lines below marked CHECK come from what we discussed. Check them.

assemble_regional_items <- function(
    # ---- Output (TODO: add the output line targets) ----

    # ---- Intermediate consumption (TODO: add the IC line targets) ----

    # ---- Taxes and subsidies on products, other subsidies, capital transfers ----
    cattle_levy_regional_value,
    pig_levy_regional_value,
    sheep_levy_regional_value,
    milk_levy_regional_value,
    subsidy_regional_value,

    # ---- Income items ----
    depreciation_regional_value,
    production_tax_regional_value,
    compensation_regional_value,
    property_income_regional_value,

    # ---- Capital account ----
    gfcf_stocks_regional_value) {

  parts <- list(

    # ==== Output at producer prices: component "value" =========================
    # TODO: one take() for each output item that you estimate, at the most
    # detailed level. Do not add totals such as AM010000 or AM110000.
    # Example:
    # take(cereals_regional_value, sub_product == "common_wheat", item = "AM011100"),
    # take(cereals_regional_value, sub_product == "barley",       item = "AM013000"),

    # ==== Intermediate consumption: component "value" ==========================
    # TODO: one take() for each IC item, for example AM201000 Seeds, AM206011,
    # AM206012 and AM206020 Feedingstuffs, AM207000 and AM208000 Maintenance,
    # AM209200 FISIM. Do not add the total AM200000.
    # Read the input targets (seed_regional_value, fertiliser_regional_value,
    # ...), not total_intermediate_consumption_less_fisim_regional_value: it is
    # a total, and it does not include FISIM, which Eurostat needs as an item.
    # A broad target such as nfs_inputs_regional_value can give several items:
    # one take() for each. Skip its rows that another target already gives.
    # Example of a value that the EAA records two times:
    # take(outputs_regional_value, product == "contract_work",
    #      item = "AM209100", again = TRUE),     # IC of agricultural services

    # ==== Taxes on products (DATA_03): component "tax" =========================
    # The item is the product that the tax is on. CHECK
    take(cattle_levy_regional_value, item = "AM111000", component = "tax"),
    take(pig_levy_regional_value,    item = "AM112000", component = "tax"),
    take(sheep_levy_regional_value,  item = "AM114000", component = "tax"),
    take(milk_levy_regional_value,   item = "AM121000", component = "tax"),

    # ==== Subsidies on products (DATA_02): component "subsidy" =================
    # CHECK that these are subsidies on products (coupled to the product).
    take(subsidy_regional_value, product == "subsidy", sub_product == "cattle",
         item = "AM111000", component = "subsidy"),
    take(subsidy_regional_value, product == "subsidy", sub_product == "sheep",
         item = "AM114000", component = "subsidy"),

    # ==== Other subsidies on production and capital transfers ==================
    # CHECK
    take(subsidy_regional_value, product == "subsidy", sub_product == "production_all",
         item = "AM310000"),
    take(subsidy_regional_value, product == "capital_transfer",
         item = "AM390000", add_up = TRUE),          # tams + all

    # ==== Income items ==========================================================
    # CHECK
    take(depreciation_regional_value,   item = "AM270000"),     # CFC
    take(production_tax_regional_value, item = "AM300000"),     # other taxes on production
    take(compensation_regional_value,   item = "AM290000"),     # compensation of employees
    take(property_income_regional_value, product == "land_rent",
         item = "AM340000"),                                     # rents
    take(property_income_regional_value, product == "interest_minus_fisim",
         item = "AM350000"),                                     # interest paid (excludes FISIM)
    # TODO: interest received (AM360000), for example from
    # interest_received_regional_value. Add it as an argument above.

    # ==== Capital account =======================================================
    # CHECK
    take(gfcf_stocks_regional_value, product == "gfcf",
         sub_product %in% c("cattle", "pig", "sheep", "poultry"),
         item = "AM230000", add_up = TRUE),          # GFCF in agricultural products
    take(gfcf_stocks_regional_value, product == "gfcf", sub_product == "non_agricultural",
         item = "AM210000"),                          # GFCF in non-agricultural products
    take(gfcf_stocks_regional_value, product == "stock",
         item = "AM380000", add_up = TRUE),           # changes in inventories

    # ==== Rows that are not facts ===============================================
    # Example, if a line target has a State row:
    # skip(subsidy_regional_value, county == "State",
    #      reason = "State total. The counties add up to it."),

    NULL  # Keep this last. Then every line above can end with a comma.
  )

  combine_regional_items(parts)
}
