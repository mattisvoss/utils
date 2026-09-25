# release_table_specs.R
#
# The layout of each release table: titles, footnotes, region order and
# column widths. The item lists (which PxStat item goes in which row or
# column, its label, and bold rows) are in specs/release_items.csv.
#
# Keys and labels:
#   item   PxStat statistic label, the px_stat_label column of the mastersheet
#          (for example "Forage Plants - Consumption").
#   label  Text that the release table shows (for example "Forage Plants").
#   region Name that county_to_nuts() and nuts3_to_nuts2() give.


# ---- Shared settings ----------------------------------------------------------

# Superscript footnote marks and the en dash.
sup1 <- "\u00b9"
sup2 <- "\u00b2"
sup3 <- "\u00b3"
dash <- "\u2013"

# NUTS 3 regions in table order. The names must be the same as the names
# that county_to_nuts(..., mid_east_dub = TRUE) gives.
nuts3_regions <- c("Border", "West", "Mid-West", "South-East",
                   "South-West", "Dublin & Mid-East", "Midland")

# Items that the release tables calculate. They are not in PxStat.
# specs/release_items.csv uses these names too.
item_net_subsidies <- "Net Subsidies"
item_subs_pct_os   <- "Net Subsidies as a % of Operating Surplus"

# Footnote text that more than one table uses.
note_net_subsidies <- paste("Net subsidies: Subsidies on products less taxes on products",
                            "plus subsidies on production less taxes on production.")


#' Read the item lists of the release tables
#'
#' The file has one row for each table row (Table 1.2, 1.4) or column
#' (Table 1.1, 1.3), in table order. Columns:
#'   table  "1.1", "1.2", "1.3" or "1.4"
#'   label  text in the table. The two characters \n make a line break. Empty for an empty row.
#'   item   PxStat statistic label. Empty for an empty row.
#'   bold   TRUE for a total row. Empty means FALSE.
#'
#' @param file Path of specs/release_items.csv.
#' @return Tibble with the columns table, label, item and bold.
read_release_items <- function(file) {
  # read_csv() reads UTF-8 and removes the byte order mark (the file has one,
  # so that Excel shows the superscripts correctly). It does not change the
  # text to the local encoding. All columns are read as text.
  items <- readr::read_csv(file, col_types = readr::cols(.default = "c"), na = "")

  if (!identical(names(items), c("table", "label", "item", "bold"))) {
    stop(file, ": the columns must be table, label, item, bold.")
  }
  for (t in c("1.1", "1.2", "1.3", "1.4")) {
    if (!any(items$table == t)) stop(file, ": there are no rows for table ", t, ".")
  }

  tibble(
    table = items$table,
    label = gsub("\\n", "\n", ifelse(is.na(items$label), "", items$label), fixed = TRUE),
    item  = items$item,
    bold  = !is.na(items$bold) & items$bold == "TRUE"
  )
}


#' Regions in nested order: each NUTS 2 region, then its NUTS 3 regions
#'
#' Tables 1.3 and 1.4 use this order. State is not included.
#'
#' @return Tibble with columns region and is_nuts2.
nested_regions <- function() {
  nuts2_of_nuts3 <- nuts3_to_nuts2(nuts3_regions)
  out <- list()
  for (n2 in unique(nuts2_of_nuts3)) {
    out <- c(out, list(tibble(region = n2, is_nuts2 = TRUE),
                       tibble(region = nuts3_regions[nuts2_of_nuts3 == n2], is_nuts2 = FALSE)))
  }
  bind_rows(out)
}


#' Make the regional observations for the release tables
#'
#' Values at current prices, with Dublin & Mid-East as one region, and the
#' two calculated items.
#'
#' @param mastersheet The regional pipeline output (mastersheet_output_regional).
#' @return Regional observations.
make_release_obs <- function(mastersheet) {
  obs <- to_regional_obs(mastersheet, value_col = "val", item_col = "px_stat_label",
                         mid_east_dub = TRUE)

  obs <- add_sum_item(obs, item_net_subsidies,
                      c("Net Subsidies on Products",
                        "Net Subsidies on Production: Other Subsidies Less Taxes on Production"))

  add_ratio_item(obs, item_subs_pct_os,
                 numerator = item_net_subsidies, denominator = "Operating Surplus")
}


# ---- Table 1.1 ----------------------------------------------------------------
# Rows: regions. Column groups: items.

build_table_1_1 <- function(obs, items, years) {

  cols <- items[items$table == "1.1", ]

  rows <- tibble(
    label  = c(nuts3_regions, "State"),
    region = c(nuts3_regions, "State"),
    bold   = c(rep(FALSE, length(nuts3_regions)), TRUE)
  )

  panel <- build_panel(
    obs,
    title = paste0("Table 1.1: Output, Input and Income in Agriculture by NUTS 3 Regions, ",
                   min(years), "-", max(years)),
    rows          = rows,
    years         = years,
    groups        = cols$item,
    group_labels  = cols$label,
    group_by      = "item",
    number_format = "#,##0"
  )

  list(sheet_name  = paste0("P-RAA", max(years), "TBL1.1"),
       panels      = list(panel),
       footnotes   = character(0),
       label_width = 18.5,
       value_width = 9)
}


# ---- Table 1.2 ----------------------------------------------------------------
# Rows: items. Column groups: regions, in two panels of four.

build_table_1_2 <- function(obs, items, years, revised_years) {

  rows <- items[items$table == "1.2", c("label", "item", "bold")]

  title <- paste0("Regional Agricultural Accounts at NUTS 3 level, ",
                  min(years), dash, max(years))

  make_panel <- function(title, regions) {
    build_panel(obs, title = title, rows = rows, years = years,
                year_labels = year_labels(years, revised = revised_years, mark = sup1),
                groups = regions, group_by = "region", stub_header = "Description")
  }

  list(sheet_name  = paste0("P-RAA", max(years), "TBL1.2"),
       panels      = list(make_panel(paste0("Table 1.2: ", title),
                                     nuts3_regions[1:4]),
                          make_panel(paste0("Table 1.2 continued: ", title),
                                     c(nuts3_regions[5:7], "State"))),
       footnotes   = c(paste0(sup1, " Revised."),
                       paste0(sup2, " FISIM: Financial Intermediation Services Indirectly Measured."),
                       paste0(sup3, " Net Interest: Interest Paid less Interest Received less FISIM.")),
       label_width = 40.5,
       value_width = 9)
}


# ---- Table 1.3 ----------------------------------------------------------------
# One year. Rows: for each region, the value and the % of the State total.
# Columns: items, with no spacer columns and no year header row.

build_table_1_3 <- function(obs, items, year) {

  cols <- items[items$table == "1.3", ]

  # The shares have the measure "val_pct_of_state" (see add_share_of_state()).
  obs <- add_share_of_state(obs)

  # For each region: the value row, the share row, an empty row.
  region_block <- function(region, bold) {
    tibble(label   = c(region, "% of State total", ""),
           region  = c(region, region, NA),
           measure = c("val", "val_pct_of_state", NA),
           bold    = c(bold, FALSE, FALSE))
  }

  regions <- nested_regions()
  rows <- bind_rows(Map(region_block, regions$region, regions$is_nuts2))
  rows <- bind_rows(rows, tibble(label = "State", region = "State", measure = "val", bold = TRUE))

  panel <- build_panel(
    obs,
    title = paste0("Table 1.3: Regional Distribution of Agricultural Output, Input and Income, ",
                   year),
    rows         = rows,
    years        = year,
    year_labels  = NULL,
    groups       = cols$item,
    group_labels = cols$label,
    group_by     = "item",
    spacer       = FALSE,
    stub_header  = "Region"
  )

  list(sheet_name  = paste0("P-RAA", year, "TBL1.3"),
       panels      = list(panel),
       footnotes   = paste0(sup1, " ", note_net_subsidies),
       label_width = 25.5,
       value_width = 13)
}


# ---- Table 1.4 ----------------------------------------------------------------
# Rows: one block for each region (a heading row, the item rows, an empty row).
# No column groups. Order: each NUTS 2 region, then its NUTS 3 regions, then State.

build_table_1_4 <- function(obs, items, years, revised_years) {

  block_items <- items[items$table == "1.4", ]

  region_block <- function(region, bold_heading) {
    n <- nrow(block_items)
    tibble(label  = c(region, block_items$label, ""),
           region = c(NA, rep(region, n), NA),
           item   = c(NA, block_items$item, NA),
           bold   = c(bold_heading, rep(FALSE, n), FALSE))
  }

  regions <- bind_rows(nested_regions(), tibble(region = "State", is_nuts2 = TRUE))
  rows <- bind_rows(Map(region_block, regions$region, regions$is_nuts2))
  rows <- rows[-nrow(rows), ]  # no empty row after the last block

  panel <- build_panel(
    obs,
    title = paste0("Table 1.4: Net Subsidies", sup1, " and Operating Surplus by Region, ",
                   min(years), dash, max(years)),
    rows        = rows,
    years       = years,
    year_labels = year_labels(years, revised = revised_years, mark = sup2),
    stub_header = "Region"
  )

  list(sheet_name  = paste0("P-RAA", max(years), "TBL1.4"),
       panels      = list(panel),
       footnotes   = c(paste0(sup1, " ", note_net_subsidies),
                       paste0(sup2, " Revised.")),
       label_width = 45.5,
       value_width = 12.5)
}
