# release_table_specs.R
#
# The content of each release table: titles, footnotes, region names and
# order, column widths. The item lists (which statistic goes in which row or
# column, its label, and bold rows) are in config/release_items.csv.
#
# Keys and labels:
#   item    PxStat statistic code ("ACA03C33"), or a calculated item key.
#   region  PxStat region code ("IE06106").
#   label   Text that the table shows ("Forage Plants", "Dublin & Mid-East").


# ---- Shared settings ----------------------------------------------------------

# Superscript footnote marks and the en dash.
sup1 <- "¹"
sup2 <- "²"
dash <- "–"

# Region codes and the names that the release shows.
# The order is the order of Tables 1.3 and 1.4: each NUTS 2 region, then its
# NUTS 3 regions, then State. The other tables use the NUTS 3 rows in this order.
# "tibble::" is necessary: tar_source() runs this line before the packages are attached.
release_regions <- tibble::tribble(
  ~code,     ~name,
  "IE04",    "Northern and Western",
  "IE041",   "Border",
  "IE042",   "West",
  "IE05",    "Southern",
  "IE051",   "Mid-West",
  "IE052",   "South-East",
  "IE053",   "South-West",
  "IE06",    "Eastern and Midland",
  "IE06106", "Dublin & Mid-East",
  "IE063",   "Midland",
  "-",       "State"
)

# Items that the release tables calculate. They are not in PxStat.
# config/release_items.csv uses these keys too.
item_net_subsidies <- "NET_SUBSIDIES"
item_subs_pct_os   <- "NET_SUBSIDIES_PCT_OS"

note_net_subsidies <- paste("Net subsidies: Subsidies on products less taxes on products",
                            "plus subsidies on production less taxes on production.")


#' Get the release regions at one or more levels, in table order
#'
#' @param level "State", "NUTS2" and/or "NUTS3".
#' @return Rows of release_regions, with a level column added.
regions_at <- function(level) {
  r <- release_regions
  r$level <- pxstat_region_level(r$code)
  r[r$level %in% level, ]
}


#' Read the item lists of the release tables
#'
#' The file has one row for each table row or column, in table order. Columns:
#'   table         "1.1", "1.2", "1.3", "1.4", or "2.x" (Tables 2.1 to 2.7)
#'   label         Text in the table. The two characters \n make a line break.
#'   item          PxStat statistic code, or a calculated item key.
#'   bold          TRUE for a total row. Empty means FALSE.
#'   space_before  TRUE puts an empty row before this row. Empty means FALSE.
#'
#' @param file Path of config/release_items.csv.
#' @return Tibble with the five columns above.
read_release_items <- function(file) {
  # read_csv() reads UTF-8 and removes the byte order mark (the file has one,
  # so that Excel shows the superscripts correctly). It does not change the
  # text to the local encoding. All columns are read as text.
  items <- readr::read_csv(file, col_types = readr::cols(.default = "c"), na = "")

  required <- c("table", "label", "item", "bold", "space_before")
  if (!all(required %in% names(items))) {
    stop(file, ": the columns must include ", paste(required, collapse = ", "), ".")
  }
  if (anyNA(items$table) || anyNA(items$label) || anyNA(items$item)) {
    stop(file, ": each row must have a table, a label and an item.")
  }
  for (t in c("1.1", "1.2", "1.3", "1.4", "2.x")) {
    if (!any(items$table == t)) stop(file, ": there are no rows for table ", t, ".")
  }

  tibble(
    table        = items$table,
    label        = gsub("\\n", "\n", items$label, fixed = TRUE),
    item         = items$item,
    bold         = !is.na(items$bold) & items$bold == "TRUE",
    space_before = !is.na(items$space_before) & items$space_before == "TRUE"
  )
}


#' Put an empty row before each row that has space_before = TRUE
#'
#' @param rows Item rows of one table.
#' @return Tibble with the columns label, item and bold. An empty row has
#'   label "" and item NA, so build_panel() gives it no numbers.
add_empty_rows <- function(rows) {
  empty <- tibble(label = "", item = NA_character_, bold = FALSE)
  out <- list()
  for (i in seq_len(nrow(rows))) {
    if (rows$space_before[i]) out <- c(out, list(empty))
    out <- c(out, list(rows[i, c("label", "item", "bold")]))
  }
  bind_rows(out)
}


#' Make the observations for the release tables
#'
#' The PxStat values (current prices), and the two calculated items.
#'
#' @param pxstat_tbl The PxStat table ACA03 in long format.
#' @return Observations.
make_release_obs <- function(pxstat_tbl) {
  obs <- pxstat_to_obs(pxstat_tbl)

  # Net Subsidies = Net Subsidies on Products + Other Subsidies less Taxes on Production
  obs <- add_sum_item(obs, item_net_subsidies, c("ACA03C21", "ACA03C38"))

  # Net Subsidies as a % of Operating Surplus
  add_ratio_item(obs, item_subs_pct_os,
                 numerator = item_net_subsidies, denominator = "ACA03C41")
}


#' Build and write all release tables
#'
#' @param obs           Observations from make_release_obs().
#' @param items         Item lists from read_release_items().
#' @param year          Release year.
#' @param revised_years Years with the mark "Revised" (Table 1.4).
#' @param out_dir       Folder for the files.
#' @return The file paths.
write_release_tables <- function(obs, items, year, revised_years, out_dir) {
  tables <- list(build_table_1_1(obs, items, year),
                 build_table_1_2(obs, items, year),
                 build_table_1_3(obs, items, year),
                 build_table_1_4(obs, items, year, revised_years))

  nuts3 <- regions_at("NUTS3")
  for (i in seq_len(nrow(nuts3))) {
    tables <- c(tables, list(build_table_2(obs, items, year, nuts3$code[i], nuts3$name[i],
                                           number = i)))
  }

  vapply(tables, write_release_table, character(1), out_dir = out_dir)
}


# ---- Table 1.1 ----------------------------------------------------------------
# Rows: NUTS 3 regions and State. Columns: for each item, the last four years.

build_table_1_1 <- function(obs, items, year) {
  years <- (year - 3):year
  it    <- items[items$table == "1.1", ]

  regions <- regions_at(c("NUTS3", "State"))
  rows <- tibble(label  = regions$name,
                 region = regions$code,
                 bold   = regions$level == "State")

  cols <- tibble(group  = rep(it$label, each = length(years)),
                 label  = rep(as.character(years), times = nrow(it)),
                 format = "#,##0",
                 item   = rep(it$item, each = length(years)),
                 year   = rep(years, times = nrow(it)))

  panel <- build_panel(
    obs, rows = rows, cols = cols,
    title = paste0("Table 1.1: Output, Input and Income in Agriculture by NUTS 3 Regions, ",
                   min(years), "-", year)
  )

  list(sheet_name = paste0("P-RAA", year, "TBL1.1"), panels = list(panel),
       footnotes = character(0), label_width = 18.5, value_width = 9, spacer_width = 3.5)
}


# ---- Table 1.2 ----------------------------------------------------------------
# One year. Rows: items. Columns: NUTS 3 regions and State.

build_table_1_2 <- function(obs, items, year) {
  rows    <- add_empty_rows(items[items$table == "1.2", ])
  regions <- regions_at(c("NUTS3", "State"))

  cols <- tibble(group  = "",
                 label  = regions$name,
                 format = "#,##0.0",
                 region = regions$code,
                 year   = year)

  panel <- build_panel(
    obs, rows = rows, cols = cols, stub_header = "Description",
    title = paste0("Table 1.2: Regional Agricultural Accounts at NUTS 3 level, ", year)
  )

  list(sheet_name = paste0("P-RAA", year, "TBL1.2"), panels = list(panel),
       footnotes  = c(paste0(sup1, " FISIM: Financial Intermediation Services Indirectly Measured."),
                      paste0(sup2, " Net Interest: Interest Paid less Interest Received less FISIM.")),
       label_width = 40.5, value_width = 12, spacer_width = 3.5)
}


# ---- Table 1.3 ----------------------------------------------------------------
# One year. Rows: for each region, the value and the % of the State total.
# Columns: items.

build_table_1_3 <- function(obs, items, year) {
  it  <- items[items$table == "1.3", ]
  obs <- add_share_of_state(obs)

  # For each region: the value row, the share row, an empty row.
  region_block <- function(code, name, bold) {
    tibble(label   = c(name, "% of State total", ""),
           region  = c(code, code, NA),
           measure = c("val", "val_pct_of_state", NA),
           bold    = c(bold, FALSE, FALSE))
  }
  regions <- regions_at(c("NUTS2", "NUTS3"))
  state   <- regions_at("State")
  rows <- bind_rows(Map(region_block, regions$code, regions$name, regions$level == "NUTS2"))
  rows <- bind_rows(rows, tibble(label = state$name, region = state$code,
                                 measure = "val", bold = TRUE))

  cols <- tibble(group = "", label = it$label, format = "#,##0.0", item = it$item, year = year)

  panel <- build_panel(
    obs, rows = rows, cols = cols, stub_header = "Region",
    title = paste0("Table 1.3: Regional Distribution of Agricultural Output, Input and Income, ", year)
  )

  list(sheet_name = paste0("P-RAA", year, "TBL1.3"), panels = list(panel),
       footnotes = paste0(sup1, " ", note_net_subsidies),
       label_width = 25.5, value_width = 13, spacer_width = 3.5)
}


# ---- Table 1.4 ----------------------------------------------------------------
# Rows: for each region, a heading row, the item rows and an empty row.
# Columns: the last four years.

build_table_1_4 <- function(obs, items, year, revised_years) {
  years <- (year - 3):year
  it    <- items[items$table == "1.4", ]

  region_block <- function(code, name, bold_heading) {
    n <- nrow(it)
    tibble(label  = c(name, it$label, ""),
           region = c(NA, rep(code, n), NA),
           item   = c(NA, it$item, NA),
           bold   = c(bold_heading, rep(FALSE, n), FALSE))
  }
  regions <- regions_at(c("NUTS2", "NUTS3", "State"))
  rows <- bind_rows(Map(region_block, regions$code, regions$name, regions$level != "NUTS3"))
  rows <- rows[-nrow(rows), ]  # no empty row after the last block

  cols <- tibble(group  = "",
                 label  = year_labels(years, revised = revised_years, mark = sup2),
                 format = "#,##0.0",
                 year   = years)

  panel <- build_panel(
    obs, rows = rows, cols = cols, stub_header = "Region",
    title = paste0("Table 1.4: Net Subsidies", sup1, " and Operating Surplus by Region, ",
                   min(years), dash, year)
  )

  list(sheet_name = paste0("P-RAA", year, "TBL1.4"), panels = list(panel),
       footnotes = c(paste0(sup1, " ", note_net_subsidies), paste0(sup2, " Revised.")),
       label_width = 45.5, value_width = 12.5, spacer_width = 3.5)
}


# ---- Tables 2.1 to 2.7 ----------------------------------------------------------
# One table for each NUTS 3 region. Rows: items.
# Columns: the last four years | the change from the year before | % of State.

build_table_2 <- function(obs, items, year, region, region_name, number) {
  years <- (year - 3):year
  it    <- items[items$table == "2.x", ]

  obs <- add_change(obs, year)
  obs <- add_share_of_state(obs)
  obs <- obs[obs$region == region, ]  # this table has only one region

  rows <- tibble(label = it$label, item = it$item, bold = it$bold)

  cols <- tibble(
    group   = c(rep("€ million", 4),
                rep(paste0("Change ", year, "/", year - 1), 2),
                "% Share of\nState Total"),
    label   = c(as.character(years), "Value (€m)", "Value (%)", as.character(year)),
    format  = c(rep("#,##0", 4), "0", "0", "0"),
    measure = c(rep("val", 4), "val_change", "val_pct_change", "val_pct_of_state"),
    year    = c(years, year, year, year)
  )

  panel <- build_panel(
    obs, rows = rows, cols = cols, stub_header = "Description", unit = NULL,
    title = paste0("Table 2.", number, ": Regional Agricultural Accounts at NUTS 3 level for ",
                   region_name, " Region, ", min(years), "-", year)
  )

  list(sheet_name = paste0("P-RAA", year, "TBL2.", number), panels = list(panel),
       footnotes = character(0), label_width = 30.5, value_width = 10.5, spacer_width = 8.5)
}
