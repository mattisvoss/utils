# release_table_specs.R
#
# The content of each release table: which rows, which columns, and which
# text. When a table changes, change this file. Do not change release_tables.R.
#
# Keys and labels:
#   The item key is the PxStat statistic label (the px_stat_label column of the
#   mastersheet, for example "Forage Plants - Consumption"). The label is the
#   text that the release table shows (for example "Forage Plants").
#   The region key is the name that county_to_nuts() and nuts3_to_nuts2() give.


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
item_net_subsidies <- "Net Subsidies"
item_subs_pct_os   <- "Net Subsidies as a % of Operating Surplus"


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

build_table_1_1 <- function(obs, years) {

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
    groups        = c("Goods Output at Producer Prices", "Intermediate Consumption",
                      item_net_subsidies, "Operating Surplus"),
    group_labels  = c("Goods Output", "Intermediate Consumption",
                      "Net Subsidies", "Operating Surplus"),
    group_by      = "item",
    number_format = "#,##0"
  )

  list(sheet_name  = paste0("P-RAA", max(years), "TBL1.1"),
       panels      = list(panel),
       footnotes   = character(0),
       label_width = 18.5)
}


# ---- Table 1.2 ----------------------------------------------------------------
# Rows: items. Column groups: regions, in two panels of four.
# A row with item = NA is an empty row.
# "tibble::" is necessary: tar_source() runs this line before the packages are attached.

rows_1_2 <- tibble::tribble(
  ~label,                                          ~item,                                                                   ~bold,
  "Livestock",                                     "All Livestock",                                                         TRUE,
  "Cattle",                                        "Cattle",                                                                FALSE,
  "Pigs",                                          "Pigs",                                                                  FALSE,
  "Sheep",                                         "Sheep",                                                                 FALSE,
  "Horses",                                        "Horses",                                                                FALSE,
  "Poultry",                                       "Poultry",                                                               FALSE,
  "",                                              NA,                                                                      FALSE,
  "Livestock Products",                            "All Livestock Products",                                                TRUE,
  "Milk",                                          "Milk",                                                                  FALSE,
  "Other Livestock Products",                      "Other Products",                                                        FALSE,
  "",                                              NA,                                                                      FALSE,
  "Crops",                                         "All Crops",                                                             TRUE,
  "Cereals",                                       "Cereals",                                                               FALSE,
  "Potatoes",                                      "Potatoes",                                                              FALSE,
  "Forage Plants",                                 "Forage Plants - Output",                                                FALSE,
  "Fresh Vegetables",                              "Vegetables",                                                            FALSE,
  "Fresh Fruit",                                   "Fresh Fruit",                                                           FALSE,
  "Other Crops",                                   "Other Crops",                                                           FALSE,
  "",                                              NA,                                                                      FALSE,
  "Goods Output at Producer Prices",               "Goods Output at Producer Prices",                                       TRUE,
  "",                                              NA,                                                                      FALSE,
  "Contract Work",                                 "Contract Work - Output",                                                FALSE,
  "",                                              NA,                                                                      FALSE,
  "Subsidies on Products less Taxes on Products",  "Net Subsidies on Products",                                             FALSE,
  "",                                              NA,                                                                      FALSE,
  "Agricultural Output at Basic Prices",           "Agricultural Output at Basic Prices",                                   TRUE,
  "",                                              NA,                                                                      FALSE,
  "Intermediate Consumption",                      "Intermediate Consumption",                                              TRUE,
  "Feeding Stuffs",                                "Feedingstuffs",                                                         FALSE,
  "Fertilisers",                                   "Fertilisers",                                                           FALSE,
  paste0("FISIM", sup2),                           "Financial Intermediation Services Indirectly Measured (FISIM)",         FALSE,
  "Seeds",                                         "Seeds",                                                                 FALSE,
  "Maintenance & Repairs",                         "Maintenance and Repairs",                                               FALSE,
  "Other Goods & Services",                        "Other Goods and Services",                                              FALSE,
  "Crop Protection Products",                      "Crop Protection Products",                                              FALSE,
  "Veterinary Expenses",                           "Veterinary Expenses",                                                   FALSE,
  "Energy & Lubricants",                           "Energy and Lubricants",                                                 FALSE,
  "Forage Plants",                                 "Forage Plants - Consumption",                                           FALSE,
  "Contract Work",                                 "Contract Work - Expenditure",                                           FALSE,
  "",                                              NA,                                                                      FALSE,
  "Gross Value Added at Basic Prices",             "Gross Value Added at Basic Prices",                                     TRUE,
  "",                                              NA,                                                                      FALSE,
  "Fixed Capital Consumption",                     "Fixed Capital Consumption",                                             FALSE,
  "",                                              NA,                                                                      FALSE,
  "Net Value Added at Basic Prices",               "Net Value Added at Basic Prices",                                       TRUE,
  "",                                              NA,                                                                      FALSE,
  "Other Subsidies less Taxes on Production",      "Net Subsidies on Production: Other Subsidies Less Taxes on Production", FALSE,
  "",                                              NA,                                                                      FALSE,
  "Factor Income",                                 "Factor Income",                                                         TRUE,
  "",                                              NA,                                                                      FALSE,
  "Compensation of Employees",                     "Compensation of Employees",                                             FALSE,
  "",                                              NA,                                                                      FALSE,
  "Operating Surplus",                             "Operating Surplus",                                                     TRUE,
  "",                                              NA,                                                                      FALSE,
  "Land Rental",                                   "Land Rental",                                                           FALSE,
  paste0("Net Interest", sup3),                    "Net Interest",                                                          FALSE,
  "",                                              NA,                                                                      FALSE,
  "Entrepreneurial Income",                        "Entrepreneurial Income",                                                TRUE
)

build_table_1_2 <- function(obs, years, revised_years) {

  title <- paste0("Regional Agricultural Accounts at NUTS 3 level, ",
                  min(years), dash, max(years))

  make_panel <- function(title, regions) {
    build_panel(obs, title = title, rows = rows_1_2, years = years,
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
       label_width = 40.5)
}


# ---- Table 1.4 ----------------------------------------------------------------
# Rows: one block for each region (a heading row, three item rows, an empty row).
# No column groups. Order: each NUTS 2 region, then its NUTS 3 regions, then State.

build_table_1_4 <- function(obs, years, revised_years) {

  items  <- c(item_net_subsidies, "Operating Surplus", item_subs_pct_os)
  labels <- c("Net Subsidies", "Operating Surplus", item_subs_pct_os)

  region_block <- function(region, bold_heading) {
    tibble(label  = c(region, labels, ""),
           region = c(NA, rep(region, 3), NA),
           item   = c(NA, items, NA),
           bold   = c(bold_heading, FALSE, FALSE, FALSE, FALSE))
  }

  nuts2_of_nuts3 <- nuts3_to_nuts2(nuts3_regions)
  blocks <- list()
  for (n2 in unique(nuts2_of_nuts3)) {
    blocks <- c(blocks, list(region_block(n2, bold_heading = TRUE)))
    for (n3 in nuts3_regions[nuts2_of_nuts3 == n2]) {
      blocks <- c(blocks, list(region_block(n3, bold_heading = FALSE)))
    }
  }
  blocks <- c(blocks, list(region_block("State", bold_heading = TRUE)))
  rows   <- bind_rows(blocks)
  rows   <- rows[-nrow(rows), ]  # no empty row after the last block

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
       footnotes   = c(paste0(sup1, " Net subsidies: Subsidies on products less taxes on products ",
                              "plus subsidies on production less taxes on production."),
                       paste0(sup2, " Revised.")),
       label_width = 45.5)
}
