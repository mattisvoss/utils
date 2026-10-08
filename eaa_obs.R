# eaa_obs.R
#
# Makes all numbers for the Eurostat transmission from regional_items.
#
#   regional_items + config/eaa_hierarchy.csv + config/eaa_items.csv  -->  eaa_obs
#
# eaa_obs has exactly the rows and columns of the Eurostat SDMX-CSV files
# (the examples from Eurostat: AMS_EAACURR_A_IT_2021_0000_V0001.csv and others).
# write_eaa_sdmx() only splits it into one file for each dataflow and year.
#
#   DATAFLOW      "ESTAT:AMS_EAACURR_A(1.0)" (current prices, val)
#                 "ESTAT:AMS_EAACONR_A(1.0)" (previous-year prices, val_pyp)
#   FREQ          "A"
#   REF_AREA      "IE", "IE0" (both the State), "IE04", "IE05", "IE06"
#   AM_ITEM       Eurostat item code, for example "AM111000"
#   STAT_CHAR     "PRD_PP" value at producer prices    (component "value" in regional_items)
#                 "SUB"    subsidies on products       (component "subsidy")
#                 "TAX"    taxes on products           (component "tax")
#                 "PRD_BP" value at basic prices       (calculated: PRD_PP + SUB - TAX)
#                 Output items (AM180000 and the items below it) have all four.
#                 The other items have only PRD_BP.
#   REFERENCE     "VAL_N" (CURR) or "VAL_N-1" (CONR)
#   LAND_TYPE     "_Z"
#   UNIT_MEASURE  "MIO_NAC" (millions of national currency)
#   TIME_PERIOD   year
#   OBS_VALUE     the number. NA = not sent (written as NaN in the file).
#   OBS_STATUS    "N" for non-significant items, else NA
#   CONF_STATUS, OBS_COMMENT, UNIT_MULT   empty
#   OBS_PERIOD    "3009NP1"
#   DECIMALS      "7"
#
# Every item, STAT_CHAR and region has a row (the full grid), as in the examples.
#
# Two configuration files:
#   config/eaa_hierarchy.csv  parent, child, sign. The same for every country.
#   config/eaa_items.csv      your item table: estat_item, zero_for_IE,
#                             non_significant, empty_in_conr (Y or N).
#
# In _targets.R:
#   tar_target(eaa_hierarchy_file, "config/eaa_hierarchy.csv", format = "file"),
#   tar_target(eaa_items_file,     "config/eaa_items.csv",     format = "file"),
#   tar_target(eaa_obs, make_eaa_obs(regional_items, eaa_hierarchy_file, eaa_items_file)),
#   tar_target(eaa_files, write_eaa_sdmx(eaa_obs, years = 2022:2024, out_dir = "output/eurostat"),
#              format = "file")
#   tar_target(eaa_excel_files,                            # only for the parallel run
#              write_eaa_excel(eaa_obs, years = 2022:2024, template_dir = "config/templates",
#                              out_dir = "output/eurostat_excel"),
#              format = "file")


make_eaa_obs <- function(regional_items, hierarchy_file, items_file) {

  hierarchy <- readr::read_csv(hierarchy_file, col_types = "ccd")
  items     <- readr::read_csv(items_file, col_types = readr::cols(.default = "c"))
  for (col in c("zero_for_IE", "non_significant", "empty_in_conr")) {
    if (!all(items[[col]] %in% c("Y", "N"))) {
      stop(items_file, ": ", col, " must be Y or N in every row.")
    }
  }

  totals       <- unique(hierarchy$parent)               # calculated here, never in regional_items
  detail_items <- setdiff(items$estat_item, totals)       # from regional_items, or zero
  output_items <- items_below(hierarchy, "AM180000")      # have subsidies and taxes on products


  # 1. Check that every detailed item has a source.
  check_coverage(regional_items, items, totals)

  not_output <- filter(regional_items, component != "value", !item %in% output_items)
  if (nrow(not_output) > 0) {
    print(distinct(not_output, source, item, component))
    stop("Subsidies and taxes on products can only be on output items (see above).")
  }


  # 2. Add up the counties into the NUTS 2 regions and into the State.
  #    sum() keeps NA: NA means "no value", and step 7 finds it.
  counties <- unique(regional_items$county)
  regions  <- tibble(county = counties, region = nuts2_code(counties))

  x <- regional_items |>
    left_join(regions, by = "county") |>
    tidyr::pivot_longer(c(val, val_pyp), names_to = "measure", values_to = "value")

  nuts2 <- x |>
    group_by(item, component, region, year, measure) |>
    summarise(value = sum(value), .groups = "drop")

  state <- x |>
    group_by(item, component, year, measure) |>
    summarise(value = sum(value), .groups = "drop") |>
    mutate(region = "IE")

  x <- bind_rows(nuts2, state)


  # 3. Add a row with 0 for every detailed item, component, region, year and
  #    measure that has no row: the zero items, and the products with no
  #    subsidy or tax. This is safe only because step 1 found no gaps.
  #    explicit = FALSE: put 0 only in the NEW rows. Without it, complete()
  #    also changes an existing NA into 0, and a missing value disappears.
  x <- tidyr::complete(x,
                       item = detail_items,
                       component = c("value", "subsidy", "tax"),
                       region, year, measure,
                       fill = list(value = 0),
                       explicit = FALSE)


  # 4. Basic price = value + subsidy - tax.
  basic <- x |>
    group_by(item, region, year, measure) |>
    summarise(value = sum(if_else(component == "tax", -value, value)), .groups = "drop") |>
    mutate(component = "basic")

  x <- bind_rows(x, basic)


  # 5. Totals and balancing items. Calculate each total when all its parts
  #    are known. Repeat until all totals are known.
  todo <- totals
  while (length(todo) > 0) {
    ready <- todo[sapply(todo, function(p) all(hierarchy$child[hierarchy$parent == p] %in% x$item))]
    if (length(ready) == 0) {
      stop("These totals have a part that is not in the item table, or a cycle: ",
           paste(todo, collapse = ", "))
    }

    new <- hierarchy |>
      filter(parent %in% ready) |>
      inner_join(x, by = c(child = "item"), relationship = "many-to-many") |>
      group_by(item = parent, component, region, year, measure) |>
      summarise(value = sum(sign * value), .groups = "drop")

    x <- bind_rows(x, new)
    todo <- setdiff(todo, ready)
  }


  # 6. Keep only the components that exist: all four for output items,
  #    only "basic" for the other items (for example GVA or interest).
  x <- filter(x, item %in% output_items | component == "basic")


  # 7. Rules from the item table.
  x <- left_join(x, items, by = c(item = "estat_item"))

  is_zero <- x$zero_for_IE == "Y" | x$non_significant == "Y"
  if (any(is_zero & abs(x$value) > 1e-9, na.rm = TRUE)) {
    print(distinct(x[which(is_zero & abs(x$value) > 1e-9), ], item, component, measure))
    stop("These items are zero in the item table, but have a value.")
  }
  x$value[is_zero] <- 0
  x$flag <- if_else(x$non_significant == "Y", "N", NA_character_)

  # Items that are not sent at previous-year prices keep their row, with NA.
  # The file shows them as NaN, as in the Eurostat examples.
  not_sent <- x$measure == "val_pyp" & x$empty_in_conr == "Y"

  missing <- x[is.na(x$value) & !not_sent, ]
  if (nrow(missing) > 0) {
    print(distinct(missing, item, component, measure))
    stop("These items have no value (NA). Often val_pyp is missing in a line target.")
  }
  x$value[not_sent] <- NA


  # 8. IE0 (NUTS 1) is the whole State, so it has the same values as IE.
  x <- bind_rows(x, mutate(filter(x, region == "IE"), region = "IE0"))


  # 9. The columns of the SDMX file.
  stat_char <- c(value = "PRD_PP", subsidy = "SUB", tax = "TAX", basic = "PRD_BP")

  x |>
    transmute(DATAFLOW     = if_else(measure == "val", "ESTAT:AMS_EAACURR_A(1.0)", "ESTAT:AMS_EAACONR_A(1.0)"),
              FREQ         = "A",
              REF_AREA     = region,
              AM_ITEM      = item,
              STAT_CHAR    = unname(stat_char[component]),
              REFERENCE    = if_else(measure == "val", "VAL_N", "VAL_N-1"),
              LAND_TYPE    = "_Z",
              UNIT_MEASURE = "MIO_NAC",
              TIME_PERIOD  = year,
              OBS_VALUE    = value,
              OBS_STATUS   = flag,
              CONF_STATUS  = NA_character_,
              OBS_COMMENT  = NA_character_,
              OBS_PERIOD   = "3009NP1",
              UNIT_MULT    = NA_character_,
              DECIMALS     = "7") |>
    arrange(DATAFLOW, TIME_PERIOD, match(STAT_CHAR, stat_char), REF_AREA, AM_ITEM)
}


#' Write one SDMX-CSV file for each dataflow and year
#'
#' The files look like the Eurostat examples: separator ";", no quotes, a value
#' that is not sent is "NaN", empty attributes are empty.
#' File name: AMS_EAACURR_A_IE_2024_0000_V0001.csv
#'
#' @param eaa_obs  The result of make_eaa_obs().
#' @param years    The years to send.
#' @param out_dir  Folder for the files.
#' @param version  Version number in the file name. Increase it if you send a
#'                 corrected file for the same dataflow and year.
#' @return The file paths.
write_eaa_sdmx <- function(eaa_obs, years, out_dir, version = 1) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  files <- character(0)

  for (flow in unique(eaa_obs$DATAFLOW)) {
    for (year in years) {
      rows <- eaa_obs[eaa_obs$DATAFLOW == flow & eaa_obs$TIME_PERIOD == year, ]
      if (nrow(rows) == 0) stop(flow, " has no rows for ", year, ".")

      # Up to 8 decimals, no scientific notation, no trailing zeros: 812.93977214, 0, NaN.
      value <- round(rows$OBS_VALUE, 8)
      value[!is.na(value) & value == 0] <- 0          # no "-0"
      text <- formatC(value, format = "f", digits = 8)
      text <- sub("\\.?0+$", "", text)
      text[is.na(value)] <- "NaN"
      rows$OBS_VALUE <- text

      name <- sub("^ESTAT:(.*)\\(.*$", "\\1", flow)  # "AMS_EAACURR_A"
      file <- file.path(out_dir, sprintf("%s_IE_%d_0000_V%04d.csv", name, year, version))
      readr::write_delim(rows, file, delim = ";", na = "")
      files <- c(files, file)
    }
  }
  files
}


#' Write the numbers into the Eurostat Excel templates, for people to check
#'
#' Only for the parallel run: a person can compare these files with the files
#' of the old Excel system. Send the SDMX files from write_eaa_sdmx() to Eurostat.
#'
#' The template has the item codes in row 5 and the region codes in column A,
#' from row 7. Each item has three columns: value, flag, comment. The code finds
#' each cell by its item code and its region code.
#' A value that is not sent (NA) stays an empty cell.
#'
#' @param eaa_obs      The result of make_eaa_obs().
#' @param years        The years to write. One file for each dataflow and year.
#' @param template_dir Folder with AMS_EAACURR_A_IE.xlsx and AMS_EAACONR_A_IE.xlsx.
#' @param out_dir      Folder for the files.
#' @return The file paths.
write_eaa_excel <- function(eaa_obs, years, template_dir, out_dir) {
  templates <- c("ESTAT:AMS_EAACURR_A(1.0)" = "AMS_EAACURR_A_IE.xlsx",
                 "ESTAT:AMS_EAACONR_A(1.0)" = "AMS_EAACONR_A_IE.xlsx")
  sheets <- c(PRD_PP = "DATA_01", SUB = "DATA_02", TAX = "DATA_03", PRD_BP = "DATA_04")

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  files <- character(0)

  for (flow in names(templates)) {
    template <- file.path(template_dir, templates[[flow]])

    for (year in years) {
      wb <- openxlsx::loadWorkbook(template)

      for (sc in names(sheets)) {
        sheet <- sheets[[sc]]

        # Where the cells are: item codes in row 5, region codes in column A.
        codes   <- unlist(readxl::read_excel(template, sheet = sheet, range = "A5:ZZ5",
                                             col_names = FALSE, .name_repair = "minimal")[1, ])
        regions <- readxl::read_excel(template, sheet = sheet, range = "A1:A12",
                                      col_names = FALSE, .name_repair = "minimal")[[1]]
        # Rows 1 to 6 are the title, the country (A3 is also "IE") and the year.
        # The region rows start in row 7, so search only from there.
        regions[1:6] <- NA

        openxlsx::writeData(wb, sheet, year, startCol = 1, startRow = 4)

        rows <- eaa_obs[eaa_obs$DATAFLOW == flow & eaa_obs$TIME_PERIOD == year &
                          eaa_obs$STAT_CHAR == sc, ]
        if (nrow(rows) == 0) stop(flow, " has no rows for ", year, ".")

        for (i in seq_len(nrow(rows))) {
          col <- match(rows$AM_ITEM[i], codes)
          row <- match(rows$REF_AREA[i], regions)
          if (is.na(col) || is.na(row)) {
            stop(template, ", sheet ", sheet, ": no cell for ", rows$AM_ITEM[i], " ", rows$REF_AREA[i])
          }
          if (!is.na(rows$OBS_VALUE[i])) {
            openxlsx::writeData(wb, sheet, round(rows$OBS_VALUE[i], 8), startCol = col, startRow = row)
          }
          if (!is.na(rows$OBS_STATUS[i])) {
            openxlsx::writeData(wb, sheet, rows$OBS_STATUS[i], startCol = col + 1, startRow = row)
          }
        }
      }

      name <- sub("\\.xlsx$", "", templates[[flow]])        # "AMS_EAACURR_A_IE"
      file <- file.path(out_dir, sprintf("%s_%d.xlsx", name, year))
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      files <- c(files, file)
    }
  }
  files
}


#' Check that every detailed item has a source
#'
#' Prints the problems and stops. A detailed item must have rows in
#' regional_items, or be zero or non-significant in the item table.
#' A total must never be in regional_items.
check_coverage <- function(regional_items, items, totals) {
  values  <- filter(regional_items, component == "value")
  facts   <- unique(values$item)
  nonzero <- unique(values$item[abs(values$val) > 1e-9])
  zero    <- items$estat_item[items$zero_for_IE == "Y" | items$non_significant == "Y"]

  report <- tibble(item = union(items$estat_item, regional_items$item)) |>
    mutate(status = case_when(
      !item %in% items$estat_item         ~ "ERROR: not in the item table",
      item %in% totals & item %in% facts  ~ "ERROR: a total in regional_items (use the detailed items)",
      item %in% totals                    ~ "calculated",
      item %in% zero & item %in% nonzero  ~ "ERROR: zero in the item table, but regional_items has a value",
      item %in% facts                     ~ "fact",
      item %in% zero                      ~ "zero or non-significant",
      TRUE                                ~ "GAP: add a take(), or mark it zero in the item table"
    ))

  problems <- filter(report, grepl("^(GAP|ERROR)", status))
  if (nrow(problems) > 0) {
    print(problems, n = Inf)
    stop("Coverage problems (see above).")
  }
  invisible(report)
}


#' NUTS 2 code of each county
#'
#' NUTS codes are hierarchical: IE041 (Border) is in IE04 (Northern and
#' Western). So the NUTS 2 code is the first four characters of the NUTS 3 code.
nuts2_code <- function(counties) {
  nuts3 <- vapply(counties, county_to_nuts, character(1), mid_east_dub = FALSE, USE.NAMES = FALSE)
  code3 <- vapply(nuts3, nuts_to_code, character(1), USE.NAMES = FALSE)
  if (!all(grepl("^IE[0-9]{3}$", code3))) {
    stop("nuts_to_code() must give NUTS 3 codes such as IE041. It gave: ",
         paste(unique(code3), collapse = ", "))
  }
  substr(code3, 1, 4)
}


#' An item and all items below it in the hierarchy
items_below <- function(hierarchy, top) {
  out <- top
  repeat {
    new <- setdiff(hierarchy$child[hierarchy$parent %in% out], out)
    if (length(new) == 0) return(out)
    out <- c(out, new)
  }
}
