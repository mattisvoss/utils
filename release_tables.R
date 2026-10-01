# release_tables.R
#
# The general part of the release tables. It is the same for all tables.
# Do not change this file for one table. Change release_table_specs.R.
#
#   PxStat table ACA03 (long)  -->  observations  -->  panel  -->  Excel file
#
# Observations ("obs") have one row for each number:
#   measure  "val" for the PxStat values. Calculated values get other names,
#            for example "val_pct_of_state" or "val_change".
#   level    "State", "NUTS2" or "NUTS3"
#   region   PxStat region code: "-" State, "IE04" NUTS 2, "IE041" NUTS 3
#   item     PxStat statistic code ("ACA03C01"), or a calculated item key
#   year     integer
#   value    numeric
#
# A panel is one block of a table. It has a title row, one or two header rows
# and a body. The rows and the columns each have keys (obs column names).
# A cell is the one obs value that matches the keys of its row and its column.


# ---- PxStat table to observations ---------------------------------------------

#' Get the level of each PxStat region code
#'
#' "-" is State. "IE" + 2 digits is NUTS 2. "IE" + 3 or more digits is NUTS 3.
#'
#' @param code Character vector of region codes.
#' @return "State", "NUTS2" or "NUTS3" for each code. Stops if a code is not known.
pxstat_region_level <- function(code) {
  level <- rep(NA_character_, length(code))
  level[code == "-"]                  <- "State"
  level[grepl("^IE[0-9]{2}$", code)]  <- "NUTS2"
  level[grepl("^IE[0-9]{3,}$", code)] <- "NUTS3"

  unknown <- unique(code[is.na(level)])
  if (length(unknown) > 0) {
    stop("Unknown region code: ", paste(unknown, collapse = ", "))
  }
  level
}


#' Make observations from the PxStat table ACA03 in long format
#'
#' @param px Table with the columns STATISTIC, TLIST(A1), C02196V04140 and VALUE.
#' @param stat_col,year_col,region_col,value_col The column names in px.
#' @return Observations. Stops if a value is missing, or if a statistic,
#'   year and region has more than one value.
pxstat_to_obs <- function(px,
                          stat_col   = "STATISTIC",
                          year_col   = "TLIST(A1)",
                          region_col = "C02196V04140",
                          value_col  = "VALUE") {

  missing_cols <- setdiff(c(stat_col, year_col, region_col, value_col), names(px))
  if (length(missing_cols) > 0) {
    stop("The PxStat table has no column: ", paste(missing_cols, collapse = ", "))
  }

  # as.character() first: if a column is a factor, as.numeric() gives the
  # factor level numbers, not the values.
  region <- as.character(px[[region_col]])
  obs <- tibble(
    measure = "val",
    level   = pxstat_region_level(region),
    region  = region,
    item    = as.character(px[[stat_col]]),
    year    = as.integer(as.character(px[[year_col]])),
    value   = suppressWarnings(as.numeric(as.character(px[[value_col]])))
  )

  bad <- obs[is.na(obs$year) | is.na(obs$value), ]
  if (nrow(bad) > 0) {
    print(bad)
    stop("The PxStat rows above have no year or no numeric value.")
  }

  key <- paste(obs$item, obs$year, obs$region)
  if (anyDuplicated(key)) {
    print(obs[key %in% key[duplicated(key)], ])
    stop("The PxStat table has more than one value for the same statistic, year and region.")
  }

  obs
}


# ---- Calculated values ----------------------------------------------------------

#' Add a new item that is the sum of other items, in each region and year
#'
#' @param obs      Observations.
#' @param new_item Key of the new item.
#' @param items    Keys of the items to add up. All must be in obs.
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


#' Add a new item that is a ratio of two items, in each region and year
#'
#' The ratio is made from the region totals. Do not add up ratios: the sum
#' of two ratios is not the ratio of the sums.
#'
#' @param obs         Observations.
#' @param new_item    Key of the new item.
#' @param numerator   Key of the item on top.
#' @param denominator Key of the item below.
#' @param scale       Multiplier. 100 gives a percentage.
#' @return obs with the new rows added.
add_ratio_item <- function(obs, new_item, numerator, denominator, scale = 100) {
  num <- obs |> filter(item == numerator)
  den <- obs |> filter(item == denominator) |> select(measure, level, region, year, den = value)

  new <- inner_join(num, den, by = c("measure", "level", "region", "year")) |>
    mutate(item = new_item, value = scale * value / den) |>
    select(-den)

  if (nrow(new) == 0 || nrow(new) != nrow(num)) {
    stop("add_ratio_item(): '", numerator, "' and '", denominator,
         "' do not have the same regions and years.")
  }
  bind_rows(obs, new)
}


#' Add the share of the State total: 100 * region value / State value
#'
#' The shares get a new measure name: "val" becomes "val_pct_of_state".
#'
#' @param obs Observations.
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


#' Add the change from the previous year
#'
#' Two new measures for `year`: "val" becomes "val_change" (value now less
#' value in year - 1) and "val_pct_change" (the change as a % of year - 1).
#'
#' @param obs  Observations.
#' @param year The year of the change.
#' @return obs with the change rows added.
add_change <- function(obs, year) {
  now  <- obs[obs$year == year, ]
  prev <- obs[obs$year == year - 1, ] |> select(measure, level, region, item, prev = value)

  both <- inner_join(now, prev, by = c("measure", "level", "region", "item"))

  bind_rows(
    obs,
    both |> mutate(measure = paste0(measure, "_change"),     value = value - prev),
    both |> mutate(measure = paste0(measure, "_pct_change"), value = 100 * (value - prev) / prev)
  ) |>
    select(-prev)
}


# ---- Panels ---------------------------------------------------------------------

#' Make one panel of a release table
#'
#' @param obs   Observations.
#' @param title Title row text.
#' @param rows  One row for each table row. Columns: label (text in column A),
#'   bold (TRUE for a total row), and one or more keys (obs column names, for
#'   example "region" or "item"). If a key is NA, the row has no numbers
#'   (an empty row or a heading row).
#' @param cols  One row for each value column. Columns: group (header text
#'   above a group of columns, "" for none), label (column header text),
#'   format (Excel number format), and one or more keys (for example "year").
#'   In the header text, "\n" is a line break.
#' @param stub_header Header text above column A.
#' @param unit   Unit text in the top-right cell, or NULL for none.
#' @param spacer TRUE: one empty column between two groups.
#' @return A panel (list). Stops if a cell has no value or more than one value.
build_panel <- function(obs, title, rows, cols,
                        stub_header = "", unit = "€m", spacer = TRUE) {

  if (!all(vapply(rows, is.atomic, logical(1))) || !all(vapply(cols, is.atomic, logical(1)))) {
    stop(title, ": 'rows' and 'cols' must be flat tables (no nested columns).")
  }
  row_keys <- setdiff(names(rows), c("label", "bold"))
  col_keys <- setdiff(names(cols), c("group", "label", "format"))

  # Excel column of each value column. Column 1 (A) has the row labels.
  # A spacer column comes before each new group.
  new_group <- c(FALSE, cols$group[-1] != cols$group[-nrow(cols)])
  sheet_col <- 1 + seq_len(nrow(cols)) + if (spacer) cumsum(new_group) else 0

  values <- matrix(NA_real_, nrow = nrow(rows), ncol = nrow(cols))

  for (i in seq_len(nrow(rows))) {
    if (anyNA(rows[i, row_keys])) next  # empty row or heading row

    for (j in seq_len(nrow(cols))) {
      keys  <- c(as.list(rows[i, row_keys]), as.list(cols[j, col_keys]))
      match <- rep(TRUE, nrow(obs))
      for (k in names(keys)) match <- match & obs[[k]] == keys[[k]]

      v <- obs$value[match]
      if (length(v) != 1) {
        stop(sprintf("%s: expected 1 value for [%s]. Found %d.", title,
                     paste(names(keys), keys, sep = "=", collapse = ", "), length(v)))
      }
      values[i, j] <- v
    }
  }

  list(title = title, unit = unit, stub_header = stub_header,
       rows = rows[, c("label", "bold")], cols = cols[, c("group", "label", "format")],
       sheet_col = sheet_col, values = values)
}


#' Year header labels, with a footnote mark on revised years
#'
#' @param years   Years, for example 2021:2024.
#' @param revised Years that get the mark.
#' @param mark    The mark, for example "²" (superscript 2).
#' @return For example "2021²" "2022²" "2023²" "2024".
year_labels <- function(years, revised = integer(0), mark = "¹") {
  paste0(years, ifelse(years %in% revised, mark, ""))
}


# ---- Excel ----------------------------------------------------------------------

#' Write a release table to an Excel file
#'
#' @param tbl A release table, a list with:
#'   sheet_name   Sheet name. It is also the file name.
#'   panels       One or more panels, written one below the other.
#'   footnotes    Character vector, written below the last panel.
#'   label_width, value_width, spacer_width  Column widths.
#' @param out_dir Folder for the file. It is made if it does not exist.
#' @return The file path.
write_release_table <- function(tbl, out_dir) {

  wb <- createWorkbook()
  sh <- tbl$sheet_name
  addWorksheet(wb, sh, gridLines = FALSE)
  modifyBaseFont(wb, fontSize = 8, fontName = "Arial")

  bold   <- createStyle(textDecoration = "bold")
  right  <- createStyle(halign = "right")
  centre <- createStyle(halign = "center")
  wrap   <- createStyle(wrapText = TRUE)
  line   <- createStyle(border = "bottom")  # thin line below the cell
  style  <- function(st, rows, cols) {
    addStyle(wb, sh, st, rows = rows, cols = cols, gridExpand = TRUE, stack = TRUE)
  }
  # Row height for header text with line breaks: 13 points for each line.
  fit_height <- function(row, texts) {
    n_lines <- max(lengths(strsplit(texts, "\n")), 1)
    if (n_lines > 1) setRowHeights(wb, sh, rows = row, heights = 13 * n_lines)
  }

  r <- 1  # next free row

  for (p in tbl$panels) {
    last_col <- max(p$sheet_col)

    # Title row. The unit (if any) is in the last column.
    writeData(wb, sh, p$title, startRow = r, startCol = 1)
    if (is.null(p$unit)) {
      mergeCells(wb, sh, rows = r, cols = 1:last_col)
    } else {
      writeData(wb, sh, p$unit, startRow = r, startCol = last_col)
      mergeCells(wb, sh, rows = r, cols = 1:(last_col - 1))
      style(right, r, last_col)
    }
    style(bold, r, 1)
    style(line, r, 1:last_col)
    r <- r + 1

    # Header: an optional group row, then the column label row.
    has_group_row <- any(p$cols$group != "")
    header_rows   <- r:(r + has_group_row)
    writeData(wb, sh, p$stub_header, startRow = r, startCol = 1)
    if (has_group_row) mergeCells(wb, sh, rows = header_rows, cols = 1)

    if (has_group_row) {
      groups <- rle(p$cols$group)
      last   <- cumsum(groups$lengths)
      for (g in seq_along(groups$values)) {
        cols <- p$sheet_col[(last[g] - groups$lengths[g] + 1):last[g]]
        writeData(wb, sh, groups$values[g], startRow = r, startCol = cols[1])
        if (length(cols) > 1) {
          mergeCells(wb, sh, rows = r, cols = cols)
          style(line, r, cols)
        }
        style(centre, r, cols)
      }
      style(wrap, r, 1:last_col)
      fit_height(r, groups$values)
      r <- r + 1
    }

    for (j in seq_len(nrow(p$cols))) {
      writeData(wb, sh, p$cols$label[j], startRow = r, startCol = p$sheet_col[j])
    }
    style(right, r, p$sheet_col)
    style(wrap, r, 1:last_col)
    fit_height(r, p$cols$label)

    style(bold, header_rows, 1:last_col)
    style(line, r, 1:last_col)
    r <- r + 1

    # Body. NA values become empty cells.
    body_rows <- r:(r + nrow(p$rows) - 1)
    writeData(wb, sh, p$rows$label, startRow = r, startCol = 1)
    for (j in seq_len(nrow(p$cols))) {
      writeData(wb, sh, p$values[, j], startRow = r, startCol = p$sheet_col[j])
      style(createStyle(numFmt = p$cols$format[j]), body_rows, p$sheet_col[j])
    }
    if (any(p$rows$bold)) style(bold, body_rows[p$rows$bold], 1:last_col)
    style(line, max(body_rows), 1:last_col)

    r <- max(body_rows) + 2  # one empty row before the next panel
  }

  # Footnotes: one row each, directly below the last panel.
  r <- r - 1
  for (fn in tbl$footnotes) {
    writeData(wb, sh, fn, startRow = r, startCol = 1)
    mergeCells(wb, sh, rows = r, cols = 1:last_col)
    r <- r + 1
  }

  # Column widths: labels, values, and spacer columns.
  value_cols  <- tbl$panels[[1]]$sheet_col
  spacer_cols <- setdiff(2:last_col, value_cols)
  setColWidths(wb, sh, cols = 1, widths = tbl$label_width)
  setColWidths(wb, sh, cols = value_cols, widths = tbl$value_width)
  if (length(spacer_cols) > 0) {
    setColWidths(wb, sh, cols = spacer_cols, widths = tbl$spacer_width)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(out_dir, paste0(sh, ".xlsx"))
  saveWorkbook(wb, file, overwrite = TRUE)
  file
}
