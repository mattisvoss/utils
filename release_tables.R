# release_tables.R
#
# Regional observations  -->  panels  -->  Excel file.
#
# A release table is a list:
#   sheet_name   Excel sheet name. It is also the file name.
#   panels       One or more panels. They are written one below the other.
#   footnotes    Character vector. Written below the last panel.
#   label_width  Width of column A.
#   value_width  Width of the value columns.
#
# A panel is one block of the table: a title row, one or two header rows,
# and a body of numbers. Each body cell is exactly one value in obs.
#
# Column layout. Column A has the row labels. Then each column group has
# one column for each year. Usually there is one empty spacer column between
# groups. Table 1.3 has one year and no spacer columns.
#
#   A        | 2021 2022 2023 2024 | spacer | 2021 2022 2023 2024 | ...
#   label    |      group 1        |        |      group 2        |


#' Excel column number of a value cell
#'
#' @param group    Group number (1 if the panel has no groups).
#' @param year_pos Position of the year in the panel (1, 2, ...).
#' @param n_years  Number of years in the panel.
#' @param spacer   TRUE if there is an empty column between groups.
#' @return Column number. Column 1 is column A.
sheet_col <- function(group, year_pos, n_years, spacer = TRUE) {
  1 + (group - 1) * (n_years + spacer) + year_pos
}


#' Make one panel of a release table
#'
#' @param obs    Regional observations.
#' @param title  Title row text.
#' @param rows   Tibble with one row for each table row. Columns:
#'   label (text in column A), bold (TRUE for a total row), and one or more
#'   key columns with the same names as obs columns ("region", "item").
#'   If a key is NA, the row has no numbers (an empty row or a heading row).
#' @param years        Years to show, for example 2021:2024.
#' @param year_labels  Year header text. Use year_labels() to add footnote marks.
#'   NULL: no year header row (for a table with one year, such as Table 1.3).
#' @param groups       Values of the column groups, or NULL for no groups.
#' @param group_by     obs column that the groups come from ("region" or "item").
#' @param group_labels Group header text. The default is the group values.
#'   Use "\n" for a line break.
#' @param spacer       TRUE: one empty column between groups.
#' @param stub_header  Header text above column A.
#' @param unit         Unit text in the top-right cell.
#' @param number_format Excel number format of the body cells.
#' @return A panel (list). Stops if a cell has no value or more than one value.
build_panel <- function(obs, title, rows, years,
                        year_labels   = as.character(years),
                        groups        = NULL,
                        group_by      = NULL,
                        group_labels  = groups,
                        spacer        = TRUE,
                        stub_header   = "",
                        unit          = "\u20acm",
                        number_format = "#,##0.0") {

  if (!all(vapply(rows, is.atomic, logical(1)))) {
    stop(title, ": 'rows' must be a flat table (no nested columns).")
  }
  key_cols <- setdiff(names(rows), c("label", "bold"))
  n_years  <- length(years)
  n_groups <- max(length(groups), 1)

  # The matrix starts at column B, so matrix column = sheet column - 1.
  values <- matrix(NA_real_, nrow = nrow(rows), ncol = sheet_col(n_groups, n_years, n_years, spacer) - 1)

  for (g in seq_len(n_groups)) {
    obs_g <- if (is.null(groups)) obs else obs[obs[[group_by]] == groups[g], ]

    for (i in seq_len(nrow(rows))) {
      keys <- rows[i, key_cols]
      if (anyNA(keys)) next  # empty row or heading row

      in_row <- rep(TRUE, nrow(obs_g))
      for (k in key_cols) in_row <- in_row & obs_g[[k]] == keys[[k]]

      for (j in seq_len(n_years)) {
        v <- obs_g$value[in_row & obs_g$year == years[j]]
        if (length(v) != 1) {
          stop(sprintf("%s: expected 1 value for [%s], group '%s', year %d. Found %d.",
                       title, paste(unlist(keys), collapse = " / "),
                       if (is.null(groups)) "-" else groups[g], years[j], length(v)))
        }
        values[i, sheet_col(g, j, n_years, spacer) - 1] <- v
      }
    }
  }

  list(title = title, unit = unit, stub_header = stub_header,
       group_labels = group_labels, year_labels = year_labels, n_years = n_years,
       spacer = spacer,
       row_labels = rows$label, row_bold = rows$bold,
       values = values, number_format = number_format)
}


#' Write a release table to an Excel file
#'
#' @param tbl     A release table (see the top of this file).
#' @param out_dir Folder for the file. It is made if it does not exist.
#' @return The file path. A target with format = "file" uses this.
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

  r <- 1  # next free row

  for (p in tbl$panels) {
    last_col <- 1 + ncol(p$values)
    n_groups <- length(p$group_labels)  # 0 if the panel has no groups

    # Title row: title on the left, unit on the right.
    writeData(wb, sh, p$title, startRow = r, startCol = 1)
    writeData(wb, sh, p$unit,  startRow = r, startCol = last_col)
    mergeCells(wb, sh, rows = r, cols = 1:(last_col - 1))
    style(bold, r, 1)
    style(right, r, last_col)
    style(line, r, 1:last_col)
    r <- r + 1

    # Header: an optional group row, then an optional year row.
    # The stub header is in column A, over both rows if there are two.
    has_group_row <- n_groups > 0
    has_year_row  <- !is.null(p$year_labels)
    header_rows   <- r:(r + has_group_row + has_year_row - 1)
    writeData(wb, sh, p$stub_header, startRow = r, startCol = 1)
    if (length(header_rows) == 2) mergeCells(wb, sh, rows = header_rows, cols = 1)

    if (has_group_row) {
      # A group with one column is right-aligned, like the numbers below it.
      group_align <- if (p$n_years == 1) right else centre
      for (g in seq_len(n_groups)) {
        cols <- sheet_col(g, 1, p$n_years, p$spacer):sheet_col(g, p$n_years, p$n_years, p$spacer)
        writeData(wb, sh, p$group_labels[g], startRow = r, startCol = cols[1])
        if (length(cols) > 1) mergeCells(wb, sh, rows = r, cols = cols)
        style(group_align, r, cols)
        style(line, r, cols)
      }
      style(wrap, r, 1:last_col)
      n_lines <- max(lengths(strsplit(p$group_labels, "\n")))
      if (n_lines > 1) setRowHeights(wb, sh, rows = r, heights = 13 * n_lines)
      r <- r + 1
    }

    if (has_year_row) {
      for (g in seq_len(max(n_groups, 1))) {
        writeData(wb, sh, as.data.frame(t(p$year_labels)), colNames = FALSE,
                  startRow = r, startCol = sheet_col(g, 1, p$n_years, p$spacer))
      }
      style(right, r, 2:last_col)
      r <- r + 1
    }

    style(bold, header_rows, 1:last_col)
    style(line, max(header_rows), 1:last_col)

    # Body. NA values become empty cells.
    body_rows <- r:(r + nrow(p$values) - 1)
    writeData(wb, sh, p$row_labels, startRow = r, startCol = 1)
    writeData(wb, sh, as.data.frame(p$values), startRow = r, startCol = 2, colNames = FALSE)
    style(createStyle(numFmt = p$number_format), body_rows, 2:last_col)
    if (any(p$row_bold)) style(bold, body_rows[p$row_bold], 1:last_col)
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

  # Column widths: labels, values, and narrow spacer columns.
  setColWidths(wb, sh, cols = 1, widths = tbl$label_width)
  setColWidths(wb, sh, cols = 2:last_col, widths = tbl$value_width)
  p1 <- tbl$panels[[1]]
  n_groups <- length(p1$group_labels)
  if (p1$spacer && n_groups > 1) {
    spacer_cols <- sheet_col(1:(n_groups - 1), p1$n_years, p1$n_years) + 1
    setColWidths(wb, sh, cols = spacer_cols, widths = 3.5)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(out_dir, paste0(sh, ".xlsx"))
  saveWorkbook(wb, file, overwrite = TRUE)
  file
}


#' Year header labels, with a footnote mark on revised years
#'
#' @param years   Years, for example 2021:2024.
#' @param revised Years that get the mark.
#' @param mark    The mark. "\u00b9" is a superscript 1.
#' @return Character vector, for example "2021¹" "2022¹" "2023¹" "2024".
year_labels <- function(years, revised = integer(0), mark = "\u00b9") {
  paste0(years, ifelse(years %in% revised, mark, ""))
}
