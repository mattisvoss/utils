## Compare two EAA transmission workbooks (AMS_EAA_A template)
##
## LAYOUT
##
##   A4              the reference year
##
##   Row 6           block code, merged across the whole block
##                   (VAL_N, then VAL_N-1, ...)
##   Row 7           block description
##   Row 8           element code, merged across its own columns
##                   (PRD_PP, SUB, TAX, PRD_BP, ALI, ...)
##   Row 9           element description
##
##   Columns A-D     item code, description, unit code, unit description
##   Column E on     one group of columns for each element. The first
##                   column of the group holds the value. The others
##                   hold flags.
##
## HOW THE READER FINDS THE VALUE COLUMNS
##
##   Excel stores a merged cell's text in its top-left cell only. So in
##   row 8, before any filling, a non-empty cell marks the FIRST column
##   of an element group. That is the value column. Everything up to
##   the next non-empty cell belongs to the same element and holds
##   flags.
##
##   This means the reader does not need to know how many columns an
##   element covers. Three today, four next year, it still works.
##
## Run peek_eaa() on your own file before you trust the row numbers.

library(readxl)
library(dplyr)
library(tidyr)

## ------------------------------------------------------------------
## Look at the file
## ------------------------------------------------------------------

peek_eaa <- function(path, sheet = 1, rows = 14, cols = 14) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE,
                    col_types = "text", .name_repair = "minimal")
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  out <- raw[seq_len(min(rows, nrow(raw))),
             seq_len(min(cols, ncol(raw))), drop = FALSE]
  rownames(out) <- paste0("row", seq_len(nrow(out)))
  colnames(out) <- paste0("col", seq_len(ncol(out)))
  print(out, na.print = ".")
  invisible(out)
}

## ------------------------------------------------------------------
## Read one workbook into long format
## ------------------------------------------------------------------

read_eaa <- function(path,
                     sheet          = 1,
                     row_block      = 6,   # VAL_N, VAL_N-1
                     row_element    = 8,   # PRD_PP, SUB, ...
                     first_data_row = 10,  # first item code
                     first_data_col = 5,   # column E
                     year_cell      = c(4, 1),   # A4
                     ref_year       = NULL) {    # override if A4 is wrong

  ## Read everything as text. A value cell can hold a flag after a
  ## tilde, or the codes 'nd' or 'na'. If readxl guesses the column
  ## type, one such cell turns a whole column into NA without saying so.
  raw <- read_excel(path, sheet = sheet, col_names = FALSE,
                    col_types = "text", .name_repair = "minimal")
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)

  ## --- the reference year ------------------------------------------
  if (is.null(ref_year)) {
    cell <- as.character(raw[year_cell[1], year_cell[2]])
    ref_year <- suppressWarnings(as.integer(
      regmatches(cell, regexpr("[0-9]{4}", cell))))
    if (length(ref_year) != 1 || is.na(ref_year))
      stop("No four-digit year found in cell (row ", year_cell[1],
           ", col ", year_cell[2], "). It holds: '", cell,
           "'. Pass ref_year = ... instead.")
  }
  message(path, ": reference year ", ref_year)

  ## --- the headers --------------------------------------------------
  block_raw <- as.character(unlist(raw[row_block,   ]))
  elem_raw  <- as.character(unlist(raw[row_element, ]))

  ## Ignore anything to the left of the data columns.
  block_raw[seq_len(first_data_col - 1)] <- NA
  elem_raw[ seq_len(first_data_col - 1)] <- NA

  ## A non-empty element cell marks the start of a group.
  value_cols <- which(!is.na(elem_raw) & nzchar(trimws(elem_raw)))
  if (length(value_cols) == 0)
    stop("No element codes found in row ", row_element,
         ". Check row_element and first_data_col with peek_eaa().")

  block   <- fill_right(block_raw)
  element <- trimws(elem_raw)

  ## Report the group widths. They should all be the same.
  ends   <- c(value_cols[-1] - 1, ncol(raw))
  widths <- ends - value_cols + 1
  if (length(unique(widths)) > 1)
    warning("Element groups are not all the same width. Widths seen: ",
            paste(sort(unique(widths)), collapse = ", "),
            ". Check the header rows with peek_eaa().")

  ## --- the item rows -------------------------------------------------
  body <- raw[first_data_row:nrow(raw), , drop = FALSE]
  code <- trimws(as.character(body[[1]]))
  keep <- !is.na(code) & nzchar(code)
  body <- body[keep, , drop = FALSE]

  item <- data.frame(
    code        = trimws(as.character(body[[1]])),
    description = trimws(as.character(body[[2]])),
    unit        = trimws(as.character(body[[3]])),
    stringsAsFactors = FALSE)

  ## --- one block of rows for each element ----------------------------
  pieces <- lapply(seq_along(value_cols), function(k) {

    j       <- value_cols[k]
    flag_js <- setdiff(seq(j, ends[k]), j)

    flag_txt <- rep(NA_character_, nrow(body))
    if (length(flag_js) > 0) {
      f <- body[, flag_js, drop = FALSE]
      f[] <- lapply(f, function(x) trimws(as.character(x)))
      flag_txt <- apply(f, 1, function(r) {
        r <- r[!is.na(r) & nzchar(r)]
        if (length(r) == 0) NA_character_ else paste(r, collapse = ",")
      })
    }

    data.frame(item,
               block     = block[j],
               element   = element[j],
               raw_value = trimws(as.character(body[[j]])),
               flag_col  = flag_txt,
               stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, pieces)

  ## --- tidy the values ------------------------------------------------
  out |>
    mutate(
      ## a flag can also sit inside the value cell, after a tilde
      flag_inline = ifelse(grepl("~", raw_value, fixed = TRUE),
                           sub("^.*~", "", raw_value), NA_character_),
      stem  = sub("~.*$", "", raw_value),
      value = suppressWarnings(as.numeric(stem)),
      ## 'nd' and 'na' are Eurostat text codes, not numbers
      text  = ifelse(is.na(value) & !is.na(stem) & nzchar(stem),
                     stem, NA_character_),
      flag  = paste_drop_na(flag_inline, flag_col),
      year  = ref_year - block_offset(block)) |>
    filter(!is.na(year)) |>
    select(code, description, unit, block, element, year, value, text, flag)
}

## VAL_N -> 0, VAL_N-1 -> 1, VAL_N-2 -> 2, anything else -> NA
block_offset <- function(x) {
  x   <- toupper(trimws(x))
  out <- rep(NA_integer_, length(x))
  out[grepl("^VAL_N$", x)] <- 0L
  lag <- grepl("^VAL_N[[:space:]]*-[[:space:]]*[0-9]+$", x)
  out[lag] <- as.integer(gsub("[^0-9]", "", x[lag]))
  out
}

## Carry a merged cell's text across the columns it covers.
fill_right <- function(x) {
  x[!is.na(x) & x == "NA"] <- NA
  for (i in seq_along(x)) if (is.na(x[i]) && i > 1) x[i] <- x[i - 1]
  x
}

paste_drop_na <- function(a, b) {
  out <- mapply(function(x, y) {
    v <- c(x, y)
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v) == 0) NA_character_ else paste(unique(v), collapse = ",")
  }, a, b, USE.NAMES = FALSE)
  as.character(out)
}

## ------------------------------------------------------------------
## Compare two files
## ------------------------------------------------------------------

compare_eaa <- function(path_a, path_b,
                        name_a = "excel", name_b = "r_pipeline",
                        tol_abs = 0.005, tol_rel = 1e-6, ...) {

  a <- read_eaa(path_a, ...)
  b <- read_eaa(path_b, ...)

  full_join(a, b, by = c("code", "element", "year"),
            suffix = c("_a", "_b")) |>
    mutate(
      description = coalesce(description_a, description_b),
      diff     = value_b - value_a,
      rel_diff = ifelse(is.na(value_a) | value_a == 0, NA_real_, diff / value_a),
      status = case_when(
        is.na(value_a) & is.na(value_b) &
          is.na(text_a) & is.na(text_b)              ~ "both empty",
        is.na(value_a) & !is.na(value_b)             ~ paste("only in", name_b),
        !is.na(value_a) & is.na(value_b)             ~ paste("only in", name_a),
        !is.na(text_a) | !is.na(text_b)              ~ "text value",
        abs(diff) <= tol_abs                         ~ "same",
        !is.na(rel_diff) & abs(rel_diff) <= tol_rel  ~ "same",
        TRUE                                         ~ "DIFFERENT")) |>
    select(code, description, element, year,
           value_a, value_b, diff, rel_diff, flag_a, flag_b, status) |>
    arrange(code, year, element)
}

## ------------------------------------------------------------------
## Internal check, on one file at a time
##
## This does not compare the two systems. It tests whether each one is
## internally consistent, which usually shows which is wrong when they
## disagree.
## ------------------------------------------------------------------

check_basic_price <- function(x, tol = 0.005) {
  x |>
    filter(element %in% c("PRD_PP", "SUB", "TAX", "PRD_BP")) |>
    select(code, description, year, element, value) |>
    pivot_wider(names_from = element, values_from = value) |>
    mutate(expected = PRD_PP + coalesce(SUB, 0) - coalesce(TAX, 0),
           gap = PRD_BP - expected) |>
    filter(!is.na(gap), abs(gap) > tol) |>
    arrange(desc(abs(gap)))
}

## ------------------------------------------------------------------
## Use
## ------------------------------------------------------------------

if (FALSE) {

  ## 1. Check the layout.
  peek_eaa("excel_AMS_EAA_A_2025.xlsx")

  ## 2. Read one file and look at what came out.
  x <- read_eaa("excel_AMS_EAA_A_2025.xlsx")
  count(x, block, year)      # VAL_N -> 2025, VAL_N-1 -> 2024
  count(x, element)
  sum(is.na(x$value) & is.na(x$text))   # empty cells

  ## 3. Compare.
  cmp <- compare_eaa("excel_AMS_EAA_A_2025.xlsx",
                     "r_AMS_EAA_A_2025.xlsx")

  count(cmp, status, sort = TRUE)

  cmp |> filter(status == "DIFFERENT") |>
    arrange(desc(abs(diff))) |> print(n = 50)

  cmp |> filter(status == "DIFFERENT") |> count(year, element, sort = TRUE)

  cmp |> filter(grepl("^only in", status))

  ## 4. Internal consistency.
  check_basic_price(read_eaa("r_AMS_EAA_A_2025.xlsx"))
  check_basic_price(read_eaa("excel_AMS_EAA_A_2025.xlsx"))
}
