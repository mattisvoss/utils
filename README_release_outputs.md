# Release outputs

These files make the release tables 1.1 to 1.4 and 2.1 to 2.7 from the PxStat
table ACA03 (long format). So the release and PxStat always show the same numbers.

| File | Contents |
|---|---|
| `R/release_tables.R` | The general part: PxStat table to observations, calculated values, panels, Excel writer. |
| `R/release_table_specs.R` | The tables: titles, footnotes, region names and order, column widths. |
| `config/release_items.csv` | The item lists: which statistic goes in which row or column, its label, and bold rows. |

## Setup

1. Put the two R files in the folder that `tar_source()` reads.
2. Put `release_items.csv` in `config/`.
3. Add `"dplyr"`, `"tidyr"`, `"tibble"`, `"readr"` and `"openxlsx"` to
   `tar_option_set(packages = ...)`.
4. Add these lines to `_targets.R`. `px_stat_aca03` is your PxStat target.

```r
# Above the target list:
release_year          <- 2024
release_revised_years <- 2021:2023   # Table 1.4 marks these years "Revised"
release_dir           <- "output/release"

# In the target list:
tar_target(release_items_file, "config/release_items.csv", format = "file"),
tar_target(release_items, read_release_items(release_items_file)),
tar_target(release_obs, make_release_obs(px_stat_aca03)),
tar_target(release_files,
           write_release_tables(release_obs, release_items, release_year,
                                release_revised_years, release_dir),
           format = "file")
```

`release_files` writes all 11 files. With `format = "file"`, targets writes them
again if one of them is changed or deleted.

## The tables

| Table | Rows | Columns | Years |
|---|---|---|---|
| 1.1 | NUTS 3 regions, State | items, each with 4 years | release year and 3 before |
| 1.2 | items | NUTS 3 regions, State | release year |
| 1.3 | regions: value and % of State | items | release year |
| 1.4 | regions: Net Subsidies, Operating Surplus, % | years | release year and 3 before |
| 2.1 to 2.7 | items (one table for each NUTS 3 region) | 4 years, change, % of State | release year and 3 before |

## Keys

- **Item:** the PxStat statistic code, for example `ACA03C33`. The PxStat texts are
  in `config/pxstat_codes_regionals_ACA03.csv`. Two items are calculated in
  `make_release_obs()`: `NET_SUBSIDIES` and `NET_SUBSIDIES_PCT_OS`.
- **Region:** the PxStat region code. `-` is State, `IE04` is NUTS 2, `IE041` is
  NUTS 3. The names that the release shows are in `release_regions` in
  `release_table_specs.R`.

## How to change a table

Most changes are in `config/release_items.csv`. The rows are in table order.

| Column | Contents |
|---|---|
| `table` | `1.1`, `1.2`, `1.3`, `1.4`, or `2.x` (all of Tables 2.1 to 2.7) |
| `label` | Text in the table. `\n` is a line break. |
| `item` | PxStat statistic code. |
| `bold` | `TRUE` for a total row. Empty means not bold. |
| `space_before` | `TRUE` puts an empty row before this row (Table 1.2). Empty means no empty row. |

To edit the CSV in Excel, save it as **CSV UTF-8**. A plain "CSV" save changes
the superscripts (¹ ²) to other characters.

Titles, footnotes and column widths are in `R/release_table_specs.R`.

## If the code stops

The code stops with a message when the data is wrong. It does not write a table
with missing numbers.

| Message | Cause |
|---|---|
| `no year or no numeric value` | A `VALUE` is empty or not a number. |
| `more than one value for the same statistic, year and region` | The PxStat table has duplicate rows. |
| `Unknown region code` | A region code is not `-`, `IE` + 2 digits, or `IE` + 3 or more digits. |
| `expected 1 value for [...]` | A statistic, region or year is not in the PxStat table, or a code in the CSV is wrong. |
| `the columns must include ...` | The CSV header is wrong. |
| `each row must have a table, a label and an item` | A row in the CSV is not complete. |
