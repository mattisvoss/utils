# Release outputs

These files make the release tables 1.1, 1.2, 1.3 and 1.4 from `mastersheet_output_regional`.

```
mastersheet_output_regional  →  release_obs  ─┐
specs/release_items.csv      →  release_items ─┴→  release_file_1_1 ... release_file_1_4
```

## Files

| File | Contents |
|---|---|
| `R/regional_obs.R` | Adds up counties to NUTS 3, NUTS 2 and State. PxStat and Eurostat will also use this. |
| `R/release_tables.R` | Makes panels and writes Excel files. It is the same for all tables. |
| `R/release_table_specs.R` | Table layout: titles, footnotes, region order, column widths. |
| `specs/release_items.csv` | Item lists: which PxStat item goes in which row or column, its label, and bold rows. |
| `targets_release_snippet.R` | Lines to add to `_targets.R`. |

## Setup

1. Put the three R files in the folder that `tar_source()` reads.
2. Put `release_items.csv` in `specs/`.
3. Add `"dplyr"`, `"tidyr"`, `"tibble"`, `"readr"` and `"openxlsx"` to `tar_option_set(packages = ...)`.
4. Add the lines from `targets_release_snippet.R` to `_targets.R`.
5. Run `targets::tar_make()`. The files go to `output/release/`.

The code uses your function `county_to_nuts()`. The function `nuts3_to_nuts2()` is in
`R/regional_obs.R`.

## Keys

- **Item key:** the `px_stat_label` column of the mastersheet, for example
  `"Forage Plants - Consumption"`. It must give one value for each county,
  item and year. The calculated items `"Net Subsidies"` and
  `"Net Subsidies as a % of Operating Surplus"` are made in `make_release_obs()`.
- **Region key:** the name that `county_to_nuts(..., mid_east_dub = TRUE)`
  gives, for example `"Dublin & Mid-East"`. `nuts3_regions` in
  `release_table_specs.R` must use the same names.

## Each year

Change `release_years` and `release_revised_years` in `_targets.R`.
Table 1.3 shows only the last year of `release_years`.

## How to change a table

Most changes are in `specs/release_items.csv`. The rows are in table order.

| Column | Contents |
|---|---|
| `table` | `1.1`, `1.2`, `1.3` or `1.4` |
| `label` | Text in the table. `\n` is a line break. Empty for an empty row. |
| `item` | PxStat statistic label. Empty for an empty row. |
| `bold` | `TRUE` for a total row. Empty means not bold. |

- **Change a label:** edit `label`. Do not edit `item`.
- **Add a row or column:** add a line in the correct position, with the PxStat label as `item`.
- **Add a calculated item:** add an `add_sum_item()` or `add_ratio_item()`
  line to `make_release_obs()`. Then use its name as `item` in the CSV.

To edit the CSV in Excel, save it as **CSV UTF-8**. A plain "CSV" save
changes the superscripts (¹ ² ³) to other characters.

Titles, footnotes and column widths are in `R/release_table_specs.R`.

## If the code stops

The code stops with a message when the data is wrong. It does not write a
table with missing numbers. The usual causes are:

| Message | Cause |
|---|---|
| `More than one value for the same county, item and year` | The item key is not unique. |
| `unknown NUTS 3 region` | `county_to_nuts()` gave a name that `nuts3_to_nuts2()` does not know. |
| `expected 1 value for [...]` | An item, region or year is missing, or its name is different in the CSV or the specs. |
| `the columns must be table, label, item, bold` | The CSV header is wrong. |
