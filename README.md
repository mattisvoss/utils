# Regional outputs

These files make the release tables 1.1, 1.2 and 1.4 from `mastersheet_output_regional`.

```
mastersheet_output_regional  →  release_obs  →  release_file_1_1
   (county level)               (regions)    →  release_file_1_2
                                             →  release_file_1_4
```

## Files

| File | Contents |
|---|---|
| `R/regional_obs.R` | Adds up counties to NUTS 3, NUTS 2 and State. PxStat and Eurostat will also use this. |
| `R/release_tables.R` | Makes panels and writes Excel files. It is the same for all tables. |
| `R/release_table_specs.R` | The rows, columns and text of each table. |
| `targets_release_snippet.R` | Lines to add to `_targets.R`. |

## Setup

1. Put the three R files in the folder that `tar_source()` reads.
2. Add `"dplyr"`, `"tidyr"`, `"tibble"` and `"openxlsx"` to `tar_option_set(packages = ...)`.
3. Add the lines from `targets_release_snippet.R` to `_targets.R`.
4. Run `targets::tar_make()`. The files go to `output/release/`.

The code uses your function `county_to_nuts()`. The function `nuts3_to_nuts2()` is in
`R/regional_obs.R`.

## Keys

- **Item key:** the `px_stat_label` column of the mastersheet, for example
  `"Forage Plants - Consumption"`. It must give one value for each county,
  item and year.
- **Region key:** the name that `county_to_nuts(..., mid_east_dub = TRUE)`
  gives, for example `"Dublin & Mid-East"`. `nuts3_regions` in
  `release_table_specs.R` must use the same names.

## Each year

Change `release_years` and `release_revised_years` in `_targets.R`.

## How to change a table

- **Change a row label:** edit `label` in the row list. Do not edit `item`.
- **Add a row:** add a line to the row list, with the PxStat label as `item`.
- **Add a calculated item:** add an `add_sum_item()` or `add_ratio_item()`
  line to `make_release_obs()`.

## If the code stops

The code stops with a message when the data is wrong. It does not write a
table with missing numbers. The usual causes are:

| Message | Cause |
|---|---|
| `More than one value for the same county, item and year` | The item key is not unique. |
| `unknown NUTS 3 region` | `county_to_nuts()` gave a name that `nuts3_to_nuts2()` does not know. |
| `expected 1 value for [...]` | An item, region or year is missing, or its name is different in the specs. |
