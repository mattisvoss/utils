# Eurostat regional accounts (REAA): reference

This part of the pipeline makes the regional transmission to Eurostat:
current prices (`AMS_EAACURR_A`) and previous-year prices (`AMS_EAACONR_A`),
for IE, IE0 and the three NUTS 2 regions.

```
regional line targets ──take()──►  regional_items  ──►  eaa_obs  ──►  SDMX-CSV files   (to Eurostat)
  (county level)                    (county level,       (NUTS 2 + State,  └──►  Excel files  (parallel run only)
                                     AM codes)            SDMX columns)
                         config/eaa_hierarchy.csv ──┘
                         config/eaa_items.csv ──────┘
```

PxStat and the release tables do not use this path yet. Later, PxStat can be
made from `regional_items` too (see "The plan" at the end).

---

## 1. Files

| File | Contents | Who changes it |
|---|---|---|
| `R/regional_items.R` | `take()` and `skip()`, and `assemble_regional_items()`: the map from the line targets to the Eurostat item codes | you, when a line target changes |
| `R/eaa_obs.R` | `make_eaa_obs()`, `write_eaa_sdmx()`, `write_eaa_excel()` | rarely |
| `config/eaa_hierarchy.csv` | how the items add up: `parent`, `child`, `sign` | almost never (the same for every country) |
| `config/eaa_items.csv` | your item table: which items are zero, non-significant, or not sent in CONR | the section, when Irish practice changes |
| `config/templates/AMS_EAACURR_A_IE.xlsx`, `AMS_EAACONR_A_IE.xlsx` | the empty Eurostat Excel templates | only when Eurostat sends new templates |

---

## 2. Setup

### 2.1 Packages

Add `"dplyr"`, `"tidyr"`, `"tibble"`, `"readr"`, `"readxl"` and `"openxlsx"` to
`tar_option_set(packages = ...)`. `tidyr` must be version 1.2.0 or later.

### 2.2 Targets

```r
tar_target(regional_items, assemble_regional_items(
  cattle_levy_regional_value = cattle_levy_regional_value,
  # ... one line for each argument of assemble_regional_items()
)),

tar_target(eaa_hierarchy_file, "config/eaa_hierarchy.csv", format = "file"),
tar_target(eaa_items_file,     "config/eaa_items.csv",     format = "file"),
tar_target(eaa_obs, make_eaa_obs(regional_items, eaa_hierarchy_file, eaa_items_file)),

tar_target(eaa_files,
           write_eaa_sdmx(eaa_obs, years = 2022:2024, out_dir = "output/eurostat"),
           format = "file"),

tar_target(eaa_excel_files,                                   # parallel run only
           write_eaa_excel(eaa_obs, years = 2022:2024, template_dir = "config/templates",
                           out_dir = "output/eurostat_excel"),
           format = "file")
```

Name each line target in the `regional_items` command. Targets finds the
dependencies by reading the command. If a function reads a target with
`tar_read()`, targets cannot see the dependency, and `regional_items` does not
run again when that line target changes.

### 2.3 One time only: add `empty_in_conr` to the item table

> **Run this one time, then delete this section from the README.**

```r
items <- readr::read_csv("config/eaa_items.csv", col_types = readr::cols(.default = "c"))
not_in_conr <- c("AM290000", "AM300000", "AM310000", "AM320000", "AM330000", "AM340000",
                 "AM350000", "AM360000", "AM370000", "AM380000", "AM390000")
items$empty_in_conr <- ifelse(items$estat_item %in% not_in_conr, "Y", "N")
readr::write_csv(items, "config/eaa_items.csv")
```

---

## 3. The main ideas

### 3.1 Detailed items and calculated items

An item is one of two kinds:

- A **detailed item** is a number that you estimate: eggs, the cattle levy,
  interest received, seeds. Only detailed items go into `regional_items`.
- A **calculated item** is a total (cereals = wheat + barley + …) or a
  balancing item (GVA = output − intermediate consumption). It has no new
  information. `eaa_obs` calculates it from the hierarchy.

A sum cannot be undone: from a total, you cannot get its parts back. So the
source of an output must have at least the detail of the most detailed output.
Eurostat is the most detailed output, so `regional_items` is at the Eurostat
detail, and not at the PxStat detail.

### 3.2 Components (STAT_CHAR)

The 57 **output items** (AM180000 and all items below it) have four numbers:

| In `regional_items` | In SDMX (STAT_CHAR) | Meaning | Excel sheet |
|---|---|---|---|
| `"value"` | `PRD_PP` | value at producer prices | DATA_01 |
| `"subsidy"` | `SUB` | subsidies on products | DATA_02 |
| `"tax"` | `TAX` | taxes on products | DATA_03 |
| (calculated) | `PRD_BP` | value at basic prices = PRD_PP + SUB − TAX | DATA_04 |

The other 33 items (intermediate consumption, income items, capital account)
have only `PRD_BP`. In `regional_items` you give them as `"value"`.

The cattle levy is item AM111000 with component `"tax"`. The hierarchy adds it
into AM110000, AM130000 and up to AM180000, the same as the producer values.

### 3.3 The hierarchy

`config/eaa_hierarchy.csv` has one row for each part of a total:

```
parent,   child,    sign
AM260000, AM180000,  1      GVA = output
AM260000, AM200000, -1          - intermediate consumption
```

All EAA identities are sums and differences. So they are valid at any level:
"add up the counties, then calculate GVA" gives the same result as "calculate
GVA in each county, then add up". This is not true for ratios, shares or
indices.

The 87 rows cover all 90 Eurostat items. All 24 identities were checked on the
Eurostat example files. AM380000 and AM390000 have no parent: they are not
part of any total.

### 3.4 Missing is not zero

A detailed item with no row in `regional_items` becomes 0, but only if the
item table says that it is zero or non-significant. If the item table does not
say so, it is a **gap**, and `eaa_obs` stops. So a forgotten `take()` can never
become a 0 without a message.

### 3.5 Regions

`eaa_obs` adds up the counties into the NUTS 2 regions and into the State.
NUTS codes are hierarchical: IE041 (Border) is in IE04 (Northern and Western).
So the NUTS 2 code is the first four characters of the NUTS 3 code from your
`county_to_nuts()` and `nuts_to_code()`.

IE0 (NUTS 1) is the whole State, so it has the same values as IE.

---

## 4. How to fill in `regional_items`

One `take()` for each Eurostat item that a line target gives:

```r
take(subsidy_regional_value, product == "subsidy", sub_product == "cattle",
     item = "AM111000", component = "subsidy"),
```

| Argument | Meaning |
|---|---|
| first argument | the line target |
| conditions | which rows, as in `dplyr::filter()`. No condition = all rows. |
| `item` | the Eurostat item code |
| `component` | `"value"` (default), `"subsidy"` or `"tax"` |
| `add_up = TRUE` | add several rows into one item, for each county and year |
| `again = TRUE` | use rows that another `take()` already uses, for a second item |

`skip(target, conditions, reason = "...")` marks rows that are not facts, with
the reason.

### 4.1 The rules

1. **Every row of every line target is used exactly one time,** by one `take()`
   or one `skip()`. If a row is not used, or is used two times, the code stops.
   So nothing is lost and nothing is counted two times.
2. **Only detailed items.** Use the most detailed code that describes the
   number exactly. No totals, no balancing items, no zeros.
3. **Read the last long target of each line,** the last target that still has
   one row for each county and product. Do not read a target that feeds a
   target that you already read, or a target that is only a sum of others.
4. **`add_up = TRUE` in every `take()` for the item** when rows are added into
   one item, also from different line targets:
   ```r
   take(wool_regional_value,  product == "wool",  item = "AM129000", add_up = TRUE),
   take(honey_regional_value, product == "honey", item = "AM129000", add_up = TRUE),
   ```
5. **`again = TRUE` only for a value that the EAA records two times.** Contract
   work is estimated one time (as IC) and is also output of agricultural services:
   ```r
   take(total_intermediate_consumption_less_fisim_regional_value,
        product == "contract_work", item = "AM209100"),
   take(total_intermediate_consumption_less_fisim_regional_value,
        product == "contract_work", item = "AM150000", again = TRUE),
   ```

### 4.2 Conditions

| You want | Write |
|---|---|
| one value | `sub_product == "barley"` |
| several values | `sub_product %in% c("oilseed", "hemp")` |
| all except some | `sub_product != "beet", sub_product != "maize"` |

Careful with `!sub_product %in% c(...)`: it also selects rows where
`sub_product` is NA. The form with `!=` drops those rows, and then the code
stops because they are not used. That is the safer result.

A list (`%in%`) is closed: a new sub-product stops the code. An exclusion is
open: a new sub-product goes into the item without a message. Use an exclusion
only for a residual item, such as AM029000 "Other industrial crops".

---

## 5. What `make_eaa_obs()` does

| Step | What | Why |
|---|---|---|
| 1 | coverage check | every detailed item has a source, or is zero in the item table |
| 2 | counties → NUTS 2 and IE | `sum()` keeps NA, so a missing value stays visible |
| 3 | a 0 row for every detailed item without a row | zero items, and products without subsidy or tax |
| 4 | basic price = value + subsidy − tax | |
| 5 | totals and balancing items | each total when all its parts are known, repeated until all are known |
| 6 | keep the components that exist | 4 for output items, only basic for the others |
| 7 | item table rules | zero → 0; non-significant → 0 and flag N; not sent in CONR → NA |
| 8 | IE0 = IE | |
| 9 | the SDMX columns | |

### 5.1 The result: `eaa_obs`

`eaa_obs` has exactly the rows and columns of the SDMX files: the full grid of
5 regions × 261 (item, STAT_CHAR) pairs, for each year and dataflow.

| Column | Values |
|---|---|
| DATAFLOW | `ESTAT:AMS_EAACURR_A(1.0)` (val) or `ESTAT:AMS_EAACONR_A(1.0)` (val_pyp) |
| FREQ | `A` |
| REF_AREA | `IE`, `IE0`, `IE04`, `IE05`, `IE06` |
| AM_ITEM | the item code |
| STAT_CHAR | `PRD_PP`, `SUB`, `TAX`, `PRD_BP` |
| REFERENCE | `VAL_N` (CURR) or `VAL_N-1` (CONR) |
| LAND_TYPE | `_Z` |
| UNIT_MEASURE | `MIO_NAC` |
| TIME_PERIOD | the year |
| OBS_VALUE | the number; NA = not sent |
| OBS_STATUS | `N` for non-significant items, else empty |
| CONF_STATUS, OBS_COMMENT, UNIT_MULT | empty |
| OBS_PERIOD | `3009NP1` |
| DECIMALS | `7` |

---

## 6. The item table (`config/eaa_items.csv`)

| Column | Meaning |
|---|---|
| `estat_item` | the item code. All 90 items must be in the table. |
| `zero_for_IE` | `Y`: the item is 0 for Ireland |
| `non_significant` | `Y`: the item is 0 and gets the flag N |
| `empty_in_conr` | `Y`: the item is not sent at previous-year prices (NaN in CONR) |

Other columns, such as `estat_label`, are allowed and not used.

---

## 7. Writing the files

### 7.1 SDMX-CSV (to Eurostat): `write_eaa_sdmx()`

One file for each dataflow and year, for example
`AMS_EAACURR_A_IE_2024_0000_V0001.csv`. The format is the same as the Eurostat
example files:

- separator `;`, no quotes, no BOM, LF line endings
- every item × STAT_CHAR × region has a row
- zero is `0`; a value that is not sent is `NaN`
- up to 8 decimals, no scientific notation (the examples have up to 11
  decimals, even though DECIMALS is 7)

If you send a corrected file for the same dataflow and year, use `version = 2`.

### 7.2 Excel (parallel run only): `write_eaa_excel()`

Fills the Eurostat Excel templates, one file for each dataflow and year, so
that a person can compare them with the files of the old Excel system. The
code finds each cell by its item code (row 5) and its region code (column A,
from row 7). A value that is not sent stays an empty cell.

Do not send these files to Eurostat. `openxlsx` drops the SharePoint metadata
of the template when it saves the file. That does not matter for a check file.

---

## 8. Each year

1. Change `years` in the two writer targets (Ireland sends the three most
   recent years).
2. Check the item table: did a zero item get a value, or a new item appear?
3. Run `tar_make()`. Fix any stop (see section 9).
4. Compare the Excel files with the old system (parallel run), or with last
   year's transmission. Eurostat flags a change of more than 15% from the
   last transmission.
5. Check that the IE row agrees with the national EAA transmission, for
   current and previous-year prices. Eurostat checks this.

---

## 9. When the code stops

### 9.1 In `regional_items`

| Message | Cause | What to do |
|---|---|---|
| `X: 5 rows are not used (rows 46, ...)` | rows that no `take()` or `skip()` selects | add a `take()` or a `skip()` |
| `The rows above are selected by more than one take() or skip()` | two `take()` calls select the same rows | make the conditions more exact, or use `again = TRUE` |
| `no rows match the conditions for ...` | usually a spelling error in a condition | check the values with `dplyr::count(tar_read(X), product, sub_product)` |
| `More than one row gives the same item, component, county and year` | two rows for one item, without `add_up` | make the conditions more exact, or use `add_up = TRUE` in every `take()` for that item |
| `Unknown county: State` | a line target has a State row | `skip()` it, with the reason |
| `has no column: val_pyp` | the line target has no column with that name | check the line target |
| `The items above have NA in val` | a line target has NA at current prices | fix the line target |

### 9.2 In `eaa_obs`

| Message | Cause | What to do |
|---|---|---|
| `Coverage problems` with `GAP` | a detailed item has no source | add a `take()`, or mark the item zero in the item table |
| `ERROR: a total in regional_items` | a `take()` gives a total | map to the detailed items |
| `ERROR: zero in the item table, but regional_items has a value` | the item table and the data disagree | decide which is correct |
| `ERROR: not in the item table` | a code in a `take()` is not a Eurostat item | correct the code |
| `Subsidies and taxes on products can only be on output items` | `component = "subsidy"` or `"tax"` on, for example, an IC item | correct the item or the component |
| `These items have no value (NA)` | usually NA in `val_pyp` of a line target, for an item that is sent in CONR | fix the line target |
| `nuts_to_code() must give NUTS 3 codes such as IE041` | the function gives another form | check `nuts_to_code()` |

---

## 10. Things that the tests found

- `tidyr::complete(fill = ...)` also changes **existing** NA into the fill
  value, so a missing value disappears. `make_eaa_obs()` uses
  `explicit = FALSE`, which fills only the new rows.
- Inside `tibble()`, a column can use a column made before it in the same call.
  `tibble(source = name, n = nrow(source))` takes `nrow()` of the new text
  column, and the result disappears without an error.
- In the Excel template, cell A3 (the country) is also "IE". A search for the
  region "IE" in the whole of column A finds row 3, not row 7. The writer
  searches only from row 7.

---

## 11. Open questions

- Where the fed cereals (barley and others) are in the regional pipeline. They
  are not in the fodder table.
- How the State-only "all" capital transfers are allocated to counties.
- Does Eurostat want OBS_STATUS `N` in SDMX? The example files have no flags.

## 12. The plan

1. Now: the Eurostat path, from `regional_items`. PxStat stays as it is.
2. Next: make PxStat from `regional_items`, with a map from AM codes to ACA03
   statistics. When the result is exactly the current `px_stat_aca03` for all
   years, switch, and delete `mastersheet_output_regional`.
3. Later: the same detail in the national accounts, so that all outputs read
   the same data.
