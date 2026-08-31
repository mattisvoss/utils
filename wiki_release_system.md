# The release system

This page describes how the pipelines produce a set of figures that can be
reproduced exactly, months or years later.

---

## 1. The problem it solves

The pipelines read data from a SQL Server database. Other sections load
data into that database, and they revise it. So the data changes under us.

Before this system, running a pipeline in March and again in September
could give different answers, and there was no way to find out why, or to
get the March answer back. Once a figure was published, we could not
reproduce it.

The rule this system enforces is:

> We can run the pipelines again at any time and get the numbers we
> published.

---

## 2. Two kinds of date

Two different dates appear in this system. They are easy to confuse, so it
is worth being clear about them.

**The reference period** is what the number describes. It is held in the
`period` and `year_concerned` columns. A milk price for 2023 has a
reference period of 2023.

**The system date** is when we knew the number. It is held in the
`SysStart` and `SysEnd` columns, and SQL Server sets it automatically. If
somebody loads a corrected 2023 milk price today, the reference period is
still 2023, but the system date is today.

A revision is a change to a number for the same reference period, at a
later system date.

---

## 3. How the database keeps history

Every data table in the database is a **system-versioned temporal table**.
This is a feature of SQL Server. It means the database keeps every version
of every row, automatically.

Each table has a partner table with the same name plus `_history`. For
example, `Milk_accounts` has `Milk_accounts_history`.

When a row is changed or deleted:

- The old version is moved to the history table, and its `SysEnd` is set
  to the moment of the change.
- The new version stays in the main table, with `SysStart` set to the same
  moment.

Nothing is ever lost. The main table always holds the current version, and
the history table holds every earlier one.

### The period columns

Each table has two extra columns:

| Column | Meaning |
|---|---|
| `SysStart` | when this version of the row became current |
| `SysEnd` | when it stopped being current |

Three things to know about them:

- They are marked `HIDDEN`, so they do not appear in `SELECT *`. This
  means R code that reads a whole table is not affected by them.
- The values are in **UTC**, not Irish time. In summer this is one hour
  behind the clock on the wall.
- SQL Server sets them from the time the **transaction** started. So every
  row written in one transaction gets the same value. This is what makes
  a load appear as a single moment rather than as a smear of times.

### Reading the database at a past moment

```sql
SELECT * FROM dbo.Milk_accounts
FOR SYSTEM_TIME AS OF '2026-06-25T10:47:03.1234567';
```

This returns the rows as they were at that moment. Rows loaded later are
not included. Rows that have since been revised come back in their old
form.

---

## 4. The release lookup table

A timestamp with seven decimal places is not something anybody wants to
remember or type. So each important moment gets a name.

```sql
CREATE TABLE dbo.Release_lookup (
  release_name varchar(50)   PRIMARY KEY,
  as_of_utc    datetime2(7)  NOT NULL,
  is_release   bit           NOT NULL DEFAULT 1,
  note         nvarchar(400) NULL
);
```

Example contents:

| release_name | as_of_utc | is_release | note |
|---|---|---|---|
| current | 9999-12-31 23:59:59.9999998 | 0 | the newest data |
| oiia_2025_advance | 2025-12-10 14:02:11.4471120 | 1 | Advance estimate 2025 |
| oiia_2025_final | 2026-06-25 10:47:03.1234567 | 1 | Final estimate 2025 |

The table does one thing: it turns a name into a moment.

### The row called `current`

`current` is the name used for everyday work. Its timestamp is one tick
below the maximum value, which means "the newest version of every row".

It has to be one tick below rather than the maximum itself, because
`AS OF` matches rows where `SysEnd` is **greater than** the value given. A
current row has `SysEnd` set to the maximum, and the maximum is not
greater than itself. Using the maximum would return nothing at all.

`current` has `is_release = 0`. It must never be published.

### Adding a release

The `publish_release()` function does this. It inserts the row using the
database clock, before any pipeline runs:

```sql
INSERT INTO dbo.Release_lookup (release_name, as_of_utc, is_release, note)
VALUES ('oiia_2025_final', SYSUTCDATETIME(), 1, 'Final estimate 2025');
```

Two details matter here:

- The timestamp comes from `SYSUTCDATETIME()`, the database's own clock.
  It must match the clock that sets `SysStart`. A time taken from a
  workstation could be a few seconds different, and would then read the
  wrong version of a row.
- The row is added **before** the pipelines run, not after. If it were
  added after, data loaded during the run would be inside some results and
  outside others.

Once a release row exists, it is never changed. That name means that
moment for ever.

---

## 5. How the pipelines use it

### The two targets at the top

Every pipeline has these two targets:

```r
tar_target(release_name, Sys.getenv("RELEASE", "current"),
           cue = tar_cue(mode = "always")),

tar_target(as_of, lookup_release(con(), release_name),
           cue = tar_cue(mode = "always"))
```

`release_name` is the name. `as_of` is the timestamp it resolves to.

`cue = tar_cue(mode = "always")` makes these two targets run on every
`tar_make()`. This is necessary. `targets` decides whether to rebuild a
target by looking at the written code, not at what the code produced last
time. The line `Sys.getenv("RELEASE", "current")` looks identical whatever
the environment variable holds. Without the cue, `targets` would skip the
target and quietly keep using the old release.

The two targets are cheap, so running them every time costs nothing.

### Every database read

Every read goes through one helper:

```r
tbl_as_of <- function(con, table, as_of, schema = "dbo") {
  stopifnot(grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?$", as_of))
  q <- sprintf("SELECT * FROM %s.%s FOR SYSTEM_TIME AS OF '%s'",
               DBI::dbQuoteIdentifier(con, schema),
               DBI::dbQuoteIdentifier(con, table),
               as_of)
  dplyr::tbl(con, dbplyr::sql(q))
}
```

The result is still lazy, so any `filter` or `select` after it is still
done by the database.

An input function changes from this:

```r
get_sheep_prices <- function(con) {
  dplyr::tbl(con, "Prices_accounts") |>
    dplyr::filter(animal == "sheep") |>
    dplyr::collect()
}
```

to this:

```r
get_sheep_prices <- function(con, as_of) {
  tbl_as_of(con, "Prices_accounts", as_of) |>
    dplyr::filter(animal == "sheep") |>
    dplyr::collect()
}
```

One new argument, one changed first line. Everything after is the same.

In the pipeline:

```r
tar_target(sheep_prices, get_sheep_prices(con(), as_of))
```

`as_of` appears in the target's code, so `targets` knows this target
depends on it. That is what makes the whole thing work.

### Two things not to do

**Do not hide `as_of` in a global variable or inside the connection.** It
looks tidier and it breaks silently. `targets` reads the code of each
target to find its dependencies. If `as_of` is not written there,
`targets` will not rebuild the target when the release changes.

**Do not convert `as_of` to a date or time in R.** It must stay a
character string from the moment it leaves the database. R holds times as
a decimal number, which cannot represent every value of `datetime2(7)`
exactly. A converted value can come back a fraction of a second early,
which would read the state of the table just before a load committed. The
answers would be slightly wrong and nothing would report an error.

### Finding any read that was missed

Every read of a data table must go through `tbl_as_of`. One read that does
not means one target using today's data while everything else uses the
release, which is very hard to notice.

To check the code:

```bash
grep -rnE "\btbl\(|dbGetQuery|dbReadTable" R/ --include="*.R" | grep -v tbl_as_of
```

To check what actually ran, look at the queries the server received. Every
one against a data table must contain `FOR SYSTEM_TIME`.

---

## 6. Stores

Each pipeline writes its `targets` store to a network share, with the
release name on the end of the directory name:

```
/mnt/share_b/accounts/_targets_current
/mnt/share_b/accounts/_targets_oiia_2025_advance
/mnt/share_b/accounts/_targets_oiia_2025_final
```

The store path is set in `_targets.R`:

```r
release <- Sys.getenv("RELEASE", "current")
tar_config_set(store = paste0("/mnt/share_b/accounts/_targets_", release))
```

Day to day, everything happens in the `current` store. When a release is
published, that release gets its own store, and nothing writes to it
again.

To look at a past release, set the release name and read that store. No
code is edited, and nothing is recalculated.

`_targets.yaml` is written automatically by `tar_config_set`. It is in
`.gitignore`, because it holds a path that is specific to this machine and
this release.

---

## 7. The order the pipelines run in

There are four pipelines, and the order matters:

1. **slaughtering_workflow** — reads the database
2. **accounts_workflow** — reads the database and the slaughterings store
3. **other_production_workflow** — reads the accounts store (meat balance,
   cereal stocks)
4. **quarterlies_workflow** — reads the accounts store

The slaughterings and the other production work used to be in one
repository. That made it look as if the accounts and the production
pipelines each needed the other's results, which would have made a
consistent release impossible. They were separated because they are two
different jobs that happen at two different points in the sequence.

If you add a target that reads another pipeline's store, check that it
does not create a loop. Use `tar_network(targets_only = TRUE)$edges` to
see the dependencies as text.

---

## 8. Publishing a release

```r
publish_release(
  con     = con,
  release = "oiia_2025_final",
  repos   = c("../slaughtering_workflow",
              "../accounts_workflow",
              "../other_production_workflow",
              "../quarterlies_workflow"),
  note    = "Final estimate 2025. Approved by X, 25 June 2026.")
```

The function does this:

1. Checks each repository: no uncommitted changes, commits pushed, the
   release tag not already there.
2. Sources `_targets.R` in each repository with the release name set, to
   find out where each one writes its store, and checks that the share is
   reachable and that the store does not already exist.
3. Adds the release row to `Release_lookup`.
4. Runs `tar_make()` in each repository, in the order given. It stops at
   the first failure.
5. Adds the git tag `release/oiia_2025_final` to each repository and
   pushes it.

If anything fails, the release row is deleted and the function stops. The
name is then free again, and the next attempt gets a new timestamp. A part
published release is worse than none, because the stores would look
complete.

### Why the git tags matter

A release is three things together:

- **the data**: the timestamp in `Release_lookup`
- **the code**: the git commit in each repository
- **the results**: the store for each repository

The store alone is not enough. If a calculation is corrected later and you
then rerun that release, `targets` will notice the changed function and
rebuild, and the answer may differ from what was published. That is
correct behaviour, and it is telling you something true: the published
figure came from the old code.

The tag is how you get the old code back:

```bash
git checkout release/oiia_2025_final
```

---

## 9. Reproducing a past release

```bash
cd accounts_workflow
git checkout release/oiia_2025_final
RELEASE=oiia_2025_final Rscript -e 'targets::tar_make()'
```

Nothing should rebuild, because the store already holds every result. If
something does rebuild, the code has changed since the release, and
`targets` is telling you so.

To read a value without running anything:

```r
targets::tar_read(eurostat_output,
                  store = "/mnt/share_b/accounts/_targets_oiia_2025_final")
```

---

## 10. Comparing two releases

This is what makes revisions visible. Read the same output from two
stores and compare.

```r
old <- tar_read(eurostat_output, store = ".../_targets_oiia_2025_advance")
new <- tar_read(eurostat_output, store = ".../_targets_oiia_2025_final")

dplyr::full_join(old, new, by = c("variable", "year_concerned", "period"),
                 suffix = c("_old", "_new")) |>
  dplyr::filter(!isTRUE(all.equal(value_old, value_new)))
```

Run this at every release. It answers the question that comes up first
whenever somebody asks about a revision: which numbers changed, and by how
much.

---

## 11. Things that go wrong

**Two loads in one run.** If somebody loads data while you are running at
`current`, different targets can read different moments. This is why
nothing is ever published from `current`.

**A read that ignores the release.** One database read that does not go
through `tbl_as_of` will use today's data while everything else uses the
release. Nothing reports an error. Use the grep in section 5 to check.

**A load written in several transactions.** SQL Server timestamps a load
from the start of the transaction. If a load function commits several
times, one load produces several different timestamps, and a release taken
in the middle would hold half of it. Every load must run inside one
transaction. To check:

```sql
SELECT SysStart, COUNT(*) FROM dbo.Milk_accounts FOR SYSTEM_TIME ALL
GROUP BY SysStart ORDER BY SysStart DESC;
```

One load should give one value.

**Editing the history table.** Do not. To change it you must turn
versioning off, which is not an undo, it is changing the record. If a load
was wrong, load the correction as a new version.

**Changing a column.** Columns can still be added, changed and dropped
while versioning is on, and SQL Server applies the change to the history
table at the same time. But dropping a column removes it from the history
as well, so the old values are gone.

---

## 12. Reference

### Tables

| Table | Purpose |
|---|---|
| `dbo.Release_lookup` | one row for each named moment |
| `dbo.<name>` | current version of each data row |
| `dbo.<name>_history` | every earlier version, written by SQL Server |

### Functions

| Function | Purpose |
|---|---|
| `lookup_release(con, release)` | turns a release name into a timestamp |
| `tbl_as_of(con, table, as_of)` | lazy read of a table at that moment |
| `publish_release(con, release, repos)` | runs everything and records it |

### Useful queries

```sql
-- what releases exist
SELECT * FROM dbo.Release_lookup ORDER BY as_of_utc;

-- which tables are versioned, and where their history goes
SELECT t.name, h.name AS history_table
FROM sys.tables t
LEFT JOIN sys.tables h ON h.object_id = t.history_table_id
WHERE t.temporal_type = 2 ORDER BY t.name;

-- every version of one row
SELECT *, SysStart, SysEnd
FROM dbo.Milk_accounts FOR SYSTEM_TIME ALL
WHERE variable = 'price' AND year_concerned = 2023
ORDER BY SysStart;
```
