# Migration Runbook: Unique Constraints to Primary Keys and System Versioning

This runbook converts a database where each table has a natural-key
`UNIQUE` constraint into a database of system-versioned temporal tables.

It assumes:

- Each table has one unique constraint, named after the table.
- The constraint columns differ between tables.
- Most constraint columns permit NULL.
- History is written by hand today, into separate archive tables.

Run every step in development first. Run in production from the same
generated files.

---

## 0. Order of work

| Step | What | Reversible? |
|---|---|---|
| 1 | Back up | — |
| 2 | Pre-flight checks | Yes |
| 3 | Fix key widths | Yes |
| 4 | Fix NULLs and duplicates in data | Yes |
| 5 | Generate and run `001_primary_keys.sql` | Yes, with a script |
| 6 | Verify catalogue-reading code | — |
| 7 | Drop legacy archive tables | No |
| 8 | Generate and run `003_system_versioning.sql` | Yes, with a script |
| 9 | Verify with a real load | — |

Step 4 is the only step that needs judgement. Everything else is
generated.

---

## 1. Setup

```r
library(DBI)

con <- dbConnect(odbc::odbc(), dsn = Sys.getenv("MY_DSN"))
message("Connected to: ", dbGetQuery(con, "SELECT DB_NAME() AS db")$db)

dir.create("sql",        showWarnings = FALSE)
dir.create("migrations", showWarnings = FALSE)
```

Three helpers. Put them in a file and source it.

```r
# Run a generator script and return its rows
gen <- function(con, path) {
  dbGetQuery(con, paste(readLines(path), collapse = "\n"))
}

# Write statements to a migration file, one delimiter between each
write_migration <- function(stmts, path) {
  writeLines(paste(trimws(stmts), collapse = "\n-- >>>\n"), path)
  message("Wrote ", length(stmts), " statements to ", path)
}

# Run a migration file, all statements in one transaction
run_migration <- function(con, path) {
  txt   <- paste(readLines(path), collapse = "\n")
  stmts <- strsplit(txt, "\n-- >>>\n", fixed = TRUE)[[1]]
  stmts <- trimws(stmts)
  stmts <- stmts[nzchar(stmts) & !grepl("^--", stmts)]
  dbWithTransaction(con, {
    for (s in stmts) dbExecute(con, s)
    dbExecute(con, sprintf(
      "INSERT INTO schema_migrations (filename) VALUES ('%s')", basename(path)))
  })
  message("Applied ", length(stmts), " statements from ", basename(path))
}
```

If your connection is a `pool` object, use `pool::poolWithTransaction()`
in place of `dbWithTransaction()`.

Create the migration record once:

```r
dbExecute(con, "
IF OBJECT_ID('dbo.schema_migrations') IS NULL
CREATE TABLE dbo.schema_migrations (
  id          int IDENTITY PRIMARY KEY,
  filename    nvarchar(200) NOT NULL,
  applied_utc datetime2(7) NOT NULL DEFAULT SYSUTCDATETIME()
);")
```

---

## 2. Pre-flight checks

### 2.1 The key inventory

This is the base query for everything below. Save it, because you will
read it many times.

```r
writeLines("
SELECT t.name AS tbl, i.name AS constraint_name, ic.key_ordinal,
       c.name AS col, c.is_nullable, ty.name AS type_name,
       c.max_length, c.precision, c.scale
FROM sys.tables t
JOIN sys.indexes i        ON i.object_id = t.object_id
                         AND i.is_unique = 1 AND i.is_primary_key = 0
JOIN sys.index_columns ic ON ic.object_id = i.object_id
                         AND ic.index_id = i.index_id AND ic.key_ordinal > 0
JOIN sys.columns c        ON c.object_id = ic.object_id AND c.column_id = ic.column_id
JOIN sys.types ty         ON ty.user_type_id = c.user_type_id
WHERE t.temporal_type = 0
ORDER BY t.name, ic.key_ordinal;
", "sql/key_inventory.sql")

keys <- gen(con, "sql/key_inventory.sql")
head(keys, 20)
table(keys$tbl)          # how many key columns per table
```

### 2.2 Name clashes for the history tables

```r
dbGetQuery(con, "SELECT name FROM sys.tables WHERE name LIKE '%[_]history'")
```

This must be empty, or you must choose other names.

### 2.3 Tables with no unique constraint

```r
dbGetQuery(con, "
SELECT t.name FROM sys.tables t
WHERE t.temporal_type = 0
  AND NOT EXISTS (SELECT 1 FROM sys.indexes i
                  WHERE i.object_id = t.object_id AND i.is_unique = 1)")
```

These need a key designed by hand. Do them separately.

---

## 3. Key width: the clustered index warning

### What the warning means

SQL Server limits an index key to **900 bytes** when clustered, and
**1700 bytes** when nonclustered.

The check uses the **declared** size, not the data. Five columns of
`nvarchar(100)` cost 1000 bytes, even if every value is 6 characters
long. `nvarchar` costs two bytes for each character, so it reaches the
limit twice as fast as `varchar`.

The warning is not an error. The index is created. But an insert whose
actual values exceed the limit fails at run time, months later. Do not
ignore it.

### 3.1 Measure

```r
keys$bytes <- ifelse(keys$max_length == -1, 8000, keys$max_length)

widths <- aggregate(bytes ~ tbl, data = keys, FUN = sum)
widths[order(-widths$bytes), ]
```

Any table over 900 needs attention. Any table over 1700 must be fixed,
because no index type accepts it.

### 3.2 Find the real sizes in the data

The declared size is usually far larger than the data needs. This builds
one query that measures every text key column.

```r
sql <- paste(
  sprintf("SELECT '%s' AS tbl, '%s' AS col, MAX(LEN(%s)) AS max_len, COUNT(*) AS n
           FROM dbo.[%s]",
          keys$tbl, keys$col, paste0("[", keys$col, "]"), keys$tbl)[
            keys$type_name %in% c("varchar", "nvarchar", "char", "nchar")],
  collapse = "\nUNION ALL\n")

observed <- dbGetQuery(con, sql)
observed[order(-observed$max_len), ]
```

### 3.3 Choose the fix

In order of preference:

1. **Shrink the column.** A column declared `nvarchar(255)` that holds
   values of 12 characters should be `nvarchar(30)`. This is the right
   fix, because the declaration was wrong.
2. **Change `nvarchar` to `varchar`.** This halves the cost. Do this only
   if the column holds no characters outside your code page.
3. **Make the primary key `NONCLUSTERED`.** The limit rises to 1700
   bytes. The table then stays a heap, unless you add a separate
   clustered index on a narrow column.
4. **Drop a column from the key.** Only if it is truly not part of the
   identity of a row. Test uniqueness without it first.

Never add a surrogate identity key to get around the limit. It makes the
duplicate problem invisible instead of solving it.

### 3.4 Apply the shrink

Write these by hand, from the `observed` table. Leave room to grow.

```r
shrink <- c(
  "ALTER TABLE dbo.[my_table] ALTER COLUMN [description] nvarchar(60) NULL;",
  "ALTER TABLE dbo.[my_table] ALTER COLUMN [source]      varchar(20)  NULL;"
)
write_migration(shrink, "migrations/000_shrink_key_columns.sql")
```

You cannot shrink a column while an index uses it. So this file must run
**after** the constraint is dropped. The simplest route is to put these
statements into `001` by hand, between the drop and the add. Step 5.3
shows where.

---

## 4. NULLs and duplicates in the data

### 4.1 NULLs in key columns

```r
nullable <- keys[keys$is_nullable == 1, ]

sql <- paste(sprintf(
  "SELECT '%s' AS tbl, '%s' AS col, COUNT(*) AS n_null FROM dbo.[%s] WHERE [%s] IS NULL",
  nullable$tbl, nullable$col, nullable$tbl, nullable$col),
  collapse = "\nUNION ALL\n")

nulls <- dbGetQuery(con, sql)
nulls[nulls$n_null > 0, ]
```

Every row returned is a decision. A NULL in a key column means the key is
wrong, or the value is unknown, or the column is not really part of the
key. Do not fill it in without asking what it means.

When you have decided, write the updates as `migrations/000b_fix_nulls.sql`.

### 4.2 Duplicates

A unique constraint permits one NULL for each combination. So duplicates
can exist today, and they will block the primary key.

```r
by_tbl <- split(keys, keys$tbl)

sql <- paste(vapply(by_tbl, function(k) {
  cols <- paste0("[", k$col[order(k$key_ordinal)], "]", collapse = ", ")
  sprintf("SELECT '%s' AS tbl, COUNT(*) AS n_dup FROM
           (SELECT %s FROM dbo.[%s] GROUP BY %s HAVING COUNT(*) > 1) d",
          k$tbl[1], cols, k$tbl[1], cols)
}, character(1)), collapse = "\nUNION ALL\n")

dups <- dbGetQuery(con, sql)
dups[dups$n_dup > 0, ]
```

Both results must be empty before you continue.

---

## 5. Migration 001: primary keys

### 5.1 The generator

```r
writeLines("
WITH k AS (
  SELECT t.object_id, t.name AS tbl, i.name AS cname, i.index_id,
         ic.key_ordinal, c.name AS col, c.is_nullable,
         CASE
           WHEN ty.name IN ('varchar','char','varbinary','binary')
             THEN ty.name + '(' + IIF(c.max_length = -1, 'max',
                  CAST(c.max_length AS varchar(10))) + ')'
           WHEN ty.name IN ('nvarchar','nchar')
             THEN ty.name + '(' + IIF(c.max_length = -1, 'max',
                  CAST(c.max_length / 2 AS varchar(10))) + ')'
           WHEN ty.name IN ('decimal','numeric')
             THEN ty.name + '(' + CAST(c.precision AS varchar(10)) + ',' +
                  CAST(c.scale AS varchar(10)) + ')'
           WHEN ty.name IN ('datetime2','time','datetimeoffset')
             THEN ty.name + '(' + CAST(c.scale AS varchar(10)) + ')'
           ELSE ty.name
         END AS full_type
  FROM sys.tables t
  JOIN sys.indexes i        ON i.object_id = t.object_id
                           AND i.is_unique = 1 AND i.is_primary_key = 0
  JOIN sys.index_columns ic ON ic.object_id = i.object_id
                           AND ic.index_id = i.index_id AND ic.key_ordinal > 0
  JOIN sys.columns c        ON c.object_id = ic.object_id AND c.column_id = ic.column_id
  JOIN sys.types ty         ON ty.user_type_id = c.user_type_id
  WHERE t.temporal_type = 0
)
SELECT tbl, seq, ord, stmt FROM (
  -- 1. drop the unique constraint
  SELECT DISTINCT tbl, 1 AS seq, 0 AS ord,
         'ALTER TABLE dbo.' + QUOTENAME(tbl) +
         ' DROP CONSTRAINT ' + QUOTENAME(cname) + ';' AS stmt
  FROM k
  UNION ALL
  -- 2. make each nullable key column NOT NULL
  SELECT tbl, 2, key_ordinal,
         'ALTER TABLE dbo.' + QUOTENAME(tbl) + ' ALTER COLUMN ' +
         QUOTENAME(col) + ' ' + full_type + ' NOT NULL;'
  FROM k WHERE is_nullable = 1
  UNION ALL
  -- 3. add the primary key, with the original name
  SELECT tbl, 3, 0,
         'ALTER TABLE dbo.' + QUOTENAME(tbl) + ' ADD CONSTRAINT ' +
         QUOTENAME(MIN(cname)) + ' PRIMARY KEY' +
         IIF(EXISTS (SELECT 1 FROM sys.indexes x
                     WHERE x.object_id = MIN(k.object_id) AND x.type = 1
                       AND x.index_id <> MIN(k.index_id)), ' NONCLUSTERED', '') +
         ' (' + STRING_AGG(QUOTENAME(col), ', ')
                WITHIN GROUP (ORDER BY key_ordinal) + ');'
  FROM k GROUP BY tbl
) s
ORDER BY tbl, seq, ord;
", "sql/gen_001_primary_keys.sql")
```

### 5.2 Generate and read

```r
out <- gen(con, "sql/gen_001_primary_keys.sql")
nrow(out)
writeLines(out$stmt[1:12])      # look at the shape
```

### 5.3 Insert any width fixes

The shrink statements from step 3.4 belong between the drop and the
`NOT NULL` changes.

```r
shrink_df <- data.frame(
  tbl  = "my_table",
  seq  = 1.5,
  ord  = 0,
  stmt = "ALTER TABLE dbo.[my_table] ALTER COLUMN [description] nvarchar(60) NULL;")

out <- rbind(out, shrink_df)
out <- out[order(out$tbl, out$seq, out$ord), ]
```

### 5.4 Write the file, then read it

```r
write_migration(out$stmt, "migrations/001_primary_keys.sql")
file.edit("migrations/001_primary_keys.sql")
```

**Read the file.** It is a few hundred lines. This is the check that no
generator can do for you. Look for a wrong type, a lost column, and a
column order that differs from the original constraint.

### 5.5 Run it

```r
run_migration(con, "migrations/001_primary_keys.sql")
```

Watch the messages. The warning about a 900-byte clustered key appears
here if you missed a wide table. It does not stop the run, so read the
output.

### 5.6 Confirm

```r
dbGetQuery(con, "
SELECT t.name AS tbl, k.name AS pk_name, i.type_desc
FROM sys.tables t
JOIN sys.key_constraints k ON k.parent_object_id = t.object_id AND k.type = 'PK'
JOIN sys.indexes i ON i.object_id = t.object_id AND i.index_id = k.unique_index_id
ORDER BY t.name")
```

---

## 6. Verify catalogue-reading code

If any function reads the catalogue to find key columns, test it now.
`INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE` returns primary keys as well
as unique constraints, so a function that filters by constraint **name**
keeps working.

```r
before <- readRDS("checks/keys_before.rds")   # saved before step 5
after  <- lapply(names(before), function(t) get_unique_constraints(con, t))
names(after) <- names(before)

vapply(names(before), function(t)
  identical(sort(before[[t]]$COLUMN_NAME), sort(after[[t]]$COLUMN_NAME)),
  logical(1))
```

Save `before` first:

```r
dir.create("checks", showWarnings = FALSE)
tbls <- unique(keys$tbl)
saveRDS(setNames(lapply(tbls, function(t) get_unique_constraints(con, t)), tbls),
        "checks/keys_before.rds")
```

---

## 7. Drop the legacy archive tables

Do this **before** versioning is on. A versioned table cannot be dropped
or truncated without turning versioning off first.

```r
old <- dbGetQuery(con, "SELECT name FROM sys.tables WHERE name LIKE '%[_]archive%'")
old
```

Back them up first, then:

```r
write_migration(sprintf("DROP TABLE dbo.[%s];", old$name),
                "migrations/002_drop_legacy_archive.sql")
run_migration(con, "migrations/002_drop_legacy_archive.sql")
```

Keep the archive code running for now. Step 9 uses it as a check.

---

## 8. Migration 003: system versioning

### 8.1 The generator

The history name drops a development suffix. Change `'_test'` and its
length to match your own suffix, or remove the `IIF` if there is none.

```r
writeLines("
WITH t AS (
  SELECT object_id, name,
         IIF(RIGHT(name, 5) = '_test', LEFT(name, LEN(name) - 5), name) + '_history'
           AS hist
  FROM sys.tables
  WHERE temporal_type = 0
    AND EXISTS (SELECT 1 FROM sys.key_constraints k
                WHERE k.parent_object_id = object_id AND k.type = 'PK')
)
SELECT name AS tbl, v.seq, v.stmt
FROM t
CROSS APPLY (VALUES
  (1, 'ALTER TABLE dbo.' + QUOTENAME(t.name) + ' ADD
  SysStart datetime2(7) GENERATED ALWAYS AS ROW START HIDDEN NOT NULL
    CONSTRAINT ' + QUOTENAME('DF_' + t.name + '_SysStart') + ' DEFAULT SYSUTCDATETIME(),
  SysEnd   datetime2(7) GENERATED ALWAYS AS ROW END   HIDDEN NOT NULL
    CONSTRAINT ' + QUOTENAME('DF_' + t.name + '_SysEnd') + '
      DEFAULT ''9999-12-31 23:59:59.9999999'',
  PERIOD FOR SYSTEM_TIME (SysStart, SysEnd);'),
  (2, 'ALTER TABLE dbo.' + QUOTENAME(t.name) + '
  SET (SYSTEM_VERSIONING = ON (HISTORY_TABLE = dbo.' + QUOTENAME(t.hist) + ',
       DATA_CONSISTENCY_CHECK = ON));')
) v(seq, stmt)
ORDER BY tbl, seq;
", "sql/gen_003_system_versioning.sql")
```

The `EXISTS` clause is the safety catch. A table with no primary key is
skipped, not attempted.

### 8.2 Generate, read, run

```r
out3 <- gen(con, "sql/gen_003_system_versioning.sql")
nrow(out3) / 2                    # number of tables
write_migration(out3$stmt, "migrations/003_system_versioning.sql")
file.edit("migrations/003_system_versioning.sql")

run_migration(con, "migrations/003_system_versioning.sql")
```

### 8.3 Confirm

```r
dbGetQuery(con, "
SELECT t.name AS tbl, h.name AS history_table, t.temporal_type_desc
FROM sys.tables t
LEFT JOIN sys.tables h ON h.object_id = t.history_table_id
WHERE t.temporal_type = 2
ORDER BY t.name")
```

Every table you intended must appear. `temporal_type = 2` means
system-versioned.

---

## 9. Verify with a real load

This is the step that pays for the work. Run one true load, then check.

### 9.1 One load must give one timestamp

```r
dbGetQuery(con, "
SELECT CAST(SysStart AS datetime2(7)) AS sys_start, COUNT(*) AS n
FROM dbo.[my_table] FOR SYSTEM_TIME ALL
GROUP BY CAST(SysStart AS datetime2(7))
ORDER BY sys_start DESC")
```

If one load produced many distinct values, a code path is missing its
transaction. Find it before you go to production.

### 9.2 Engine history must match the old archive

Run this while the old archive code still runs in parallel:

```r
a <- dbGetQuery(con, "SELECT * FROM dbo.[my_table_archive_backup]")
h <- dbGetQuery(con, "SELECT * FROM dbo.[my_table_history]")
nrow(a); nrow(h)
```

When these agree over several loads, delete the archive code.

### 9.3 Read a vintage

```r
read_as_of <- function(con, table, as_of) {
  dbGetQuery(con, glue::glue_sql(
    "SELECT * FROM {`table`} FOR SYSTEM_TIME AS OF {as_of}", .con = con))
}

read_as_of(con, "my_table", as.POSIXct("2026-08-01 09:00:00", tz = "UTC"))
```

The period columns are UTC. Always pass UTC.

---

## 10. Production

Change nothing in the files. Run the same artefacts.

```r
con_prod <- dbConnect(odbc::odbc(), dsn = Sys.getenv("MY_DSN_PROD"))
message("Connected to: ", dbGetQuery(con_prod, "SELECT DB_NAME() AS db")$db)

# 1. back up
# 2. restore that backup over development, and run the files there once more
# 3. then:
run_migration(con_prod, "migrations/001_primary_keys.sql")
run_migration(con_prod, "migrations/002_drop_legacy_archive.sql")
run_migration(con_prod, "migrations/003_system_versioning.sql")
```

Step 2 is the step people skip. A development database that has been
edited by hand for months proves less than you think.

---

## 11. If you must reverse it

```r
undo <- dbGetQuery(con, "
SELECT 'ALTER TABLE dbo.' + QUOTENAME(t.name) +
       ' SET (SYSTEM_VERSIONING = OFF);' AS stmt
FROM sys.tables t WHERE t.temporal_type = 2")

for (s in undo$stmt) dbExecute(con, s)
```

The history tables remain as ordinary tables after this. Nothing is lost.
Dropping the period columns and the primary keys is a further step, and
you should prefer to restore the backup instead.

---

## 12. Notes to keep

- `HIDDEN` on the period columns keeps them out of `SELECT *`. Without
  it, `dbAppendTable()` tries to write to `GENERATED ALWAYS` columns and
  fails.
- The engine stamps `SysStart` with the time the **transaction** began.
  One load in one transaction gives one clean vintage.
- A delete followed by an insert, inside one transaction, is a clean
  version change. There is no gap.
- A row inserted and deleted in the same transaction is discarded. It
  never reaches history.
- Never edit the history table. Correct a bad load by loading the
  correction as a new version.
- Columns can still be added, altered and dropped while versioning is on.
  A dropped column disappears from history too.
