/**********************************************************************
  TEMPORAL MIGRATION
  Convert natural-key UNIQUE constraints into PRIMARY KEYs,
  then turn on system versioning.

  HOW TO USE THIS FILE
  --------------------
  Do not run the whole file. Run one PART at a time, in order, and
  read the result of each part before you go on.

  PART 0   Guards and settings
  PART 1   Control tables
  PART 2   Fill the target list
  PART 3   Review and exclude
  PART 4   Pre-flight checks   <- nothing changes until these pass
  PART 5   Check the tables you already migrated by hand
  PART 6   Generate script 001 (primary keys)
  PART 7   Run script 001
  PART 8   Generate script 003 (system versioning)
  PART 9   Run script 003
  PART 10  Verify
  PART 11  Reverse

  THE SAFETY MODEL
  ----------------
  1. One control table decides which tables are touched. Nothing else.
  2. Statements are generated into a table, not into the results grid.
     The grid truncates long text without telling you.
  3. The runner has a @debug flag. It prints and rolls back.
  4. Each script runs inside one transaction. A failure leaves nothing
     half done.
**********************************************************************/


/**********************************************************************
  PART 0. GUARDS AND SETTINGS
**********************************************************************/

-- Confirm the database. Read this before you continue.
SELECT DB_NAME() AS current_database, SUSER_NAME() AS login_name;

-- STRING_AGG needs SQL Server 2017 (14.x). Temporal tables need 2016.
-- Stop now if the engine is older.
IF CAST(SERVERPROPERTY('ProductMajorVersion') AS int) < 14
  THROW 50000, 'This script needs SQL Server 2017 or later (STRING_AGG).', 1;

SELECT SERVERPROPERTY('ProductVersion') AS product_version,
       SERVERPROPERTY('Edition')        AS edition;
GO


/**********************************************************************
  PART 1. CONTROL TABLES

  migration_targets  = which tables to touch, and what to call their
                       history tables. You edit this by hand.
  migration_steps    = the generated statements, and a record of which
                       ones ran.

  Both are ordinary tables. They stay in the database as the record of
  what happened.
**********************************************************************/

IF OBJECT_ID('dbo.migration_targets') IS NULL
CREATE TABLE dbo.migration_targets (
  sch        sysname       NOT NULL,   -- schema of the table
  tbl        sysname       NOT NULL,   -- table name
  uc_name    sysname       NULL,       -- the unique constraint to convert
  uc_count   int           NULL,       -- how many unique constraints exist
  hist_name  sysname       NULL,       -- name for the history table
  include    bit           NOT NULL DEFAULT 1,
  note       nvarchar(200) NULL,
  CONSTRAINT PK_migration_targets PRIMARY KEY (sch, tbl)
);

IF OBJECT_ID('dbo.migration_steps') IS NULL
CREATE TABLE dbo.migration_steps (
  id          int IDENTITY  PRIMARY KEY,
  script      varchar(20)   NOT NULL,  -- '001' or '003'
  sch         sysname       NOT NULL,
  tbl         sysname       NOT NULL,
  seq         int           NOT NULL,  -- order of the kind of statement
  ord         int           NOT NULL,  -- order within that kind
  stmt        nvarchar(max) NOT NULL,
  applied_utc datetime2(7)  NULL       -- set by the runner
);
GO


/**********************************************************************
  PART 2. FILL THE TARGET LIST

  Notes on the exclusions:

  * In LIKE, an underscore matches ANY single character. So '%_staging'
    also matches 'xstaging'. Write [_] to mean a real underscore.

  * temporal_type: 0 = ordinary table, 1 = a history table,
    2 = a system-versioned table. We take only 0. This is what excludes
    the two tables you already migrated, and their history tables.

  * uc_name comes from the catalogue. is_unique_constraint = 1 selects
    a real UNIQUE constraint, not a plain unique index. A unique index
    needs DROP INDEX, not DROP CONSTRAINT, so it must not be included
    here by accident.
**********************************************************************/

TRUNCATE TABLE dbo.migration_targets;

INSERT INTO dbo.migration_targets (sch, tbl, uc_name, uc_count, hist_name, note)
SELECT s.name,
       t.name,
       uc.uc_name,
       uc.uc_count,
       -- Strip a trailing '_test' from the history name.
       -- Change the suffix here if yours is different.
       IIF(RIGHT(t.name, 5) = '_test', LEFT(t.name, LEN(t.name) - 5), t.name)
         + '_history',
       NULL
FROM sys.tables t
JOIN sys.schemas s ON s.schema_id = t.schema_id
OUTER APPLY (
  SELECT MIN(i.name) AS uc_name, COUNT(*) AS uc_count
  FROM sys.indexes i
  WHERE i.object_id = t.object_id
    AND i.is_unique_constraint = 1
) uc
WHERE t.temporal_type = 0          -- not already versioned, not a history table
  AND t.is_ms_shipped = 0
  AND s.name = 'dbo'
  AND t.name NOT LIKE '%[_]archive[_]test'   -- your archive tables
  AND t.name NOT LIKE '%staging'             -- your staging tables
  AND t.name NOT LIKE '%[_]history'
  AND t.name NOT IN ('migration_targets', 'migration_steps', 'schema_migrations');
GO


/**********************************************************************
  PART 3. REVIEW AND EXCLUDE

  Read this list. It is the whole scope of the migration.
**********************************************************************/

SELECT sch, tbl, uc_name, uc_count, hist_name, include, note
FROM dbo.migration_targets
ORDER BY include DESC, tbl;

-- Tables with no unique constraint. They need a key designed by hand.
-- Exclude them for now.
UPDATE dbo.migration_targets
SET include = 0, note = 'no unique constraint'
WHERE uc_name IS NULL;

-- Tables with more than one unique constraint. The generator cannot
-- guess which one is the natural key. Set uc_name by hand, then set
-- include back to 1.
UPDATE dbo.migration_targets
SET include = 0, note = 'more than one unique constraint - choose by hand'
WHERE uc_count > 1;

-- Exclude anything else you do not want. Example:
-- UPDATE dbo.migration_targets SET include = 0, note = 'scratch table'
-- WHERE tbl IN ('some_table', 'another_table');

-- Final scope:
SELECT COUNT(*) AS tables_in_scope FROM dbo.migration_targets WHERE include = 1;
GO


/**********************************************************************
  PART 4. PRE-FLIGHT CHECKS

  Every check below must return NO ROWS before you go to PART 6.
  Nothing in this part changes the database.
**********************************************************************/

-- 4.1 KEY WIDTH -------------------------------------------------------
-- The engine checks the DECLARED size of the key columns, not the data.
-- Limits: 900 bytes for a clustered index, 1700 for a nonclustered one.
-- nvarchar costs 2 bytes for each character, so it reaches the limit
-- twice as fast as varchar.
--
-- Going over gives a WARNING, not an error. The index is created, and
-- an insert with large values fails later. Do not ignore it.
--
-- max_length = -1 means a MAX type. Those cannot be in an index key
-- at all.

SELECT m.tbl,
       SUM(IIF(c.max_length = -1, 999999, c.max_length)) AS key_bytes,
       MAX(IIF(c.max_length = -1, 1, 0))                 AS has_max_type,
       STRING_AGG(CONCAT(c.name, ' ', ty.name, '(',
                         IIF(c.max_length = -1, 'max',
                             CAST(IIF(ty.name IN ('nvarchar','nchar'),
                                      c.max_length / 2, c.max_length) AS varchar(10))),
                         ')'), ', ')
         WITHIN GROUP (ORDER BY ic.key_ordinal) AS key_definition
FROM dbo.migration_targets m
JOIN sys.tables t         ON t.name = m.tbl AND SCHEMA_NAME(t.schema_id) = m.sch
JOIN sys.indexes i        ON i.object_id = t.object_id AND i.name = m.uc_name
JOIN sys.index_columns ic ON ic.object_id = i.object_id AND ic.index_id = i.index_id
                         AND ic.key_ordinal > 0
JOIN sys.columns c        ON c.object_id = ic.object_id AND c.column_id = ic.column_id
JOIN sys.types ty         ON ty.user_type_id = c.user_type_id
WHERE m.include = 1
GROUP BY m.tbl
HAVING SUM(IIF(c.max_length = -1, 999999, c.max_length)) > 900
ORDER BY key_bytes DESC;

-- If rows come back, see PART 4.1b to find the real sizes, then shrink
-- the columns. The shrink statements go into script 001 (PART 6.4),
-- because you cannot alter a column while an index uses it.


-- 4.1b REAL SIZES IN THE DATA ----------------------------------------
-- The declared size is usually far larger than the data needs. This
-- builds one query that measures every text key column, then runs it.

DECLARE @sql nvarchar(max);

SELECT @sql = STRING_AGG(CAST(
  'SELECT ''' + m.tbl + ''' AS tbl, ''' + c.name + ''' AS col, '
  + CAST(IIF(ty.name IN ('nvarchar','nchar'), c.max_length/2, c.max_length) AS varchar(10))
  + ' AS declared_len, MAX(LEN(' + QUOTENAME(c.name) + ')) AS observed_len, COUNT(*) AS n_rows'
  + ' FROM ' + QUOTENAME(m.sch) + '.' + QUOTENAME(m.tbl)
  AS nvarchar(max)), CHAR(13) + 'UNION ALL ')
FROM dbo.migration_targets m
JOIN sys.tables t         ON t.name = m.tbl AND SCHEMA_NAME(t.schema_id) = m.sch
JOIN sys.indexes i        ON i.object_id = t.object_id AND i.name = m.uc_name
JOIN sys.index_columns ic ON ic.object_id = i.object_id AND ic.index_id = i.index_id
                         AND ic.key_ordinal > 0
JOIN sys.columns c        ON c.object_id = ic.object_id AND c.column_id = ic.column_id
JOIN sys.types ty         ON ty.user_type_id = c.user_type_id
WHERE m.include = 1
  AND ty.name IN ('varchar','nvarchar','char','nchar');

EXEC sp_executesql @sql;
GO


-- 4.2 NULLs IN KEY COLUMNS -------------------------------------------
-- A UNIQUE constraint permits NULL. A PRIMARY KEY does not.
-- Every row returned here is a decision you must make. A NULL in a key
-- column means the key is wrong, the value is unknown, or the column is
-- not really part of the key. Do not fill it in without asking why.

DECLARE @sql nvarchar(max);

SELECT @sql = STRING_AGG(CAST(
  'SELECT ''' + m.tbl + ''' AS tbl, ''' + c.name + ''' AS col, COUNT(*) AS n_null'
  + ' FROM ' + QUOTENAME(m.sch) + '.' + QUOTENAME(m.tbl)
  + ' WHERE ' + QUOTENAME(c.name) + ' IS NULL HAVING COUNT(*) > 0'
  AS nvarchar(max)), CHAR(13) + 'UNION ALL ')
FROM dbo.migration_targets m
JOIN sys.tables t         ON t.name = m.tbl AND SCHEMA_NAME(t.schema_id) = m.sch
JOIN sys.indexes i        ON i.object_id = t.object_id AND i.name = m.uc_name
JOIN sys.index_columns ic ON ic.object_id = i.object_id AND ic.index_id = i.index_id
                         AND ic.key_ordinal > 0
JOIN sys.columns c        ON c.object_id = ic.object_id AND c.column_id = ic.column_id
WHERE m.include = 1 AND c.is_nullable = 1;

EXEC sp_executesql @sql;
GO


-- 4.3 DUPLICATES ------------------------------------------------------
-- A UNIQUE constraint permits one NULL for each combination. So two
-- rows that differ only by a NULL are legal today, and they will block
-- the primary key.

DECLARE @sql nvarchar(max);

SELECT @sql = STRING_AGG(CAST(x.stmt AS nvarchar(max)), CHAR(13) + 'UNION ALL ')
FROM (
  SELECT 'SELECT ''' + m.tbl + ''' AS tbl, COUNT(*) AS dup_groups FROM (SELECT '
         + STRING_AGG(QUOTENAME(c.name), ', ') WITHIN GROUP (ORDER BY ic.key_ordinal)
         + ' FROM ' + QUOTENAME(m.sch) + '.' + QUOTENAME(m.tbl) + ' GROUP BY '
         + STRING_AGG(QUOTENAME(c.name), ', ') WITHIN GROUP (ORDER BY ic.key_ordinal)
         + ' HAVING COUNT(*) > 1) d HAVING COUNT(*) > 0' AS stmt
  FROM dbo.migration_targets m
  JOIN sys.tables t         ON t.name = m.tbl AND SCHEMA_NAME(t.schema_id) = m.sch
  JOIN sys.indexes i        ON i.object_id = t.object_id AND i.name = m.uc_name
  JOIN sys.index_columns ic ON ic.object_id = i.object_id AND ic.index_id = i.index_id
                           AND ic.key_ordinal > 0
  JOIN sys.columns c        ON c.object_id = ic.object_id AND c.column_id = ic.column_id
  WHERE m.include = 1
  GROUP BY m.sch, m.tbl
) x;

EXEC sp_executesql @sql;
GO


-- 4.4 FOREIGN KEYS THAT POINT AT THE UNIQUE CONSTRAINT ----------------
-- DROP CONSTRAINT fails if a foreign key references that constraint.
-- You must drop the foreign key first, and put it back afterwards.

SELECT fk.name AS foreign_key, OBJECT_NAME(fk.parent_object_id) AS child_table,
       m.tbl AS parent_table, m.uc_name
FROM sys.foreign_keys fk
JOIN dbo.migration_targets m
  ON m.tbl = OBJECT_NAME(fk.referenced_object_id) AND m.include = 1
JOIN sys.indexes i
  ON i.object_id = fk.referenced_object_id AND i.index_id = fk.key_index_id
WHERE i.name = m.uc_name;


-- 4.5 OTHER OBJECTS THAT BLOCK ALTER COLUMN ---------------------------
-- ALTER COLUMN fails if the column is used by another index, by a
-- computed column, by a check constraint, or by user-created statistics.
-- We drop the unique constraint first, but these others remain.

-- other indexes
SELECT m.tbl, i2.name AS blocking_index, c.name AS col
FROM dbo.migration_targets m
JOIN sys.tables t          ON t.name = m.tbl AND SCHEMA_NAME(t.schema_id) = m.sch
JOIN sys.indexes i         ON i.object_id = t.object_id AND i.name = m.uc_name
JOIN sys.index_columns ic  ON ic.object_id = i.object_id AND ic.index_id = i.index_id
                          AND ic.key_ordinal > 0
JOIN sys.columns c         ON c.object_id = ic.object_id AND c.column_id = ic.column_id
JOIN sys.index_columns ic2 ON ic2.object_id = c.object_id AND ic2.column_id = c.column_id
JOIN sys.indexes i2        ON i2.object_id = ic2.object_id AND i2.index_id = ic2.index_id
WHERE m.include = 1 AND c.is_nullable = 1 AND i2.name <> m.uc_name;

-- computed columns and check constraints
SELECT m.tbl, 'computed column' AS kind, cc.name AS object_name
FROM dbo.migration_targets m
JOIN sys.tables t ON t.name = m.tbl AND SCHEMA_NAME(t.schema_id) = m.sch
JOIN sys.computed_columns cc ON cc.object_id = t.object_id
WHERE m.include = 1
UNION ALL
SELECT m.tbl, 'check constraint', ck.name
FROM dbo.migration_targets m
JOIN sys.tables t ON t.name = m.tbl AND SCHEMA_NAME(t.schema_id) = m.sch
JOIN sys.check_constraints ck ON ck.parent_object_id = t.object_id
WHERE m.include = 1;


-- 4.6 NAME CLASHES FOR THE HISTORY TABLES -----------------------------
-- SET SYSTEM_VERSIONING creates the history table. It fails if the name
-- is taken.

SELECT m.tbl, m.hist_name, 'name already used' AS problem
FROM dbo.migration_targets m
WHERE m.include = 1
  AND EXISTS (SELECT 1 FROM sys.tables x
              WHERE x.name = m.hist_name AND SCHEMA_NAME(x.schema_id) = m.sch)
UNION ALL
-- two tables that would produce the same history name
SELECT m.tbl, m.hist_name, 'duplicate history name'
FROM dbo.migration_targets m
WHERE m.include = 1
  AND EXISTS (SELECT 1 FROM dbo.migration_targets y
              WHERE y.include = 1 AND y.hist_name = m.hist_name AND y.tbl <> m.tbl);


-- 4.7 COLUMN NAMES ALREADY IN USE -------------------------------------
-- We add columns called SysStart and SysEnd.

SELECT m.tbl, c.name AS existing_column
FROM dbo.migration_targets m
JOIN sys.tables t  ON t.name = m.tbl AND SCHEMA_NAME(t.schema_id) = m.sch
JOIN sys.columns c ON c.object_id = t.object_id
WHERE m.include = 1 AND c.name IN ('SysStart', 'SysEnd');
GO


/**********************************************************************
  PART 5. THE TABLES YOU ALREADY MIGRATED BY HAND

  These are excluded automatically, because temporal_type is no longer 0.
  But you must confirm that they follow the same convention as the
  generated ones. Two standards in one database is worse than one
  wrong standard.
**********************************************************************/

-- 5.1 What exists now
SELECT SCHEMA_NAME(t.schema_id) AS sch,
       t.name                   AS tbl,
       h.name                   AS history_table,
       pk.name                  AS pk_name,
       ss.name                  AS start_col, ss.is_hidden AS start_hidden,
       se.name                  AS end_col,   se.is_hidden AS end_hidden,
       TYPE_NAME(ss.user_type_id) + '(' + CAST(ss.scale AS varchar(2)) + ')' AS period_type
FROM sys.tables t
LEFT JOIN sys.tables h          ON h.object_id = t.history_table_id
LEFT JOIN sys.key_constraints pk ON pk.parent_object_id = t.object_id AND pk.type = 'PK'
LEFT JOIN sys.periods p         ON p.object_id = t.object_id
LEFT JOIN sys.columns ss        ON ss.object_id = t.object_id AND ss.column_id = p.start_column_id
LEFT JOIN sys.columns se        ON se.object_id = t.object_id AND se.column_id = p.end_column_id
WHERE t.temporal_type = 2
ORDER BY t.name;

-- 5.2 Check each item against the convention:
--
--   a) history_table follows the same naming rule (no '_test')
--   b) start_hidden and end_hidden are both 1
--   c) period_type is datetime2(7)
--   d) pk_name matches the old '<table>_UC_entry' name, if code reads
--      the catalogue by constraint name
--   e) the primary key columns equal the old unique constraint columns
--
-- 5.3 Fixes, if they do not match.

-- (b) Make the period columns hidden. This works in place.
--     ALTER TABLE dbo.my_table ALTER COLUMN SysStart ADD HIDDEN;
--     ALTER TABLE dbo.my_table ALTER COLUMN SysEnd   ADD HIDDEN;
--
--     Why this matters: dbAppendTable and similar clients build their
--     column list from the table. A visible GENERATED ALWAYS column
--     makes the append fail.

-- (a) Rename the history table. You must turn versioning off first,
--     because sp_rename is refused on a versioned pair.
--     ALTER TABLE dbo.my_table SET (SYSTEM_VERSIONING = OFF);
--     EXEC sp_rename 'dbo.my_table_history_test', 'my_table_history';
--     ALTER TABLE dbo.my_table
--       SET (SYSTEM_VERSIONING = ON
--            (HISTORY_TABLE = dbo.my_table_history, DATA_CONSISTENCY_CHECK = ON));

-- (d) Rename the primary key constraint. This is safe and in place.
--     EXEC sp_rename 'dbo.PK_my_table', 'my_table_UC_entry', 'OBJECT';

-- 5.4 If these two tables were versioned before they held real loads,
--     their history holds development noise. Clear it now, while it
--     still costs nothing:
--     ALTER TABLE dbo.my_table SET (SYSTEM_VERSIONING = OFF);
--     TRUNCATE TABLE dbo.my_table_history;
--     ALTER TABLE dbo.my_table
--       SET (SYSTEM_VERSIONING = ON (HISTORY_TABLE = dbo.my_table_history));
GO


/**********************************************************************
  PART 6. GENERATE SCRIPT 001 - PRIMARY KEYS

  Each table needs, in this order:
    seq 1  drop the unique constraint
    seq 2  (optional) shrink any over-wide key column   <- added by hand
    seq 3  make each nullable key column NOT NULL
    seq 4  add the primary key, with the SAME name as the old constraint

  Keeping the name matters if any code reads the catalogue to find the
  key columns. INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE returns
  primary keys as well as unique constraints, so such code keeps working.
**********************************************************************/

DELETE FROM dbo.migration_steps WHERE script = '001' AND applied_utc IS NULL;

;WITH uc AS (
  -- one row for each table in scope: its unique constraint, and whether
  -- another clustered index exists once that constraint is dropped
  SELECT t.object_id, m.sch, m.tbl, m.uc_name AS cname, i.index_id,
         CAST(CASE WHEN EXISTS (SELECT 1 FROM sys.indexes x
                                WHERE x.object_id = t.object_id
                                  AND x.type = 1            -- clustered
                                  AND x.index_id <> i.index_id)
                   THEN 1 ELSE 0 END AS bit) AS other_clustered
  FROM dbo.migration_targets m
  JOIN sys.tables t  ON t.name = m.tbl AND SCHEMA_NAME(t.schema_id) = m.sch
  JOIN sys.indexes i ON i.object_id = t.object_id AND i.name = m.uc_name
  WHERE m.include = 1 AND t.temporal_type = 0
),
k AS (
  -- one row for each key column, with its exact type rebuilt as text
  SELECT uc.*, ic.key_ordinal, c.name AS col, c.is_nullable,
         CASE
           WHEN ty.name IN ('varchar','char','varbinary','binary')
             THEN ty.name + '(' + IIF(c.max_length = -1, 'max',
                                      CAST(c.max_length AS varchar(10))) + ')'
           WHEN ty.name IN ('nvarchar','nchar')
             THEN ty.name + '(' + IIF(c.max_length = -1, 'max',
                                      CAST(c.max_length / 2 AS varchar(10))) + ')'
           WHEN ty.name IN ('decimal','numeric')
             THEN ty.name + '(' + CAST(c.precision AS varchar(10)) + ','
                                + CAST(c.scale AS varchar(10)) + ')'
           WHEN ty.name IN ('datetime2','time','datetimeoffset')
             THEN ty.name + '(' + CAST(c.scale AS varchar(10)) + ')'
           ELSE ty.name
         END
         -- ALTER COLUMN resets the collation to the database default if
         -- you do not state it. Carry it over.
         + ISNULL(' COLLATE ' + c.collation_name, '') AS full_type
  FROM uc
  JOIN sys.index_columns ic ON ic.object_id = uc.object_id
                           AND ic.index_id = uc.index_id AND ic.key_ordinal > 0
  JOIN sys.columns c        ON c.object_id = ic.object_id AND c.column_id = ic.column_id
  JOIN sys.types ty         ON ty.user_type_id = c.user_type_id
)
INSERT INTO dbo.migration_steps (script, sch, tbl, seq, ord, stmt)
-- seq 1: drop the unique constraint
SELECT '001', sch, tbl, 1, 0,
       'ALTER TABLE ' + QUOTENAME(sch) + '.' + QUOTENAME(tbl)
       + ' DROP CONSTRAINT ' + QUOTENAME(cname) + ';'
FROM uc
UNION ALL
-- seq 3: make each nullable key column NOT NULL
SELECT '001', sch, tbl, 3, key_ordinal,
       'ALTER TABLE ' + QUOTENAME(sch) + '.' + QUOTENAME(tbl)
       + ' ALTER COLUMN ' + QUOTENAME(col) + ' ' + full_type + ' NOT NULL;'
FROM k
WHERE is_nullable = 1
UNION ALL
-- seq 4: add the primary key
SELECT '001', sch, tbl, 4, 0,
       'ALTER TABLE ' + QUOTENAME(sch) + '.' + QUOTENAME(tbl)
       + ' ADD CONSTRAINT ' + QUOTENAME(MIN(cname)) + ' PRIMARY KEY'
       -- If a clustered index already exists, the primary key must be
       -- nonclustered. This also raises the key limit to 1700 bytes.
       + IIF(MAX(CAST(other_clustered AS int)) = 1, ' NONCLUSTERED', '')
       + ' (' + STRING_AGG(QUOTENAME(col), ', ')
                WITHIN GROUP (ORDER BY key_ordinal) + ');'
FROM k
GROUP BY sch, tbl;
GO

-- 6.4 ADD ANY WIDTH FIXES BY HAND (seq 2)
-- These must run after the drop and before the NOT NULL change.
-- Example:
-- INSERT INTO dbo.migration_steps (script, sch, tbl, seq, ord, stmt)
-- VALUES ('001','dbo','my_table',2,1,
--   'ALTER TABLE [dbo].[my_table] ALTER COLUMN [description] nvarchar(60) NULL;');

-- 6.5 READ THE GENERATED SCRIPT. This is the check no generator can do.
-- Look for: a wrong type, a lost column, a column order that differs
-- from the old constraint, a missing COLLATE.
SELECT id, tbl, seq, ord, stmt
FROM dbo.migration_steps
WHERE script = '001'
ORDER BY tbl, seq, ord;

-- Count what you are about to run.
SELECT COUNT(DISTINCT tbl) AS tables_affected, COUNT(*) AS statements
FROM dbo.migration_steps WHERE script = '001' AND applied_utc IS NULL;
GO


/**********************************************************************
  PART 7. RUN SCRIPT 001

  Run this block TWICE.
    First with @debug = 1. It prints every statement and rolls back.
    Then with @debug = 0. It runs and records.

  Everything is inside one transaction. If statement 90 fails, the
  first 89 are undone. You never end with a table that has no key at all.
**********************************************************************/

SET XACT_ABORT ON;   -- any error aborts the whole transaction

DECLARE @script varchar(20) = '001';
DECLARE @debug  bit         = 1;      -- <<< set to 0 to run for real

DECLARE @id int, @stmt nvarchar(max), @n int = 0;

BEGIN TRY
  BEGIN TRANSACTION;

  DECLARE c CURSOR LOCAL FAST_FORWARD FOR
    SELECT id, stmt
    FROM dbo.migration_steps
    WHERE script = @script AND applied_utc IS NULL
    ORDER BY tbl, seq, ord;

  OPEN c;
  FETCH NEXT FROM c INTO @id, @stmt;

  WHILE @@FETCH_STATUS = 0
  BEGIN
    PRINT @stmt;                        -- PRINT shows the first 4000 chars
    IF @debug = 0
    BEGIN
      EXEC sp_executesql @stmt;
      UPDATE dbo.migration_steps
      SET applied_utc = SYSUTCDATETIME()
      WHERE id = @id;
    END
    SET @n += 1;
    FETCH NEXT FROM c INTO @id, @stmt;
  END

  CLOSE c; DEALLOCATE c;

  IF @debug = 1
  BEGIN
    ROLLBACK;
    PRINT '--- DEBUG: rolled back. ' + CAST(@n AS varchar(10)) + ' statements.';
  END
  ELSE
  BEGIN
    COMMIT;
    PRINT '--- APPLIED ' + CAST(@n AS varchar(10)) + ' statements.';
  END
END TRY
BEGIN CATCH
  IF CURSOR_STATUS('local','c') >= 0 BEGIN CLOSE c; DEALLOCATE c; END
  IF @@TRANCOUNT > 0 ROLLBACK;
  PRINT '--- FAILED ON: ' + ISNULL(@stmt, '(none)');
  THROW;
END CATCH
GO

-- 7.1 CONFIRM. Every table in scope must now have a primary key.
SELECT m.tbl, k.name AS pk_name, i.type_desc AS pk_index_type,
       STRING_AGG(c.name, ', ') WITHIN GROUP (ORDER BY ic.key_ordinal) AS pk_columns
FROM dbo.migration_targets m
JOIN sys.tables t          ON t.name = m.tbl AND SCHEMA_NAME(t.schema_id) = m.sch
LEFT JOIN sys.key_constraints k ON k.parent_object_id = t.object_id AND k.type = 'PK'
LEFT JOIN sys.indexes i    ON i.object_id = t.object_id AND i.index_id = k.unique_index_id
LEFT JOIN sys.index_columns ic ON ic.object_id = i.object_id AND ic.index_id = i.index_id
                              AND ic.key_ordinal > 0
LEFT JOIN sys.columns c    ON c.object_id = ic.object_id AND c.column_id = ic.column_id
WHERE m.include = 1
GROUP BY m.tbl, k.name, i.type_desc
ORDER BY m.tbl;

-- 7.2 STOP HERE and test the application.
-- If any code reads the catalogue to find key columns, run it now and
-- compare the result with what it gave before. The constraint names are
-- unchanged, so it should agree.
GO


/**********************************************************************
  PART 8. GENERATE SCRIPT 003 - SYSTEM VERSIONING

  Two statements for each table:
    seq 1  add the period columns and declare the period
    seq 2  turn versioning on and name the history table

  Points that matter:

  * HIDDEN keeps SysStart and SysEnd out of SELECT *. Without it, a
    client that builds its column list from the table tries to write to
    GENERATED ALWAYS columns, and the write fails.
  * datetime2(7) is required. Older date types are refused.
  * The values are UTC. Any as-of query must pass UTC.
  * The end default must be the maximum value. It marks the row current.
  * The EXISTS clause is the safety catch. A table with no primary key
    is skipped, not attempted.
**********************************************************************/

DELETE FROM dbo.migration_steps WHERE script = '003' AND applied_utc IS NULL;

;WITH tgt AS (
  SELECT m.sch, m.tbl, m.hist_name
  FROM dbo.migration_targets m
  JOIN sys.tables t ON t.name = m.tbl AND SCHEMA_NAME(t.schema_id) = m.sch
  WHERE m.include = 1
    AND t.temporal_type = 0                        -- not already versioned
    AND EXISTS (SELECT 1 FROM sys.key_constraints k
                WHERE k.parent_object_id = t.object_id AND k.type = 'PK')
)
INSERT INTO dbo.migration_steps (script, sch, tbl, seq, ord, stmt)
SELECT '003', tgt.sch, tgt.tbl, v.seq, 0, v.stmt
FROM tgt
CROSS APPLY (VALUES
  (1, 'ALTER TABLE ' + QUOTENAME(tgt.sch) + '.' + QUOTENAME(tgt.tbl) + ' ADD
  SysStart datetime2(7) GENERATED ALWAYS AS ROW START HIDDEN NOT NULL
    CONSTRAINT ' + QUOTENAME('DF_' + tgt.tbl + '_SysStart')
    + ' DEFAULT SYSUTCDATETIME(),
  SysEnd   datetime2(7) GENERATED ALWAYS AS ROW END   HIDDEN NOT NULL
    CONSTRAINT ' + QUOTENAME('DF_' + tgt.tbl + '_SysEnd')
    + ' DEFAULT ''9999-12-31 23:59:59.9999999'',
  PERIOD FOR SYSTEM_TIME (SysStart, SysEnd);'),
  (2, 'ALTER TABLE ' + QUOTENAME(tgt.sch) + '.' + QUOTENAME(tgt.tbl) + '
  SET (SYSTEM_VERSIONING = ON (HISTORY_TABLE = '
    + QUOTENAME(tgt.sch) + '.' + QUOTENAME(tgt.hist_name)
    + ', DATA_CONSISTENCY_CHECK = ON));')
) v(seq, stmt);
GO

-- 8.1 READ IT.
SELECT id, tbl, seq, stmt
FROM dbo.migration_steps WHERE script = '003' ORDER BY tbl, seq;

-- 8.2 Confirm the count matches the scope. A table missing from this
-- list has no primary key, or is versioned already.
SELECT (SELECT COUNT(*) FROM dbo.migration_targets WHERE include = 1) AS in_scope,
       (SELECT COUNT(DISTINCT tbl) FROM dbo.migration_steps WHERE script = '003')
         AS will_be_versioned;
GO


/**********************************************************************
  PART 9. RUN SCRIPT 003

  Same runner as PART 7. Change @script to '003'.
  Run once with @debug = 1, then with @debug = 0.

  A note on the transaction: for script 003 a partial run is not
  dangerous, because each table is independent. The transaction is kept
  because it makes the outcome simple: all or nothing.
**********************************************************************/

SET XACT_ABORT ON;

DECLARE @script varchar(20) = '003';
DECLARE @debug  bit         = 1;      -- <<< set to 0 to run for real

DECLARE @id int, @stmt nvarchar(max), @n int = 0;

BEGIN TRY
  BEGIN TRANSACTION;

  DECLARE c CURSOR LOCAL FAST_FORWARD FOR
    SELECT id, stmt FROM dbo.migration_steps
    WHERE script = @script AND applied_utc IS NULL
    ORDER BY tbl, seq, ord;

  OPEN c;
  FETCH NEXT FROM c INTO @id, @stmt;
  WHILE @@FETCH_STATUS = 0
  BEGIN
    PRINT @stmt;
    IF @debug = 0
    BEGIN
      EXEC sp_executesql @stmt;
      UPDATE dbo.migration_steps SET applied_utc = SYSUTCDATETIME() WHERE id = @id;
    END
    SET @n += 1;
    FETCH NEXT FROM c INTO @id, @stmt;
  END
  CLOSE c; DEALLOCATE c;

  IF @debug = 1 BEGIN ROLLBACK; PRINT '--- DEBUG: rolled back.'; END
  ELSE          BEGIN COMMIT;   PRINT '--- APPLIED ' + CAST(@n AS varchar(10)); END
END TRY
BEGIN CATCH
  IF CURSOR_STATUS('local','c') >= 0 BEGIN CLOSE c; DEALLOCATE c; END
  IF @@TRANCOUNT > 0 ROLLBACK;
  PRINT '--- FAILED ON: ' + ISNULL(@stmt, '(none)');
  THROW;
END CATCH
GO


/**********************************************************************
  PART 10. VERIFY
**********************************************************************/

-- 10.1 Every table, its history table, and its period columns.
-- Compare the two hand-migrated tables with the generated ones. They
-- must look the same.
SELECT t.name AS tbl, h.name AS history_table,
       ss.is_hidden AS start_hidden, se.is_hidden AS end_hidden,
       t.temporal_type_desc
FROM sys.tables t
LEFT JOIN sys.tables h   ON h.object_id = t.history_table_id
LEFT JOIN sys.periods p  ON p.object_id = t.object_id
LEFT JOIN sys.columns ss ON ss.object_id = t.object_id AND ss.column_id = p.start_column_id
LEFT JOIN sys.columns se ON se.object_id = t.object_id AND se.column_id = p.end_column_id
WHERE t.temporal_type = 2
ORDER BY t.name;

-- 10.2 Anything still not versioned, and why.
SELECT m.tbl, m.include, m.note,
       IIF(EXISTS (SELECT 1 FROM sys.key_constraints k
                   JOIN sys.tables t2 ON t2.object_id = k.parent_object_id
                   WHERE t2.name = m.tbl AND k.type = 'PK'), 'yes', 'NO') AS has_pk
FROM dbo.migration_targets m
WHERE NOT EXISTS (SELECT 1 FROM sys.tables t
                  WHERE t.name = m.tbl AND t.temporal_type = 2)
ORDER BY m.include DESC, m.tbl;

-- 10.3 AFTER ONE REAL LOAD: one load must give ONE timestamp.
-- If many appear, a code path is missing its transaction. Find it
-- before you go to production.
-- SELECT SysStart, COUNT(*) AS n
-- FROM dbo.my_table FOR SYSTEM_TIME ALL
-- GROUP BY SysStart ORDER BY SysStart DESC;

-- 10.4 Read a vintage. The value is UTC.
-- SELECT * FROM dbo.my_table FOR SYSTEM_TIME AS OF '2026-08-01T09:00:00';

-- 10.5 Keep the generated statements outside the database as well.
-- Use Results to File in SSMS, not the grid. The grid truncates.
SELECT stmt FROM dbo.migration_steps ORDER BY script, tbl, seq, ord;
GO


/**********************************************************************
  PART 11. REVERSE

  Turning versioning off keeps both tables. Nothing is lost. The history
  table becomes an ordinary table.

  To go further back, restore the backup. Do not try to undo the
  NOT NULL changes and the primary keys by script.
**********************************************************************/

-- Generate the statements. Read them, then run them.
SELECT 'ALTER TABLE ' + QUOTENAME(SCHEMA_NAME(t.schema_id)) + '.' + QUOTENAME(t.name)
       + ' SET (SYSTEM_VERSIONING = OFF);' AS stmt
FROM sys.tables t
WHERE t.temporal_type = 2
  AND EXISTS (SELECT 1 FROM dbo.migration_targets m
              WHERE m.tbl = t.name AND m.include = 1);
GO


/**********************************************************************
  PRODUCTION

  Do not generate again in production. The catalogue there may differ.
  Instead:

    1. Back up production.
    2. Restore that backup over development. Run this whole file again
       on the fresh copy. This is the true rehearsal, and it is the step
       people skip.
    3. Copy the rows of migration_targets and migration_steps from
       development into production, or generate and read them again with
       the same care.
    4. Run PART 7 and PART 9 with @debug = 1, read the output, then
       @debug = 0.

  Keep migration_targets and migration_steps. They are the record of
  what was done, and when.
**********************************************************************/
