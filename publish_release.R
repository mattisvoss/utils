#' Publish a release
#'
#' Runs the pipelines in order, at one frozen moment in time, and records
#' what produced the result. A release is three things together: a
#' timestamp on the database, a git commit for each repository, and a
#' `targets` store for each repository. All three are needed to reproduce
#' a published figure.
#'
#' The function does these steps in order:
#' \enumerate{
#'   \item Checks the release name, the repositories, and the database.
#'     Nothing changes until every check passes.
#'   \item Adds the release row to `Release_lookup`, with the database
#'     clock. This freezes the moment. It happens before any pipeline
#'     runs, so a load that arrives during the run cannot enter part of
#'     the result.
#'   \item Runs `targets::tar_make()` in each repository, in the given
#'     order, with the release name in the environment. It stops at the
#'     first failure.
#'   \item Records the commit of each repository in `Release_component`,
#'     and adds the tag `release/<name>` to each repository.
#' }
#'
#' If any step fails, the function removes the release row and stops. The
#' name is then free again, and the next attempt gets a new timestamp. A
#' partly published release is worse than no release, because the stores
#' look complete.
#'
#' @param con A DBI connection to the database that holds `Release_lookup`.
#' @param release Character. The name of the release, for example
#'   `"oiia_2025_final"`. Lower case letters, digits and underscores only,
#'   because the name is used for a directory and for a git tag. It must
#'   not exist already, and it must not be `"current"`.
#' @param repos Character vector of paths to the repositories. **The order
#'   matters.** Each pipeline reads the store of the one before it, so
#'   they must run from the first input to the last output. For example
#'   `c("../slaughterings", "../accounts", "../quarterlies")`.
#' @param note Character, optional. Why this release was made, and who
#'   approved it. It is kept with the release row.
#' @param require_pushed Logical. If `TRUE` (the default), every commit
#'   must already be on the remote. A commit that exists only on your
#'   machine is not a record.
#'
#' @return Invisibly, a data frame with one row for each repository,
#'   giving the commit, the store path and the completion time.
#'
#' @section Warning:
#' Use this function only for a true release. For everyday work, run each
#' pipeline with the release name `"current"`.
#'
#' @examples
#' \dontrun{
#' publish_release(
#'   con     = con,
#'   release = "oiia_2025_final",
#'   repos   = c("../slaughterings", "../accounts", "../quarterlies"),
#'   note    = "Final estimate 2025. Approved by X, 25 June 2026.")
#' }
#'
#' @seealso [lookup_release()], [tbl_as_of()]
#' @export
publish_release <- function(con,
                            release,
                            repos,
                            note = NULL,
                            require_pushed = TRUE) {

  stopifnot(inherits(con, "DBIConnection"), DBI::dbIsValid(con))
  requireNamespace("callr", quietly = TRUE) ||
    stop("Package 'callr' is needed. It runs each pipeline in a clean session.")

  git <- function(repo, ...) {
    out <- suppressWarnings(
      system2("git", c("-C", repo, ...), stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status")
    list(ok = is.null(status) || status == 0, out = paste(out, collapse = "\n"))
  }

  store_of <- function(repo, release) {
    file.path(repo, paste0("_targets_", release))
  }

  tag <- paste0("release/", release)

  ## -- 1. CHECKS. Nothing changes until all of these pass. ---------------

  if (!is.character(release) || length(release) != 1)
    stop("'release' must be a single string.")
  if (!grepl("^[a-z0-9_]+$", release))
    stop("'release' may hold only lower case letters, digits and underscores: '",
         release, "'")
  if (release == "current")
    stop("'current' is the everyday name. It cannot be published.")

  if (!is.character(repos) || length(repos) == 0)
    stop("'repos' must be a character vector of at least one path.")

  for (repo in repos) {
    if (!dir.exists(repo))
      stop("No such directory: ", repo)
    if (!dir.exists(file.path(repo, ".git")))
      stop("Not a git repository: ", repo)
    if (!file.exists(file.path(repo, "_targets.R")))
      stop("No _targets.R in: ", repo)

    dirty <- git(repo, "status", "--porcelain")
    if (!dirty$ok) stop("git status failed in ", repo, ": ", dirty$out)
    if (nzchar(dirty$out))
      stop("Uncommitted changes in ", repo, ".\n",
           "Commit or stash them. Code you cannot recover is not a record.\n",
           dirty$out)

    if (nzchar(git(repo, "rev-parse", "--verify", "--quiet", tag)$out))
      stop("The tag '", tag, "' already exists in ", repo, ".")

    if (require_pushed) {
      upstream <- git(repo, "rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}")
      if (!upstream$ok)
        stop("No upstream branch set in ", repo,
             ". Push the branch, or set require_pushed = FALSE.")
      unpushed <- git(repo, "log", "--oneline", "@{u}..HEAD")
      if (nzchar(unpushed$out))
        stop("Commits in ", repo, " are not pushed:\n", unpushed$out)
    }

    if (dir.exists(store_of(repo, release)))
      stop("A store for this release already exists: ", store_of(repo, release),
           "\nDelete it, or choose another name.")
  }

  taken <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT release_name FROM dbo.Release_lookup WHERE release_name = {release}",
    .con = con))
  if (nrow(taken) > 0)
    stop("The release '", release, "' already exists in Release_lookup.\n",
         "A release is permanent. Choose another name.")

  used <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT component FROM dbo.Release_component WHERE release_name = {release}",
    .con = con))
  if (nrow(used) > 0)
    stop("Release_component already holds rows for '", release, "'.")

  commits <- vapply(repos, function(repo) {
    r <- git(repo, "rev-parse", "HEAD")
    if (!r$ok) stop("Cannot read the commit in ", repo)
    r$out
  }, character(1))

  message("Release:      ", release)
  message("Database:     ", DBI::dbGetQuery(con, "SELECT DB_NAME() AS d")$d)
  for (i in seq_along(repos))
    message("  ", basename(repos[i]), "  ", substr(commits[i], 1, 8))

  ## -- 2. FREEZE THE MOMENT ----------------------------------------------
  ## This must happen before any pipeline runs. If it happened after, a
  ## load that arrived during the run would be inside some targets and
  ## outside others.

  DBI::dbExecute(con, glue::glue_sql(
    "INSERT INTO dbo.Release_lookup (release_name, as_of_utc, is_release, note)
     VALUES ({release}, SYSUTCDATETIME(), 1, {note %||% NA_character_})",
    .con = con))

  as_of <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT CONVERT(varchar(30), as_of_utc, 126) AS t
     FROM dbo.Release_lookup WHERE release_name = {release}", .con = con))$t
  message("As of (UTC):  ", as_of)

  ## From here on, any failure must remove the release row again.
  ok <- FALSE
  on.exit({
    if (!ok) {
      DBI::dbExecute(con, glue::glue_sql(
        "DELETE FROM dbo.Release_component WHERE release_name = {release}",
        .con = con))
      DBI::dbExecute(con, glue::glue_sql(
        "DELETE FROM dbo.Release_lookup WHERE release_name = {release}",
        .con = con))
      message("Failed. The release row was removed. The name is free again.")
    }
  }, add = TRUE)

  ## -- 3. RUN THE PIPELINES, IN ORDER -------------------------------------
  ## callr runs each one in a clean R session, with its own working
  ## directory and its own environment. This also works on Windows, where
  ## the 'env' argument of system2() is ignored.

  done <- vector("list", length(repos))

  for (i in seq_along(repos)) {
    repo <- repos[i]
    message("\n--- ", basename(repo), " ---")

    callr::r(
      function() targets::tar_make(),
      wd     = repo,
      env    = c(callr::rcmd_safe_env(), RELEASE = release),
      show   = TRUE,
      spinner = FALSE)

    store <- store_of(repo, release)
    if (!dir.exists(store))
      stop("The pipeline finished but no store was written: ", store,
           "\nCheck that _targets.R sets the store from the RELEASE variable.")

    errs <- targets::tar_meta(store = store, fields = "error")
    errs <- errs[!is.na(errs$error), ]
    if (nrow(errs) > 0)
      stop("Targets with errors in ", basename(repo), ":\n",
           paste(errs$name, errs$error, sep = ": ", collapse = "\n"))

    done[[i]] <- data.frame(
      component     = basename(repo),
      git_commit    = commits[i],
      store         = store,
      completed_utc = as.character(Sys.time()),
      stringsAsFactors = FALSE)
  }

  ## -- 4. RECORD WHAT PRODUCED IT -----------------------------------------

  for (i in seq_along(repos)) {
    DBI::dbExecute(con, glue::glue_sql(
      "INSERT INTO dbo.Release_component (release_name, component, git_commit)
       VALUES ({release}, {basename(repos[i])}, {commits[i]})", .con = con))

    t <- git(repos[i], "tag", "-a", tag, "-m",
             paste0("Release ", release, " as of ", as_of))
    if (!t$ok) stop("Could not tag ", repos[i], ": ", t$out)

    p <- git(repos[i], "push", "origin", tag)
    if (!p$ok)
      warning("The tag was made but not pushed in ", repos[i],
              ". Push it by hand: git -C ", repos[i], " push origin ", tag,
              call. = FALSE)
  }

  ok <- TRUE
  out <- do.call(rbind, done)
  message("\nPublished: ", release)
  invisible(out)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
