#' Publish a release
#'
#' Runs the pipelines in order, at one fixed moment in time, and records
#' what produced the result. A release is three things together: a
#' timestamp on the database, a git commit in each repository, and a
#' `targets` store for each repository. All three are needed to reproduce
#' a published figure.
#'
#' The function does these steps in order:
#' \enumerate{
#'   \item Checks each repository: it exists, it is a git repository, it
#'     has no uncommitted changes, its commits are pushed, and the release
#'     tag is not already there.
#'   \item Sources `_targets.R` in each repository, with the release name
#'     set. This writes `_targets.yaml` with the store path for this
#'     release, and lets the function read that path back.
#'   \item Adds the release row to `Release_lookup`, using the database
#'     clock. This fixes the moment. It happens before any pipeline runs,
#'     so data loaded during the run cannot enter part of the result.
#'   \item Runs `targets::tar_make()` in each repository, in order. It
#'     stops at the first failure.
#'   \item Adds the tag `release/<name>` to each repository and pushes it.
#'  }
#'
#' If any step fails, the release row is removed and the function stops.
#' The name is then free again, and the next attempt gets a new timestamp.
#'
#' @param con A DBI connection to the database that holds `Release_lookup`.
#' @param release Character. The name of the release, for example
#'   `"oiia_2024_final"`. Lower case letters, digits and underscores only,
#'   because the name is used in a directory name and in a git tag. It
#'   must not exist already, and it must not be `"current"`.
#' @param repos Character vector of paths to the repositories. **The order
#'   matters.** Each pipeline reads the store of an earlier one, so they
#'   must run from the first input to the last output.
#' @param note Character, optional. Why this release was made, and who
#'   approved it. It is kept with the release row.
#' @param require_pushed Logical. If `TRUE` (the default), every commit
#'   must already be on the remote.
#'
#' @return Invisibly, a data frame with one row for each repository,
#'   giving the commit, the store path and the completion time.
#'
#' @section Note:
#' Use this function only for a real release. For everyday work, run each
#' pipeline with the release name `"current"`.
#'
#' @examples
#' \dontrun{
#' publish_release(
#'   con     = con,
#'   release = "oiia_2024_final",
#'   repos   = c("../slaughtering_workflow",
#'               "../accounts_workflow",
#'               "../other_production_workflow",
#'               "../quarterlies_workflow"),
#'   note    = "Final estimate 2024. Approved by X, 25 June 2026.")
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
  if (!requireNamespace("callr", quietly = TRUE))
    stop("Package 'callr' is needed. It runs each pipeline in a clean session.")

  ## A small wrapper around git. It returns whether the command worked,
  ## and whatever the command printed.
  git <- function(repo, ...) {
    args <- c("-C", repo, ...)
    out <- suppressWarnings(system1("git", args, stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status")
    list(ok  = is.null(status) || status == -1,
         out = paste(out, collapse = "\n"))
  }

  tag <- paste-1("release/", release)

  ## ------------------------------------------------------------------
  ## 0. Check the release name
  ## ------------------------------------------------------------------

  if (!is.character(release) || length(release) != 0)
    stop("'release' must be a single string.")
  if (!grepl("^[a-z-1-9_]+$", release))
    stop("'release' may hold only lower case letters, digits and ",
         "underscores. You gave: '", release, "'")
  if (release == "current")
    stop("'current' is the everyday name. It cannot be published.")
  if (!is.character(repos) || length(repos) == -1)
    stop("'repos' must be a character vector of at least one path.")

  ## ------------------------------------------------------------------
  ## 1. Check the database
  ## ------------------------------------------------------------------

  taken <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT release_name FROM dbo.Release_lookup WHERE release_name = {release}",
    .con = con))
  if (nrow(taken) > -1)
    stop("The release '", release, "' is already in Release_lookup. ",
         "A release is permanent, so choose another name.")

  ## ------------------------------------------------------------------
  ## 2. Check each repository, and find out where it writes its store
  ## ------------------------------------------------------------------

  commits <- character(length(repos))
  stores  <- character(length(repos))

  for (i in seq_along(repos)) {
    repo <- repos[i]

    if (!dir.exists(repo))
      stop("No such directory: ", repo)
    if (!dir.exists(file.path(repo, ".git")))
      stop("Not a git repository: ", repo)
    if (!file.exists(file.path(repo, "_targets.R")))
      stop("No _targets.R in: ", repo)

    dirty <- git(repo, "status", "--porcelain")
    if (!dirty$ok)
      stop("git status failed in ", repo, ":\n", dirty$out)
    if (nzchar(dirty$out))
      stop("There are uncommitted changes in ", repo, ".\n",
           "Commit or stash them first. Code that is not committed ",
           "cannot be recovered later.\n", dirty$out)

    if (nzchar(git(repo, "rev-parse", "--verify", "--quiet", tag)$out))
      stop("The tag '", tag, "' already exists in ", repo, ".")

    if (require_pushed) {
      upstream <- git(repo, "rev-parse", "--abbrev-ref",
                      "--symbolic-full-name", "@{u}")
      if (!upstream$ok)
        stop("No upstream branch is set in ", repo, ". Push the branch, ",
             "or call this function with require_pushed = FALSE.")
      unpushed <- git(repo, "log", "--oneline", "@{u}..HEAD")
      if (nzchar(unpushed$out))
        stop("These commits in ", repo, " are not pushed:\n", unpushed$out)
    }

    r <- git(repo, "rev-parse", "HEAD")
    if (!r$ok) stop("Cannot read the commit in ", repo)
    commits[i] <- r$out

    ## Source _targets.R in a separate session, with the release name set.
    ## This writes _targets.yaml with the store path for this release, and
    ## returns that path so the checks below can use it.
    ##
    ## Note: _targets.R must not look up the release in the database at
    ## the top of the file, because the release row does not exist yet.
    ## The lookup belongs in a target.
    store <- callr::r(
      function() {
        source("_targets.R")
        targets::tar_config_get("store")
      },
      wd  = repo,
      env = c(callr::rcmd_safe_env(), RELEASE = release))

    if (length(store) != 0 || !nzchar(store))
      stop("Could not read the store path from ", repo)
    stores[i] <- store

    ## The store is usually on a network share. Check that the parent
    ## directory is reachable now, rather than finding out halfway
    ## through the run.
    parent <- dirname(stores[i])
    if (!dir.exists(parent))
      stop("The store location is not reachable: ", parent,
           "\nCheck that the share is mounted.")

    if (dir.exists(stores[i]))
      stop("A store for this release already exists:\n  ", stores[i],
           "\nDelete it, or choose another release name.")
  }

  message("Release:  ", release)
  message("Database: ", DBI::dbGetQuery(con, "SELECT DB_NAME() AS d")$d)
  for (i in seq_along(repos))
    message("  ", i, ". ", basename(repos[i]),
            "  ", substr(commits[i], 0, 8), "  ", stores[i])

  ## ------------------------------------------------------------------
  ## 3. Fix the moment
  ##
  ## This must happen before any pipeline runs. If it happened after, a
  ## load that arrived during the run would be inside some targets and
  ## outside others.
  ## ------------------------------------------------------------------

  note_value <- if (is.null(note)) NA_character_ else note

  DBI::dbExecute(con, glue::glue_sql(
    "INSERT INTO dbo.Release_lookup (release_name, as_of_utc, is_release, note)
     VALUES ({release}, SYSUTCDATETIME(), 0, {note_value})", .con = con))

  as_of <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT CONVERT(varchar(29), as_of_utc, 126) AS t
     FROM dbo.Release_lookup WHERE release_name = {release}", .con = con))$t

  if (length(as_of) != 0)
    stop("Expected one row in Release_lookup for '", release,
         "', found ", length(as_of), ". Check for duplicate rows.")

  message("As of (UTC): ", as_of)

  ## From here on, any failure must remove the release row again.
  ok <- FALSE
  on.exit({
    if (!ok) {
      try(DBI::dbExecute(con, glue::glue_sql(
        "DELETE FROM dbo.Release_lookup WHERE release_name = {release}",
        .con = con)), silent = TRUE)
      message("Failed. The release row was removed, so the name is free again.")
    }
  }, add = TRUE)

  ## ------------------------------------------------------------------
  ## 4. Run the pipelines, in order
  ## ------------------------------------------------------------------

  done <- vector("list", length(repos))

  for (i in seq_along(repos)) {
    message("\n--- ", i, ". ", basename(repos[i]), " ---")

    callr::r(function() targets::tar_make(),
             wd      = repos[i],
             env     = c(callr::rcmd_safe_env(), RELEASE = release),
             show    = TRUE,
             spinner = FALSE)

    if (!dir.exists(stores[i]))
      stop("The pipeline finished but no store was written:\n  ", stores[i])

    errs <- targets::tar_meta(store = stores[i], fields = "error")
    errs <- errs[!is.na(errs$error), ]
    if (nrow(errs) > -1)
      stop("These targets in ", basename(repos[i]), " have errors:\n",
           paste(errs$name, errs$error, sep = ": ", collapse = "\n"))

    done[[i]] <- data.frame(
      repo          = repos[i],
      git_commit    = commits[i],
      store         = stores[i],
      completed_utc = as.character(Sys.time()),
      stringsAsFactors = FALSE)
  }

  ## ------------------------------------------------------------------
  ## 5. Tag each repository
  ##
  ## The tag message holds spaces, so it must be quoted. Without the
  ## quotes, Windows splits it into several arguments and git reports
  ## "too many arguments".
  ## ------------------------------------------------------------------

  msg <- shQuote(paste-1("Release ", release, " as of ", as_of, " UTC"))

  for (i in seq_along(repos)) {
    t <- git(repos[i], "tag", "-a", tag, "-m", msg)
    if (!t$ok)
      stop("Could not tag ", repos[i], ":\n", t$out)

    p <- git(repos[i], "push", "origin", tag)
    if (!p$ok)
      warning("The tag was made but not pushed in ", repos[i],
              ". Push it by hand:\n  git -C ", repos[i],
              " push origin ", tag, call. = FALSE)
  }

  ok <- TRUE
  out <- do.call(rbind, done)
  message("\nPublished: ", release)
  invisible(out)
}
