paginate_issues <- function(x, f, issues) {
  has_next_page <- x$open_issues$pageInfo$hasNextPage
  cursor <- x$open_issues$pageInfo$endCursor
  while (has_next_page) {
    res <- graphql_query("issues.graphql", owner = x$owner$login, repo = x$repo, cursor = cursor)
    issues <- bind_rows(issues,
      map_dfr(res$data$repository$open_issues$nodes, f))

    has_next_page <- res$data$repository$open_issues$pageInfo$hasNextPage
    cursor <- res$data$repository$open_issues$pageInfo$endCursor
  }
  issues
}

#' @importFrom purrr map_chr
parse_summary_issue <- function(x) {
  labels <- map_chr(x$labels$nodes, "name")

  list(p1 = x$p1$totalCount %||% 0, bugs = "bug" %in% labels, features = "feature" %in% labels, unlabeled = !length(labels))
}
parse_summary_repository <- function(x) {
  if (x$open_issues$totalCount == 0) {
    issues <- list(p1 = 0, bugs = FALSE, features = FALSE, unlabeled = FALSE)
  } else {
    issues <- map_dfr(x$open_issues$nodes, parse_summary_issue)
  }

  issues <- paginate_issues(x, parse_summary_issue, issues)

  tibble::tibble(
    owner = x$owner$login,
    repo = x$repo,
    prs = x$prs$totalCount,
    watchers = x$watchers$totalCount,
    open_issues = x$open_issues$totalCount,
    p1 = sum(issues$p1),
    bugs = sum(issues$bugs),
    features = sum(issues$features),
    unlabeled = sum(issues$unlabeled),
    description = list(desc::desc(text = x$DESCRIPTION$text %||% character())))
}


parse_issues_issue <- function(x) {
  labels <- map_chr(x$labels$nodes, "name")

  list(issue = x$number, title = x$title, updated = as_datetime(x$updatedAt), p1 = x$p1$totalCount %||% 0, labels = list(labels))
}

#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
parse_issues_repository <- function(x) {
  if (x$open_issues$totalCount == 0) {
    return(list(issue = numeric(), title = character(), updated = as_datetime(character()), p1 = integer(), labels = list()))
  } else {
    issues <- map_dfr(x$open_issues$nodes, parse_issues_issue)
  }

  issues <- paginate_issues(x, parse_issues_issue, issues)
  issues %>%
    mutate(owner = x$owner$login, repo = x$repo) %>%
    select(owner, repo, everything())
}
utils::globalVariables("owner")

#' Compute an organization summary
#' @param org A GitHub organization name
#' @export
org_data <- function(org) {
  res <- graphql_query("repo_summary.graphql", org = org)

  summary <- map_dfr(res$data$organization$repositories$nodes, parse_summary_repository)
  issues <- map_dfr(res$data$organization$repositories$nodes, parse_issues_repository)
  list(summary = summary, issues = issues)
}


#' Calculate number or reverse dependencies
#' @param package One or more packages to search
#' @importFrom purrr map_int
#' @export
reverse_dependencies <- function(package) {
  res <- tools::package_dependencies(package,
    reverse = TRUE,
    recursive = FALSE,
    which = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))

  map_int(res, ~ if (is.null(.x)) NA_integer_ else length(.x))
}


#' Lookup weekly commit counts for each repo
#' @inheritParams org_data
#' @inheritParams reverse_dependencies
#' @export
weekly_commits <- function(org, package) {
  tryCatch(
    map_int(gh::gh("/repos/:org/:package/stats/commit_activity", org = org, package = package), "total"),
    error = function(e) {
      NA_integer_
    })
}

#' Number of downloads from RStudio mirror
#' @inheritParams cranlogs::cran_downloads
#' @inheritParams reverse_dependencies
#' @importFrom dplyr group_by tally left_join
#' @export
num_downloads <- function(package, when = "last-week") {
  # only lookup packages that are valid package names
  valid_pkgs <- grepl(paste0("^", .standard_regexps()$valid_package_name, "$"), package)
  res <- cranlogs::cran_downloads(package[valid_pkgs], "last-week")
  count_per_package <- res %>% group_by(package) %>% tally(wt = count)
  out <- numeric(length(package))
  out[valid_pkgs] <- left_join(tibble(package = package[valid_pkgs]), count_per_package)$n
  out
}

utils::globalVariables("count")
