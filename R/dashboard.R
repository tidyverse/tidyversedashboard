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

#' Compute an organization summary
#' @param org A GitHub organization name
#' @export
org_data <- function(org) {
  res <- graphql_query("repo_summary.graphql", org = org)

  summary <- map_dfr(res$data$organization$repositories$nodes, parse_summary_repository)
  issues <- map_dfr(res$data$organization$repositories$nodes, parse_issues_repository)
  list(summary = summary, issues = issues)
}

