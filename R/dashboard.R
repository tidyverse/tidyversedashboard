#' @importFrom purrr map_chr
parse_summary_issue <- function(x) {
  labels <- map_chr(x$labels$nodes, "name")

  list(p1 = x$p1$totalCount %||% 0, bugs = "bug" %in% labels, features = "feature" %in% labels, unlabeled = !length(labels))
}
parse_summary_repository <- function(x) {
  if (x$open_issues$totalCount == 0) {
    issues <- list(p1 = 0, bugs = FALSE, features = FALSE, unlabeled = FALSE)
  } else {
    issues <- map_dfr(x$open_issues$nodes, parse_issue)
  }
  if (x$open_issues$pageInfo$hasNextPage) {
    message(x$repo, " has more than 100 open issues!")
  }
  #message(x$repo)

  tibble::tibble(
    owner = x$owner$login,
    repo = x$repo,
    prs = x$prs$totalCount,
    watchers = x$watchers$totalCount,
    open_issues = x$open_issues$totalCount,
    p1 = sum(issues$p1),
    bugs = sum(issues$bugs),
    features = sum(issues$features),
    unlabeled = sum(issues$unlabeled))
}


parse_issues_issue <- function(x) {
  labels <- map_chr(x$labels$nodes, "name")

  list(issue = x$number, title = x$title, updated = as_datetime(x$updatedAt), p1 = x$p1$totalCount %||% 0, labels = list(labels))
}

#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
parse_issues_repository <- function(x) {
  if (x$open_issues$totalCount == 0) {
    return(list(org = character(), repo = character(), issue = numeric(), title = character(), p1 = 0, labels = list()))
  } else {
    issues <- map_dfr(x$open_issues$nodes, parse_issues_issue)
  }
  has_next_page <- x$open_issues$pageInfo$hasNextPage
  cursor <- x$open_issues$pageInfo$endCursor
  while (has_next_page) {
    res <- graphql_query("issues.graphql", owner = x$owner$login, repo = x$repo, cursor = cursor)
    issues <- bind_rows(issues,
      map_dfr(res$data$repository$open_issues$nodes, parse_issues_issue))

    has_next_page <- res$data$repository$open_issues$pageInfo$hasNextPage
    cursor <- res$data$repository$open_issues$pageInfo$endCursor
  }
  mutate(issues, owner = x$owner$login, repo = x$repo)
}

#' Compute an organization summary
#' @param org A GitHub organization name
#' @export
org_summary <- function(org) {
  res <- graphql_query("repo_summary.graphql", org = org)

  summary <- map_dfr(res$data$organization$repositories$nodes, parse_summary_repository)
  issues <- map_dfr(res$data$organization$repositories$nodes, parse_issues_repository)
  #list(summary = map_dfr(res$data$organization$repositories$nodes, parse_repository),
    #commits = map)
}
