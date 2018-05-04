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

#' Compute an organization summary
#' @param org A GitHub user, either a normal user or an organization
#' @export
org_data <- function(org) {
  res <- graphql_query("repo_summary.graphql", org = org)

  summary <- map_dfr(res$data$repositoryOwner$repositories$nodes, parse_summary_repository)
  issues <- map_dfr(res$data$repositoryOwner$repositories$nodes, parse_issues_repository)
  list(summary = summary, issues = issues)
}
