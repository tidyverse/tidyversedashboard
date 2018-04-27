#' @importFrom purrr map_chr
parse_issue <- function(x) {
  labels <- map_chr(x$labels$nodes, "name")

  list(p1 = x$p1$totalCount %||% 0, bugs = "bug" %in% labels, features = "feature" %in% labels, unlabeled = !length(labels))
}
parse_repository <- function(x) {
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

#' Compute an organization summary
#' @param org A GitHub organization name
#' @export
org_summary <- function(org) {
  res <- graphql_query("summary.json", org = org)
  map_dfr(res$data$organization$repositories$nodes, parse_repository)
}
