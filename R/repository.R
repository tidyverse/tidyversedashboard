#' @importFrom purrr map_chr
parse_summary_issue <- function(x) {
  labels <- map_chr(x$labels$nodes, "name")

  list(p1 = x$p1$totalCount %||% 0, bugs = "bug" %in% labels, features = "feature" %in% labels, unlabeled = !length(labels))
}
parse_summary_repository <- function(x) {
  tibble::tibble(
    owner = x$owner$login,
    repo = x$repo,
    prs = x$prs$totalCount,
    watchers = x$watchers$totalCount,
    open_issues = x$open_issues$totalCount,
    description = list(desc::desc(text = x$DESCRIPTION$text %||% character())))
}

#' Compute an organization summary
#' @param org A GitHub user, either a normal user or an organization
#' @param privacy The repository privacy
#' @export
#' @importFrom dplyr group_by summarize left_join add_row
#' @importFrom tidyr replace_na
org_data <- function(org, privacy = c("PUBLIC", "PRIVATE", "BOTH")) {
  privacy <- normalize_privacy(privacy)

  res <- paginate(function(cursor, ...) graphql_query("repo_summary.graphql", org = org, cursor = cursor, privacy = privacy))
  summary <- map_dfr(res, function(x) map_dfr(x$repositoryOwner$repositories$nodes, parse_summary_repository))
  issues <- map_dfr(res, function(x) map_dfr(x$repositoryOwner$repositories$nodes, parse_issues_repository))

  if (nrow(issues) == 0) {
    issues <- issues %>% 
      add_row() %>% 
      mutate(
        owner = org,
        repo = NA
      )
  }
  
  summary <- left_join(
    summary,
    issues %>%
      group_by(owner, repo) %>%
      summarize(p1 = sum(p1),
                bugs = num_label(labels, "bug"),
                features = num_label(labels, "feature"),
                unlabeled = sum(lengths(labels) == 0))) %>%
    replace_na(list(p1 = 0, bugs = 0, features = 0, unlabeled = 0))

  list(summary = summary, issues = issues)
}
num_label <- function(x, label) {
  sum(map_lgl(x, ~ any(.x == label)))
}
