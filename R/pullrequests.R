parse_pr_comments <- function(x) {
  tibble(commenter = x$author$login %||% NA_character_,
         commented = parse_datetime_8601(x$publishedAt %||% NA_character_))
}

parse_pr_reviews <- function(x) {
  tibble(reviewer = x$author$login %||% NA_character_,
         reviewed = parse_datetime_8601(x$submittedAt %||% NA_character_))
}

parse_pr_prs <- function(x) {
  reviews <- map_dfr(x$reviews$nodes, parse_pr_reviews) %|||% tibble(reviewer = character(), reviewed = parse_datetime_8601(character()))
  comments <- map_dfr(x$comments$nodes, parse_pr_comments) %|||% tibble(commenter = character(), commented = parse_datetime_8601(character()))

  tibble(
    issue = x$number,
    author = x$author$login %||% NA_character_,
    created = parse_datetime_8601(x$createdAt %||% NA_character_),
    updated = parse_datetime_8601(x$updatedAt %||% NA_character_),
    reviews = list(reviews),
    comments = list(comments))
}

#' @importFrom purrr %||% set_names map_dfr
#' @importFrom tidyr unnest
#' @importFrom tibble tibble
parse_pr_repository <- function(x) {
  res <- map_dfr(x$pullRequests$nodes, parse_pr_prs)
  if (length(res)) {
    res$repo <- x$name
  }
  res
}

utils::globalVariables("repo")

#' Compute an pull request reviewer summary
#' @inheritParams org_data
#' @importFrom dplyr select everything
#' @export
org_pr <- function(org, privacy = c("PUBLIC", "PRIVATE", "BOTH")) {
  privacy <- normalize_privacy(privacy)

  res <- paginate(function(cursor, ...)
    graphql_query("pullrequests.graphql", org = org, cursor = cursor, privacy = privacy))

  mutate(
    map_dfr(res, function(x) map_dfr(x$repositoryOwner$repositories$nodes, parse_pr_repository)),
    owner = org)
}
