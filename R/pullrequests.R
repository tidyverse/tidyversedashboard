parse_pr_comments <- function(x) {
  tibble(commenter = x$author$login %||% NA_character_,
         commented = x$publishedAt %||% NA_character_)
}

parse_pr_reviews <- function(x) {
  tibble(reviewer = x$author$login %||% NA_character_,
         reviewed = x$submittedAt %||% NA_character_)
}

parse_pr_prs <- function(x) {
  reviews <- map_dfr(x$reviews$nodes, parse_pr_reviews) %|||% tibble(reviewer = character(), reviewed = character())
  comments <- map_dfr(x$comments$nodes, parse_pr_comments) %|||% tibble(commenter = character(), commented = character())

  tibble(
    number = x$number,
    author = x$author$login %||% NA_character_,
    created = x$createdAt %||% NA_character_,
    updated = x$updatedAt %||% NA_character_,
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
org_pr <- function(org) {
  # TODO: paginate repos, comments and maybe pullRequests?
  res <- paginate(function(cursor, ...)
    graphql_query("pullrequests.graphql", org = org, cursor = cursor))
  
  mutate(
    map_dfr(res, function(x) map_dfr(x$repositoryOwner$repositories$nodes, parse_pr_repository)),
    created = parse_datetime_8601(created),
    updated = parse_datetime_8601(updated),
    reviewed = parse_datetime_8601(reviewed),
    commented = parse_datetime_8601(commented),
    owner = org)
}
