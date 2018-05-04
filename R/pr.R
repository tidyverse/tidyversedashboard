parse_pr_comments <- function(x) {
  tibble(commenter = x$author$login %||% NA_character_,
         commented = as_datetime(x$publishedAt %||% NA_character_))
}

#' @importFrom lubridate as_datetime
parse_pr_reviews <- function(x) {
  tibble(reviewer = x$author$login %||% NA_character_,
         reviewed = as_datetime(x$submittedAt %||% NA_character_))
}

parse_pr_prs <- function(x) {
  reviews <- map_dfr(x$reviews$nodes, parse_pr_reviews) %|||% tibble(reviewer = character(), reviewed = as_datetime(character()))
  comments <- map_dfr(x$comments$nodes, parse_pr_comments) %|||% tibble(commenter = character(), reviewed = character())

  tibble(
    number = x$number,
    author = x$author$login %||% NA_character_,
    created = as_datetime(x$createdAt %||% NA_character_),
    updated = as_datetime(x$updatedAt %||% NA_character_),
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
  res <- graphql_query("pullrequests.graphql", org = org)

  mutate(
    map_dfr(res$data$organization$repositories$nodes, parse_pr_repository),
    owner = org)
}
