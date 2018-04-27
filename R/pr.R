org_pr <- function(org) {
  # TODO: paginate repos and maybe pullRequests?
  res <- graphql_query("pullrequests.json", org = org)

  d <- res$data$organization$repositories$nodes

  has_pr <- map_int(d, ~ length(.x$pullRequests$nodes)) > 0
  d1 <- d[has_pr]

  d2 <- map_dfr(d1, function(.x) {
    res <- flatten_df(.x$pullRequests)
    res$repo <- .x$name
    res$author <- map_chr(res$author, pluck)
    res
  })

  has_review <- lengths(d2$reviews) > 0
  get_review_info <- function(x) {
       tibble(
           reviewer = map_chr(x, c("author", "login")),
           reviewedAt = map_chr(x, ~ .$submittedAt %||% NA_character_))
   }
  d3 <- bind_rows(
    mutate(d2[has_review, ], reviews = map(reviews, get_review_info)) %>% unnest(),
    select(d2[!has_review, ], -reviews))

  d3
}
