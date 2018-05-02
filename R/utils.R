#' @importFrom purrr compact
graphql_query <- function(json, ...) {
  file <- system.file(json, package = "tidyversedashboard")
  query <- readChar(file, file.info(file)$size)
  gh::gh("POST /graphql", query = query, variables = compact(list(...)))
}
