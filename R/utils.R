#' @importFrom purrr compact
graphql_query <- function(json, ...) {
  file <- system.file(json, package = "tidyversedashboard")
  query <- readChar(file, file.info(file)$size)
  gh::gh("POST /graphql", query = query, variables = compact(list(...)))
}

#' A wrapper around [DT::datatable] to change some defaults
#' @importFrom DT datatable
#' @inheritParams DT::datatable
#' @export
# A datatable with some common options set
data_table <- function(data, options = list(), ..., filter = "top", style = "bootstrap", autoHideNavigation = TRUE, rownames = FALSE) {
    default_opts <- list(pageLength = 25,
      dom = "tip",
      columnDefs = list(
        list(targets = "_all", orderSequence = c("desc", "asc"))))

    options <- modifyList(options, default_opts)
  datatable(data, ..., options = options, filter = filter, style = style, autoHideNavigation = autoHideNavigation, rownames = rownames)
}

`%|||%` <- function(x, y) if (length(x)) x else y

#' @importFrom lubridate fast_strptime
parse_datetime_8601 <- function(x) {
  fast_strptime(x, format = "%Y-%m-%dT%H:%M:%SZ")
}
