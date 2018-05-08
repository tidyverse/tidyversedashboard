#' Calculate number or reverse dependencies
#' @param package One or more packages to search
#' @importFrom purrr map_int
#' @export
reverse_dependencies <- function(package) {
  res <- tools::package_dependencies(package,
    reverse = TRUE,
    recursive = FALSE,
    which = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))

  map_int(res, ~ if (is.null(.x)) NA_integer_ else length(.x))
}


#' Lookup weekly commit counts for each repo
#' @inheritParams org_data
#' @inheritParams reverse_dependencies
#' @export
weekly_commits <- function(org, package) {
  tryCatch(
    map_int(gh::gh("/repos/:org/:package/stats/commit_activity", org = org, package = package), "total"),
    error = function(e) {
      NA_integer_
    })
}

#' Number of downloads from RStudio mirror
#' @inheritParams cranlogs::cran_downloads
#' @inheritParams reverse_dependencies
#' @importFrom dplyr group_by tally left_join
#' @export
num_downloads <- function(package, when = "last-week") {
  # only lookup packages that are valid package names
  valid_pkgs <- grepl(paste0("^", .standard_regexps()$valid_package_name, "$"), package)
  res <- cranlogs::cran_downloads(package[valid_pkgs], "last-week")
  count_per_package <- res %>% group_by(package) %>% tally(wt = count)
  out <- numeric(length(package))
  out[valid_pkgs] <- left_join(tibble(package = package[valid_pkgs]), count_per_package)$n
  out
}

utils::globalVariables("count")

#' Return the number of development dependencies in a package description file.
#' @param description A [desc::desc] object.
#' @name desc
#' @export
desc_dev_deps <- function(description) {
  map_int(description,
    ~ if (length(.x))
      length(.x$get_remotes()))
}

email_aliases <- c(
  "James Hester <james.hester@rstudio.com>" = "Jim Hester <jim.hester@rstudio.com>",
  "Jim Hester <james.f.hester@gmail.com>" = "Jim Hester <jim.hester@rstudio.com>",
  "Jim Hester <james.hester@rstudio.com>" = "Jim Hester <jim.hester@rstudio.com>",
  "Gabor Csardi <csardi.gabor@gmail.com>" = "G\u00E1bor Cs\u00E1rdi <csardi.gabor@gmail.com>")

remove_aliases <- function(x, y) {
  m <- x %in% names(y)
  x[m] <- y[x[m]]
  x
}

#' @rdname desc
#' @export
desc_maintainer <- function(description) {
  res <- map_chr(description, 
    possibly(function(.x) { .x$get_maintainer() %|||% NA_character_}, otherwise = NA_character_))

  remove_aliases(res, email_aliases)
}

#' Generate a status badge
#' @param owner The repository owner
#' @param package The package
#' @name status_badge
#' @export
travis_status_badge <- function(owner, package) {
  glue::glue('<a href="https://travis-ci.org/{owner}/{package}"><img src="https://travis-ci.org/{owner}/{package}.svg?branch=master"></a>')
}

#' @rdname status_badge
#' @export
appveyor_status_badge <- function(owner, package) {
  glue::glue('<a href="https://ci.appveyor.com/api/projects/status/github/{owner}/{package}"><img src="https://ci.appveyor.com/api/projects/status/github/{owner}/{package}?svg=true&branch=master"></a>')
}
