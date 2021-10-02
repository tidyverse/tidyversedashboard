res <- paginate(function(cursor, ...) 
  graphql_query("repo_summary.graphql", org = "ThinkR-open", cursor = cursor,
   privacy = "PUBLIC")
)



x <- res$data$repositoryOwner$repositories$nodes[[1]]
one_parse <- parse_summary_repository(x)

the_org <- org_data(org = "ThinkR-open", privacy = "PUBLIC")

test_that("multiplication works", {
  expect_equal(ncol(one_parse), 8)
  expect_equal(ncol(the_org$summary), 12)
})
