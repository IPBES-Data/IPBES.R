# tests/testthat/test-doi_exists.R

test_that("doi_exists() returns correct results", {
  skip_on_cran() # Skip this test on CI systems
  dois <- c(
    "sbcd1234",
    "10.1234/abcd",
    "https://doi.org/10.1002/jcb.23190",
    "10.47366/sabia.v5n1a3",
    "10.1002/ece3.10852",
    "10.1002/ece3.10853",
    "10.1002/ece3.10854",
    "10.1002/ece3.10855",
    "10.1002/ece3.10856",
    "10.1002/ece3.10857"
  )
  
  # Test with doi.org
  result_doi_org <- doi_exists(dois, use = "doi.org")
  expect_equal(length(result_doi_org), length(dois))
  expect_type(result_doi_org, "logical")
  expect_named(result_doi_org)
  expect_false(result_doi_org["sbcd1234"])
  expect_false(result_doi_org["10.1234/abcd"])
  expect_true(result_doi_org["10.1002/jcb.23190"])
  expect_true(result_doi_org["10.47366/sabia.v5n1a3"])
  expect_true(result_doi_org["10.1002/ece3.10852"])
  expect_true(result_doi_org["10.1002/ece3.10853"])
  expect_true(result_doi_org["10.1002/ece3.10854"])
  expect_true(result_doi_org["10.1002/ece3.10855"])
  expect_true(result_doi_org["10.1002/ece3.10856"])
  expect_true(result_doi_org["10.1002/ece3.10857"])
  
  # Test with OpenAlex
  result_openalex <- doi_exists(dois, use = "OpenAlex")
  expect_equal(length(result_openalex), length(dois))
  expect_type(result_openalex, "logical")
  expect_named(result_openalex)
  expect_false(result_openalex["sbcd1234"])
  expect_false(result_openalex["10.1234/abcd"])
  expect_true(result_openalex["10.1002/jcb.23190"])
  expect_true(result_openalex["10.47366/sabia.v5n1a3"])
  expect_true(result_openalex["10.1002/ece3.10852"])
  expect_true(result_openalex["10.1002/ece3.10853"])
  expect_true(result_openalex["10.1002/ece3.10854"])
  expect_true(result_openalex["10.1002/ece3.10855"])
  expect_true(result_openalex["10.1002/ece3.10856"])
  expect_true(result_openalex["10.1002/ece3.10857"])
})

test_that("doi_exists() handles NULL and empty dois", {
  skip_on_cran() # Skip this test on CI systems
  expect_warning(result_doi_org <- doi_exists(NULL, use = "doi.org"), "dois are NULL")
  expect_equal(result_doi_org, logical(0))
  expect_warning(result_openalex <- doi_exists(NULL, use = "OpenAlex"), "dois are NULL")
  expect_equal(result_openalex, logical(0))

  expect_warning(result_doi_org <- doi_exists(character(0), use = "doi.org"), "dois of length zero")
  expect_equal(result_doi_org, logical(0))
  expect_warning(result_openalex <- doi_exists(character(0), use = "OpenAlex"), "dois of length zero")
  expect_equal(result_openalex, logical(0))
})

test_that("doi_exists() handles dois with and without resolver", {
  skip_on_cran() # Skip this test on CI systems
  dois <- c("https://doi.org/10.1002/jcb.23190", "10.1002/jcb.23190")
  result_doi_org <- doi_exists(dois, use = "doi.org")
  expect_true(result_doi_org["10.1002/jcb.23190"])
  result_openalex <- doi_exists(dois, use = "OpenAlex")
  expect_true(result_openalex["10.1002/jcb.23190"])
})

test_that("doi_exists() print a warning if cache_file is used", {
  skip_on_cran() # Skip this test on CI systems
  expect_warning(doi_exists(dois = "10.1002/jcb.23190", cache_file = "cache.rds", use = "doi.org"), "Cache file deprecated")
  expect_warning(doi_exists(dois = "10.1002/jcb.23190", cache_file = "cache.rds", use = "OpenAlex"), "Cache file deprecated")
})

test_that("doi_exists() works with only non existing dois", {
  skip_on_cran() # Skip this test on CI systems
  dois <- c("sbcd1234", "10.1234/abcd")
  result <- doi_exists(dois)
  expect_equal(length(result), length(dois))
  expect_type(result, "logical")
  expect_named(result)
  expect_false(result["sbcd1234"])  
  expect_false(result["10.1234/abcd"])
  
  result_openalex <- doi_exists(dois, use = "OpenAlex")
  expect_equal(length(result_openalex), length(dois))
  expect_type(result_openalex, "logical")
  expect_named(result_openalex)
  expect_false(result_openalex["sbcd1234"])
  expect_false(result_openalex["10.1234/abcd"])
})
