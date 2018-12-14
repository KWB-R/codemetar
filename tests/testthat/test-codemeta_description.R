context("codemeta_description.R")

# Helper functions
example_description <- function(x) codemeta_description(example_file(x))

check_maintainer_and_n_authors <- function(x, name, n) {
  expect_true(x$maintainer[[1]]$familyName == name)
  expect_equal(length(x$author), n)
}

# String constants
url_1 <- "https://doi.org/10.looks.like/doi"
url_2 <- "https://github.com/ropensci/essurvey"
url_3 <- "https://ropensci.github.io/essurvey/"
url_4 <- "https://github.com/ropensci/codemetar"
url_5 <- "https://ropensci.github.io/codemetar"
url_6 <- "https://github.com/ropensci/codemetar#codemetar"
url_7 <- "https://ropensci.org"

test_that("We can use a preset id", {
  codemeta_description(package_file("codemetar", "DESCRIPTION"), id = url_1)
})

test_that("several URLs", {
  codemeta <- example_description("DESCRIPTION_two_URLs")
  expect_equal(codemeta$codeRepository, url_2)
  expect_true(url_3 %in% codemeta$relatedLink)
})

test_that("no direct link to README", {
  codemeta <- example_description("DESCRIPTION_good_readmeinurl")
  expect_equal(codemeta$codeRepository, url_4)
  expect_true(all(c(url_5, url_6) %in% codemeta$relatedLink))
})

test_that("We can parse additional terms", {
  codemeta <- example_description("DESCRIPTION_ex1.dcf")
  expect_equal(length(codemeta$keywords), 6)
  expect_equal(codemeta$isPartOf, url_7)
})

test_that("We can parse plain Authors: & Maintainers: entries", {

  example_description("DESCRIPTION_ex1.dcf") %>%
    check_maintainer_and_n_authors("Boettiger", 0)

  example_description("example.dcf") %>%
    check_maintainer_and_n_authors("Developer", 2)

  example_description("DESCRIPTION_plainauthors") %>%
    check_maintainer_and_n_authors("Ok", 2)

  authors <- example_description("DESCRIPTION_twomaintainers")
  expect_equal(length(authors$author), 1)
  expect_equal(length(authors$maintainer), 2)
})

test_that("Helper functions work correctly", {

  expect_fields <- function(x, fields) expect_true(all(fields %in% names(x)))

  # Provide testdata
  codemeta <- new_codemeta()
  codemeta$package <- "abc"
  description <- desc::desc(example_file("DESCRIPTION_good"))

  # test add_repository_terms()
  expect_error(add_repository_terms())
  add_repository_terms(codemeta, description) %>%
    expect_fields(c("codeRepository", "relatedLink"))

  # test add_language_terms()
  expect_error(add_language_terms())
  add_language_terms(codemeta) %>%
    expect_fields(c("programmingLanguage", "runtimePlatform"))

  # test add_person_terms()
  expect_error(add_person_terms())
  add_person_terms(codemeta, description) %>%
    expect_fields(c(
      "author", "contributor", "copyrightHolder", "funder", "maintainer"
    ))

  # test add_software_terms
  expect_error(add_software_terms())
  add_software_terms(codemeta, description) %>%
    expect_fields(c("softwareSuggestions", "softwareRequirements"))

  # test add_remote_provider()
  expect_error(add_remote_provider())
  expect_error(add_remote_provider(codemeta))
  remotes <- sprintf("provider%d/abc", 1:2)
  result <- add_remote_provider(codemeta, remotes)
  expect_identical(result$remote_provider, remotes)

  # test add_additional_terms()
  expect_error(add_additional_terms())
  add_additional_terms(codemeta, description) %>%
    expect_fields(c("isPartOf", "keywords"))
})
