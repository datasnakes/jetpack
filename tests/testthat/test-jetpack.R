context("jetpack")

library(withr)

options(repos=list(CRAN="https://cloud.r-project.org/"))

Sys.setenv(TEST_JETPACK = "true")

contains <- function(file, x) {
  grepl(x, paste(readLines(file), collapse=""))
}

test_that("it works", {
  tryCatch({
    with_dir(tempdir(), {
      check <- jetpack::check()
      expect(check)
    })
  }
})
