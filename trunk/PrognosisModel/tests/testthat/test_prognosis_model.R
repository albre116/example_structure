
test_that("Hap to Geno frequency", {
  # Setup
  h1 <- 3.392431e-05
  h2 <- 5.401105e-07

  # run
  gf <- haplotypeToGenotypeFrequency(h1, h2,offset_fit)

  # validate
  expect_that(gf, is_more_than(0.0))
})

test_that("get prognosis", {
  # Setup
  h1 <- 3.392431e-05
  h2 <- 5.401105e-07
  race <- "CAU"

  # run
  results <- calculatePrognosis(h1, h2, race)

  # validate
  expect_false(is.null(results))
  expect_named(results, c('Good', 'Fair', 'Poor'))
  expect_equal(sum(results), 1.0)
})

test_that("Hap to Geno frequency null call", {
  # Setup
  h1 <- 0
  h2 <- 0

  # run
  gf <- haplotypeToGenotypeFrequency(h1, h2,offset_fit)

  # validate
  expect_that(as.numeric(gf), is_more_than(0.0))
})

test_that("get prognosis null call", {
  # Setup
  h1 <- 0
  h2 <- 0
  race <- "CAU"

  # run
  results <- calculatePrognosis(h1, h2, race)

  # validate
  expect_false(is.null(results))
  expect_named(results, c('Good', 'Fair', 'Poor'))
  expect_equal(sum(results), 1.0)
})



