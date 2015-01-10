test_that("Getting prognosis result",{

  #setup
  race <- "CAU"
  log_geno_freq <- -24.03097

  # Training data is available as part of the package.
  #fit <- buildModel(training_data)

  #run
  results <- proportionalOddsPrognosis(race,log_geno_freq,fit_polr)

  #   A           C           D
  #   0.001406375 0.060211735 0.938381890

  #validate
  expect_false(is.null(results))
  expect_that(length(results), equals(3))
  expect_true((results['C'] > results['A']) &
                (results['B'] > results['A']))

  expect_that(sum(results), equals(1.0))
})





test_that("build a model fit",{

  #setup
  race <- "CAU"

  #run
  # Training data is available as part of the package.
  fit <- fit_polr

  #test
  expect_false(is.null(fit))
  # class  polr
  expect_that(fit, is_a('polr'))

})
