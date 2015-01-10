SERVER_URL <- 'http://p1haplostats-s1:28080'

# Prepare the request to the webservice.
# 1. Population
population <- 'AFA'
# 2. HLA data
s1 <- list(locus="A", type1="31:01", type2="66:01")
s2 <- list(locus="BPR", type1="40:02", type2="41:02")
s3 <- list(locus="C", type1="03:04", type2="17:03")
s4 <- list(locus="DRB1", type1="07:01", type2="04:04")
s5 <- list(locus="DQB1", type1="03:02", type2="02:01")
mug <- list(s1, s2,s3,s4,s5)


test_that("Impute by mug and sire",{

  host <- SERVER_URL

  imputationList <- imputeForMug(population, mug, host)

  expect_false(is.null(imputationList))
})

test_that("Population is empty", {
  host <- SERVER_URL

  imputationList <- imputeForMug(population = "", mug, host)
  expect_null(imputationList)
})

test_that("Population is null", {
  host <- SERVER_URL

  imputationList <- imputeForMug(population = NULL, mug, host)
  expect_null(imputationList)
})

test_that("Population is UNK", {
  host <- SERVER_URL

  imputationList <- imputeForMug(population = "UNK", mug, host)

  expect_false(is.null(imputationList))
})

test_that("Multiple Population", {
  host <- SERVER_URL
  population <- c('AFA', 'CAU')
  imputationList <- imputeForMug(population = population, mug, host)
  expect_null(imputationList)

})

test_that("Mug is NULL", {
  host <- SERVER_URL

  imputationList <- imputeForMug('CAU', mug = NULL, host)
  expect_null(imputationList)

})

test_that("Imputation of mug from getMugForRID", {
  rid <-1419426
  host <- SERVER_URL

  mug <- getMugForRID(rid, host)

  imputationList <- imputeForMug('CAU', mug, host)

  expect_true(is.list(imputationList))
})


test_that("Impute pair by mug and sire not null",{

  host <- SERVER_URL

  imputationList <- imputePairForMug(population, mug, host)

  expect_false(is.null(imputationList))
})



test_that("Impute pair by mug and sire returns pairs",{

  host <- SERVER_URL

  imputationList <- imputePairForMug(population, mug, host)

  expect_false(is.null(imputationList))
  expect_more_than(nrow(imputationList), 0)
  expect_named(imputationList, c('Haplotype1',"Race1","Frequency1","Haplotype2","Race2","Frequency2","Pair Frequency"))

})




