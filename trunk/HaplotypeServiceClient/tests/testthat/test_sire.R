SERVER_URL <- 'http://p1haplostats-s1:28080'



test_that("SIRE by RID",{

  rid <-1419426
  host <- SERVER_URL

  sire <- getSireForRID(rid, host)

  expect_false(is.null(sire))


})

test_that('SIRE with Invalid RID', {
  rid <-8888899999
  host <- SERVER_URL

  sire <- getSireForRID(rid, host)

  expect_null(sire)
})

test_that('SIRE with Invalid host', {
  rid <-1419426
  host <- 'http://testurl.nmdp.org:9988'

  expect_message(sire <- getSireForRID(rid, host), "Failed")
  expect_null(sire)

})
