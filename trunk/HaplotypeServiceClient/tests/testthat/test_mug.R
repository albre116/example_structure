SERVER_URL <- 'http://p1haplostats-s1:28080'

# Not a very good test, because the RID is from production.
#
test_that("Mug by RID",{

  rid <-1419426
  host <- SERVER_URL

  mug <- getMugForRID(rid, host)

  expect_false(is.null(mug))
  expect_true(is.list(mug))

  loci <- sapply(mug, function(l) l$locus)
  expect_equal(sort(loci), c("A", "B", "C", "DQB1", "DRB1", "DRB4"))

})

test_that('Mug with Invalid RID', {
  rid <-8888899999
  host <- SERVER_URL

  mug <- getMugForRID(rid, host)

  expect_null(mug)

})

test_that('Mug with Invalid host', {
  rid <-1419426
  host <- 'http://testurl.nmdp.org:9988'

  expect_message(mug <- getMugForRID(rid, host), "Failed")
  expect_null(mug)

})


