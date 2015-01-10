test_that("Check Bayes Classifier Result",{
  mug<-list(structure(list(locus = "A", type1 = "33:03", type2 = "68:01"), .Names = c("locus",
                                                                                      "type1", "type2")), structure(list(locus = "B", type1 = "14:01",
                                                                                                                         type2 = "55:01"), .Names = c("locus", "type1", "type2")),
            structure(list(locus = "C", type1 = "03:03", type2 = "07:05"), .Names = c("locus",
                                                                                      "type1", "type2")), structure(list(locus = "DQB1", type1 = "06:03",
                                                                                                                         type2 = "06:09"), .Names = c("locus", "type1", "type2"
                                                                                                                         )), structure(list(locus = "DRB1", type1 = "13:01", type2 = ""), .Names = c("locus",
                                                                                                                                                                                                     "type1", "type2")))
  race<-c("CAU","AFA","HIS","API")
  prior<-c(75,25,100,20)
  host<-"http://p1haplostats-s1:28080"
  out<-calculateBayesProbabilities(mug,race,prior,host)
  expect_equal(sum(out$prior),1)
  expect_equal(sum(out$posterior),1)
})
