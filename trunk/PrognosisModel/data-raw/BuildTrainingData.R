
buildTrainingData <- function(RAW_DATA) {

  RAW_DATA$Productivity.Group<-as.character(RAW_DATA$Productivity.Group)
  RAW_DATA$Productivity.Group[ RAW_DATA$Productivity.Group=="B"]="A"
  RAW_DATA$Productivity.Group[ RAW_DATA$Productivity.Group=="C"]="B"
  RAW_DATA$Productivity.Group[ RAW_DATA$Productivity.Group=="D"]="C"
  RAW_DATA$Race<-as.character(RAW_DATA$Race)

  RAW_DATA$Frequency.1<-RAW_DATA$Frequency.1/100
  RAW_DATA$Frequency.2<-RAW_DATA$Frequency.2/100


  min_thresh<-min(RAW_DATA$Total.Genotype.Frequency[ RAW_DATA$Total.Genotype.Frequency!=0],na.rm=T)
  RAW_DATA$Total.Genotype.Frequency[ RAW_DATA$Total.Genotype.Frequency==0]<-min_thresh
  RAW_DATA$Total.Genotype.Frequency[ is.na(RAW_DATA$Total.Genotype.Frequency)]=min_thresh

  min_thresh<-min(RAW_DATA$Frequency.1[ RAW_DATA$Frequency.1!=0],na.rm=T)
  min_thresh_h1<-min_thresh
  RAW_DATA$Frequency.1[ RAW_DATA$Frequency.1==0]<-min_thresh
  RAW_DATA$Frequency.1[ is.na(RAW_DATA$Frequency.1)]=min_thresh

  min_thresh<-min(RAW_DATA$Frequency.2[ RAW_DATA$Frequency.2!=0],na.rm=T)
  min_thresh_h2<-min_thresh
  RAW_DATA$Frequency.2[ RAW_DATA$Frequency.2==0]<-min_thresh
  RAW_DATA$Frequency.2[ is.na(RAW_DATA$Frequency.2)]=min_thresh


  permute<-rbinom(nrow(RAW_DATA),1,prob=0.5)==1
  RAW_DATA[permute,]<-RAW_DATA[permute,c(1:6,8,7,10,9,11:13)]
  DATA<-RAW_DATA[c("RID","Race","Frequency.1",
                   "Frequency.2","Rank.1","Rank.2",
                   "Total.Genotype.Frequency","Productivity.Group")]

  DATA$Productivity.Group<-ordered(DATA$Productivity.Group,levels=c("C","B","A"))
  colnames(DATA)<-c("RID","Race","H1","H2","Rank_H1","Rank_H2","GF","Productivity")
  DATA<-DATA[complete.cases(DATA),]###remove NA's
  DATA<-DATA[DATA$GF!=0,]
  rm(RAW_DATA)

  ###Format Predictors to be ln-scale
  DATA$GF<-log(DATA$GF)
  DATA$H1<-log(DATA$H1)
  DATA$H2<-log(DATA$H2)
  #DATA[c("H1","H2","GF")]<-normalizeData(DATA[c("H1","H2","GF")],type="0_1")
  summary(DATA)

  ####Split Train and Test
  set.seed(1103)
  train_idx<-sample(1:nrow(DATA),floor(nrow(DATA)*0.5))
  logical<-rep(FALSE,nrow(DATA))
  logical[train_idx]<-TRUE
  train_idx<-logical
  test_idx<-!logical
  rm(logical)
  TRAIN<-DATA[train_idx,]
  TEST<-DATA[test_idx,]

  return(list(TRAIN=TRAIN,min_thresh_h1=min_thresh_h1,min_thresh_h2=min_thresh_h2))
}


library(testthat)
test_that("build training data", {

  #setup
  kims_training_data <- read.csv('data-raw/Raw_Data.csv')

  #run
  training_data <- buildTrainingData(kims_training_data)
  training_data<-training_data[["TRAIN"]]

  #Verify
  expect_false(is.null(training_data))
  # We have all the expected fields
  expect_that(names(training_data), equals(c("RID", "Race", "H1", "H2", "Rank_H1", "Rank_H2", "GF", "Productivity")))
  # we have only A B C
  expect_that(levels(training_data$Productivity), equals(c('C', 'B', 'A')))

})


