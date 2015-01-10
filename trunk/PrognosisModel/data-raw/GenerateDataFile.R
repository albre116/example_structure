source("data-raw/BuildTrainingData.R")
source("data-raw/EstablishOffset.R")
source("data-raw/PolrModelFit.R")


# > devtools::use_data_raw()
# Creating data-raw/
#   Next:
#   * Add data creation scripts in data-raw
#   * Use devtools::use_data() to add data to package

# Read the raw data and convert it into data.frame that will be saved
raw_data <- read.csv('data-raw/Raw_Data.csv')
offset_fit <- establishOffset(raw_data)
training_data <- buildTrainingData(raw_data)
MIN_THRESH_H1<-training_data[["min_thresh_h1"]]
MIN_THRESH_H2<-training_data[["min_thresh_h2"]]
training_data<-training_data[["TRAIN"]]
fit_polr<-buildModel(training_data)

# save as R/sysdata.rda The objects in R/sysdata.rda are only availalbe inside
# the package. training_data will be avaialble as a global inside the package.
devtools::use_data(training_data, offset_fit, fit_polr,MIN_THRESH_H1,MIN_THRESH_H2, internal = TRUE, overwrite = TRUE)

