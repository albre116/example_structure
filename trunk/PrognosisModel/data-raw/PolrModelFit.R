
buildModel <- function(training_data) {

  ###Try a basic Proportional Odds Model on the genotype Frequencies
  ###first fit weights to the different classes
  ww<-table(training_data$Productivity,training_data$Race)
  ww<-as.data.frame(ww)
  for(race in unique(ww$Var2)){
    idx<-ww$Var2==race
    ww$Freq[idx]<-ww$Freq[idx]/sum(ww$Freq[idx])
    ww$Freq[idx]<-1/ww$Freq[idx]
  }

  ###modify the weights
  ###this modifies the loss funciton for mis-classification costs
  #ww$Freq[ ww$Var1=="A"]=ww$Freq[ ww$Var1=="A"]*1
  #ww$Freq[ ww$Var1=="B"]=ww$Freq[ ww$Var1=="B"]*1
  #ww$Freq[ ww$Var1=="C"]=ww$Freq[ ww$Var1=="C"]*1

  training_data$weights_fit<-0
  for(i in 1:nrow(ww)){
    idx<-training_data$Race==ww$Var2[i] & as.character(training_data$Productivity)==ww$Var1[i]
    training_data$weights_fit[idx]<-ww$Freq[i]
  }

  fit<-MASS::polr(Productivity~GF*Race,weights=weights_fit,data=training_data)

  return(fit)

}
