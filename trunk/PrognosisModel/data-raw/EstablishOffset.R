
offsetData<- function(RAW_DATA) {
  ###Import Data
  RAW_DATA$Productivity.Group<-as.character(RAW_DATA$Productivity.Group)
  RAW_DATA$Productivity.Group[ RAW_DATA$Productivity.Group=="B"]="A"
  RAW_DATA$Productivity.Group[ RAW_DATA$Productivity.Group=="C"]="B"
  RAW_DATA$Productivity.Group[ RAW_DATA$Productivity.Group=="D"]="C"
  RAW_DATA$Race<-as.character(RAW_DATA$Race)
  #RAW_DATA$Race[ RAW_DATA$Race=="CAU"]="WH"

  RAW_DATA$Frequency.1<-RAW_DATA$Frequency.1/100
  RAW_DATA$Frequency.2<-RAW_DATA$Frequency.2/100


  min_thresh_gf<-min(RAW_DATA$Total.Genotype.Frequency[ RAW_DATA$Total.Genotype.Frequency!=0],na.rm=T)
  RAW_DATA$Total.Genotype.Frequency[ RAW_DATA$Total.Genotype.Frequency==0]<-min_thresh_gf
  RAW_DATA$Total.Genotype.Frequency[ is.na(RAW_DATA$Total.Genotype.Frequency)]=min_thresh_gf

  min_thresh_h1<-min(RAW_DATA$Frequency.1[ RAW_DATA$Frequency.1!=0],na.rm=T)
  RAW_DATA$Frequency.1[ RAW_DATA$Frequency.1==0]<-min_thresh_h1
  RAW_DATA$Frequency.1[ is.na(RAW_DATA$Frequency.1)]=min_thresh_h1

  min_thresh_h2<-min(RAW_DATA$Frequency.2[ RAW_DATA$Frequency.2!=0],na.rm=T)
  RAW_DATA$Frequency.2[ RAW_DATA$Frequency.2==0]<-min_thresh_h2
  RAW_DATA$Frequency.2[ is.na(RAW_DATA$Frequency.2)]=min_thresh_h2

  permute<-rbinom(nrow(RAW_DATA),1,prob=0.5)==1
  RAW_DATA[permute,]<-RAW_DATA[permute,c(1:6,8,7,10,9,11:13)]
  DATA<-RAW_DATA[c("RID","Race","Frequency.1",
                   "Frequency.2","Rank.1","Rank.2",
                   "Total.Genotype.Frequency","Productivity.Group")]

  DATA$Productivity.Group<-ordered(DATA$Productivity.Group,levels=c("C","B","A"))
  colnames(DATA)<-c("RID","Race","H1","H2","Rank_H1","Rank_H2","GF","Productivity")
  DATA<-DATA[complete.cases(DATA),]###remove NA's
  DATA<-DATA[DATA$GF!=0,]
  use_in_fit<-DATA$GF>min_thresh_gf & DATA$H1>min_thresh_h1 & DATA$H2>min_thresh_h2
  rm(RAW_DATA)

  ###Format Predictors to be ln-scale
  DATA$GF<-log(DATA$GF)
  DATA$H1<-log(DATA$H1)
  DATA$H2<-log(DATA$H2)
  #DATA[c("H1","H2","GF")]<-normalizeData(DATA[c("H1","H2","GF")],type="0_1")
  DATA$Race<-as.factor(DATA$Race)

  return(DATA[use_in_fit,])


}



establishOffset<-function(RAW_DATA){
  DATA<-offsetData(RAW_DATA)
  plot(H1~H2,data=DATA)###check data support
  plot(GF~I(H1+H2),data=DATA,main="Plot of offset between genotype and haplotype frequencies")###check that GF=H1*H2
  float<-lm(GF~I(H1+H2),data=DATA)
  abline(float)
  return(float)


}








