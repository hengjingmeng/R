# test
machinelearningtest<-function(modelfile,custFileName,testdata,baseHome,datatype){
  setwd(baseHome)
  load(modelfile)
  library(dplyr)
  library(caret)
  library(gbm)
  library(pROC)
  source('10dengfen.R')
  source('KSvalue.R')
  source('plotks.R')
  source('plotroc.R')
  source('naratio.R')
  source('plotdengfen.R')
  ###########################################dataprocess########################################
  dat<-read.csv(testdata,header = T,fileEncoding="GB18030",stringsAsFactors=FALSE,quote = "") #***处输入训练数据文件名称  
  dattype<-read.csv(datatype,header = T)  #**处输入字段数据类型说明文件
  
  n<-ncol(dat)
  namelist1<-as.vector(dattype[which(dattype$type=="bigint" | dattype$type=="double"),1])
  namelist2<-as.vector(dattype[which(dattype$type=='string'),1])
  lianxu<-dat[,colnames(dat) %in% namelist1]
  n1<-ncol(lianxu)
  lisan<-dat[,colnames(dat) %in% namelist2]
  n2<-ncol(lisan)
  
  for (i in 1:n1){
    #lianxu[,i]<-round(lianxu[,i],1)
    lianxu[,i]<-as.character(lianxu[,i])
    lianxu[which(lianxu[,i]=="" | lianxu[,i]=="未知"),i]<--1
    lianxu[is.na(lianxu[,i]),i]<--1
    lianxu[,i]<-as.numeric(lianxu[,i])
    lianxu[is.na(lianxu[,i]),i]<--1
  }
  
  descrCor<-cor(lianxu)
  highcor<-findCorrelation(descrCor,cutoff = 0.8)
  lianxu<-lianxu[,-highcor]
  
  for (i in 1:n2){
    lisan[,i]<-as.character(lisan[,i])
    lisan[which(lisan[,i]=="" | lisan[,i]=="未知"),i]<--1
    lisan[is.na(lisan[,i]),i]<--1
    lisan[,i]<-as.factor(lisan[,i])
  }
  
  data2<-cbind(lisan,lianxu)
  data2[which(data2$y>0),'y']<-"b"
  data2[which(data2$y<=0),'y']<-"g"
  data2$y<-as.factor(data2$y)
  
  # data2<-cbind(lisan,lianxu)
  # data2$y<-0
  # data2[which(data2$overdue_days>overduedays),'y']<-"b"
  # data2[which(data2$overdue_days<=overduedays),'y']<-"g"
  # data2$y<-as.factor(data2$y)
  
  data2<-data2[1:2000,]
  data1<-select(data2,-certi_no)
  
  ############################################testdata#############################################
  
  pre<-predict(clasfit,data1,type = 'prob')
  pre<-as.data.frame(pre)
  pre<-cbind(pre,data1$y)
  names(pre)[3]<-"y"
  
  rocpre<-roc(pre$y,pre$b)
  Bestpoint<-coords(rocpre, "best", ret = "threshold")[[1]]
  pre$prey<-0
  pre[which(pre$b>Bestpoint),'prey']<-"b"
  pre[which(pre$b<=Bestpoint),'prey']<-"g"
  pre<-mutate(pre,fenshu=b*100)
  fenweishu<-data.frame(quantile(pre$fenshu,seq(0,1,0.1)))
  names(fenweishu)[1]<-"point"
  confumat<-confusionMatrix(data=pre$prey,reference=pre$y) 
  
  testgood<-nrow(data2[which(data2$y=='g'),])
  testbad<-nrow(data2[which(data2$y=='b'),])
  testbadlv<-paste(round(testbad/(testbad+testgood),3)*100,'%')
  
  ############################################out##################################################
  
  Acc<-confumat[[3]][1]
  Re<-confumat$byClass[6]
  Auc<-rocpre$auc
  Ks<-myks(pre$y,pre$g)
  fenshu10dengfen<-fenshu10deng(pre)
  renshu10dengfen<-renshu10deng(pre)
  na_ratio<-naratio(data1)
  plotks(pre$y,pre$g,custFileName)
  plotroc(rocpre,custFileName)
  plot10dengfenlv(fenshu10dengfen,baseHome,custFileName)
  plot10dengrenlv(renshu10dengfen,baseHome,custFileName)
  plot10dengfencumu(fenshu10dengfen,baseHome,custFileName)
  plot10dengrencumu(renshu10dengfen,baseHome,custFileName)
  
  
  return(list(testgood,testbad,testbadlv,na_ratio,fenweishu,Acc,Re,Auc,Ks,fenshu10dengfen,renshu10dengfen))
 
}
#测试执行：
machinelearningtest('zrtest.RData','ttt','/alidata1/zr/R/automodeltest/test_sample.csv','/alidata1/zr/R/automodeltest/','/alidata1/zr/R/automodeltest/leixingtest.csv')
