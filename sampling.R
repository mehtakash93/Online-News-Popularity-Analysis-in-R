library(e1071)
library(nnet)
library(ROCR)


data<-read.csv("OnlineNewsPopularity.csv",header=T)
data_kl <- data


newData<-data[data$n_tokens_content !=0,]
channel_ent<-newData[newData$data_channel_is_entertainment==1,]
channel_life<-newData[newData$data_channel_is_lifestyle==1,]
channel_bus<-newData[newData$data_channel_is_bus==1,]
channel_tech<-newData[newData$data_channel_is_tech==1,]
channel_world<-newData[newData$data_channel_is_world==1,]
channel_soc<-newData[newData$data_channel_is_socmed==1,]

data_training<-channel_soc[sample(nrow(channel_soc),as.integer((70*dim(channel_soc)[1])/100),replace=FALSE ),]
data_testing<-channel_soc[!(channel_soc$url) %in% data_training$url,]

temp_train<-channel_ent[sample(nrow(channel_ent),as.integer((70*dim(channel_ent)[1])/100),replace=FALSE ),]
data_training<-rbind(data_training,temp_train)

temp_test<-channel_ent[!(channel_ent$url) %in% temp_train$url,]
data_testing<-rbind(data_testing,temp_test)

temp_train<-channel_world[sample(nrow(channel_world),as.integer((70*dim(channel_world)[1])/100),replace=FALSE ),]
data_training<-rbind(data_training,temp_train)

temp_test<-channel_world[!(channel_world$url) %in% temp_train$url,]
data_testing<-rbind(data_testing,temp_test)

temp_train<-channel_tech[sample(nrow(channel_tech),as.integer((70*dim(channel_tech)[1])/100),replace=FALSE ),]
data_training<-rbind(data_training,temp_train)

temp_test<-channel_tech[!(channel_tech$url) %in% temp_train$url,]
data_testing<-rbind(data_testing,temp_test)

temp_train<-channel_bus[sample(nrow(channel_bus),as.integer((70*dim(channel_bus)[1])/100),replace=FALSE ),]
data_training<-rbind(data_training,temp_train)

temp_test<-channel_bus[!(channel_bus$url) %in% temp_train$url,]
data_testing<-rbind(data_testing,temp_test)

temp_train<-channel_life[sample(nrow(channel_life),as.integer((70*dim(channel_life)[1])/100),replace=FALSE ),]
data_training<-rbind(data_training,temp_train)

temp_train<-newData[!(newData$url) %in% data_training$url & !(newData$url) %in% data_testing$url,]
data_testing<-rbind(data_testing,temp_test)


Y_training_svm<-data_training$shares


Y_testing_svm<-data_testing$shares

Y_training_nb<-data_training$shares


Y_testing_nb<-data_testing$shares



Y_training_ann<-data.frame(data_training$shares)


Y_testing_ann<-data.frame(data_testing$shares)

Y_kl <- data$shares

