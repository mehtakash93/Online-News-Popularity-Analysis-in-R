Y_training_svm[Y_training_svm < 1400] <- -1
Y_training_svm[Y_training_svm >= 1400] <- 1

Y_testing_svm[Y_testing_svm < 1400] <- -1
Y_testing_svm[Y_testing_svm >= 1400] <- 1

drop<-c('url','timedelta','is_weekend','shares')
data_training_svm <- data_training[,!names(data_training) %in% drop ]
data_testing_svm <- data_testing[,!names(data_testing) %in% drop ]

scale_columns_svm<-c('n_tokens_title','n_tokens_content','num_hrefs','num_self_hrefs','num_imgs','num_videos','average_token_length','num_keywords','self_reference_min_shares','self_reference_max_shares','self_reference_avg_sharess','kw_min_min','kw_max_min','kw_avg_min','kw_min_max','kw_max_max','kw_avg_max','kw_min_avg','kw_max_avg','kw_avg_avg')

temp_data_training_svm<-data_training_svm[,!names(data_training_svm) %in% scale_columns_svm ]
scaled_temp_training_data_svm<-scale(data_training_svm[,names(data_training_svm) %in% scale_columns_svm] ,center=TRUE,scale=TRUE)
scaled_training_data_svm<-cbind(temp_data_training_svm,scaled_temp_training_data_svm,Y_training_svm)

temp_data_testing_svm<-data_testing_svm[,!names(data_testing_svm) %in% scale_columns_svm ]
scaled_temp_testing_data_svm<-scale(data_testing_svm[,names(data_testing_svm) %in% scale_columns_svm] ,center=TRUE,scale=TRUE)
scaled_testing_data_svm<-cbind(temp_data_testing_svm,scaled_temp_testing_data_svm,Y_testing_svm)

library(e1071)
Y_training_svm<-data.frame(Y_training_svm)
Y_testing_svm<-data.frame(Y_testing_svm)
train<-scaled_training_data_svm
test<-scaled_testing_data_svm



colnames(train)[58]<-"output"
colnames(test)[58]<-"output"




#model <- svm(scaled_training_data[,-58], Y_training,scale= FALSE,na.action = na.omit, type="C-classification")

#pred<-predict(model, scaled_testing_data,type = "raw")

#radial function
model_svm<-svm(output~.,data=train,scale=FALSE,type="C-classification")

#linear function
#model_svm<-svm(output~.,data=train,kernel="linear",scale=FALSE,type="C-classification")

#polynomial function
#model<-svm(output~.,data=train,kernel="polynomial",degree=1,coef0= 2,scale=FALSE,type="C-classification")

#model_svm<-svm(output~.,data=train,kernel="sigmoid",coef0=1,scale=FALSE,type="C-classification")
pred_svm<-predict(model_svm,newdata=test)

svm_ka_p<-as.vector(pred_svm)
svm_ka_p<-data.frame(svm_ka_p)


p_svm<-prediction(test$output,pred_svm)




precision_svm<-p_svm@tp[[1]][2]/(p_svm@tp[[1]][2]+p_svm@fp[[1]][2])
accuracy_svm<-(p_svm@tp[[1]][2]+p_svm@tn[[1]][2])/(p_svm@tp[[1]][2]+p_svm@tn[[1]][2]+p_svm@fn[[1]][2]+p_svm@fp[[1]][2])
recall_svm<-p_svm@tp[[1]][2]/(p_svm@tp[[1]][2]+p_svm@fn[[1]][2])
fmeasure_svm<-2*recall_svm*precision_svm/(recall_svm+precision_svm)
tabulated_results_svm<-data.frame(accuracy_svm,precision_svm,recall_svm,fmeasure_svm)
