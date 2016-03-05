
Y_training_ann<-scale(Y_training_ann ,center=TRUE,scale=TRUE)
Y_testing_ann<-scale(Y_testing_ann ,center=TRUE,scale=TRUE)
med <- median(Y_training_ann)

Y_training_ann[Y_training_ann<med]<--1
Y_training_ann[Y_training_ann>=med]<-1
Y_training_ann <- data.frame(Y_training_ann)


Y_testing_ann[Y_testing_ann<med]<--1
Y_testing_ann[Y_testing_ann>=med]<-1
Y_testing_ann <- data.frame(Y_testing_ann)



#drop<-c('shares','timedelta','url',"n_non_stop_words","title_subjectivity","title_sentiment_polarity")
drop<-c('url','timedelta','is_weekend','shares')
data_training_ann <- data_training[,!names(data_training) %in% drop ]
data_testing_ann <- data_testing[,!names(data_testing) %in% drop ]

scale_columns_ann<-c('data_channel_is_lifestyle','data_channel_is_entertainment','data_channel_is_socmed','data_channel_is_bus','data_channel_is_tech','data_channel_is_world','weekday_is_monday','weekday_is_tuesday','weekday_is_wednesday','weekday_is_thursday','weekday_is_friday','weekday_is_saturday','weekday_is_sunday')

temp_data_training_ann<-data_training_ann[,names(data_training_ann) %in% scale_columns_ann ]
scaled_temp_training_data_ann<-scale(data_training_ann[,!names(data_training_ann) %in% scale_columns_ann] ,center=TRUE,scale=TRUE)
scaled_training_data_ann<-cbind(temp_data_training_ann,scaled_temp_training_data_ann)

temp_data_testing_ann<-data_testing_ann[,names(data_testing_ann) %in% scale_columns_ann ]
scaled_temp_testing_data_ann<-scale(data_testing_ann[,!names(data_testing_ann) %in% scale_columns_ann] ,center=TRUE,scale=TRUE)
scaled_testing_data_ann<-cbind(temp_data_testing_ann,scaled_temp_testing_data_ann)


model_ann <- nnet(scaled_training_data_ann, Y_training_ann, size = 9,linout = TRUE,
              decay = 0.1, maxit = 100)


pred_ann<-predict(model_ann, scaled_testing_data_ann)
p_ann<-as.vector(pred_ann)
p_ann<-data.frame(p_ann)

p_ann[p_ann<=0]<--1
p_ann[p_ann>0]<-1
p_ann <- data.frame(p_ann)




p_ann<-prediction(p_ann,Y_testing_ann)

precision_ann<-p_ann@tp[[1]][2]/(p_ann@tp[[1]][2]+p_ann@fp[[1]][2])
accuracy_ann<-(p_ann@tp[[1]][2]+p_ann@tn[[1]][2])/(p_ann@tp[[1]][2]+p_ann@tn[[1]][2]+p_ann@fn[[1]][2]+p_ann@fp[[1]][2])
recall_ann<-p_ann@tp[[1]][2]/(p_ann@tp[[1]][2]+p_ann@fn[[1]][2])
fmeasure_ann<-2*recall_ann*precision_ann/(recall_ann+precision_ann)
tabulated_results_ann<-data.frame(accuracy_ann,precision_ann,recall_ann,fmeasure_ann)



