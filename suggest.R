

drop<-c('url','timedelta','weekday_is_monday','weekday_is_tuesday','weekday_is_wednesday','weekday_is_thursday','weekday_is_friday','weekday_is_saturday','weekday_is_sunday','is_weekend','data_channel_is_entertainment','data_channel_is_lifestyle','data_channel_is_bus','data_channel_is_tech','data_channel_is_world','data_channel_is_socmed','shares','LDA_00','LDA_01','LDA_02','LDA_03','LDA_04')
data_training_knn <- data_training[,!names(data_training) %in% drop ]
data_testing_knn <- data_testing[,!names(data_testing) %in% drop ]


scale_columns_knn<-c('')

temp_data_training_knn<-data_training_knn[,names(data_training_knn) %in% scale_columns_knn ]
scaled_temp_training_data_knn<-scale(data_training_knn[,!names(data_training_knn) %in% scale_columns_knn] ,center=TRUE,scale=TRUE)
scaled_training_data_knn<-cbind(temp_data_training_knn,scaled_temp_training_data_knn)

temp_data_testing_knn<-data_testing_knn[,names(data_testing_knn) %in% scale_columns_knn ]
scaled_temp_testing_data_knn<-scale(data_testing_knn[,!names(data_testing_knn) %in% scale_columns_knn] ,center=TRUE,scale=TRUE)
scaled_testing_data_knn<-cbind(temp_data_testing_knn,scaled_temp_testing_data_knn)



tempdist <- rep(10000, times = dim(Y_training_ann)[1])

for(i in c(1:dim(Y_testing_ann)[1]))
{
  if(Y_testing_ann[i,1] == -1)
  {x <- scaled_testing_data_knn[i,]
  ind <- i
  break
  }     
} 

for(i in c(1:dim(Y_training_ann)[1]))
{
  if(Y_training_ann[i,1] == -1)
    tempdist[i] <- sqrt(sum((x - scaled_training_data_knn[i,]) ^ 2))
  
}


tempdist <- as.vector(tempdist)
s<-sort(tempdist,index.return = TRUE)

final<-scaled_training_data_knn[s$ix[1],]
for(i in 2:5)
  final <- rbind(final,scaled_training_data_knn[s$ix[i],])

meanval <- colMeans(final, na.rm = TRUE)

changeval <- rep(0, times = dim(data_training_knn)[2] )



for(i in 1:dim(data_training_knn)[2])
{
  
  changeval[i] <- meanval[i]*sd(data_training_knn[,i]) + mean(data_training_knn[,i])  
  print(colnames(data_training_knn)[i])
  print(changeval[i])  
}



y <- data_testing_knn[ind,]

