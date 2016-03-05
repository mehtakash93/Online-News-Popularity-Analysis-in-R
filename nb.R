drop<-c('shares','timedelta','url',"n_non_stop_words","title_subjectivity","title_sentiment_polarity")
data_training_nb <- data_training[,!names(data_training) %in% drop ]
data_testing_nb <- data_testing[,!names(data_testing) %in% drop ]
data_training_nb$min_negative_polarity[data_training_nb$min_negative_polarity<0]=data_training_nb$min_negative_polarity[data_training_nb$min_negative_polarity<0]*-1
data_training_nb$max_negative_polarity[data_training_nb$max_negative_polarity<0]=data_training_nb$max_negative_polarity[data_training_nb$max_negative_polarity<0]*-1
data_training_nb$avg_negative_polarity[data_training_nb$avg_negative_polarity<0]=data_training_nb$avg_negative_polarity[data_training_nb$avg_negative_polarity<0]*-1

data_testing_nb$min_negative_polarity[data_testing_nb$min_negative_polarity<0]=data_testing_nb$min_negative_polarity[data_testing_nb$min_negative_polarity<0]*-1
data_testing_nb$max_negative_polarity[data_testing_nb$max_negative_polarity<0]=data_testing_nb$max_negative_polarity[data_testing_nb$max_negative_polarity<0]*-1
data_testing_nb$avg_negative_polarity[data_testing_nb$avg_negative_polarity<0]=data_testing_nb$avg_negative_polarity[data_testing_nb$avg_negative_polarity<0]*-1

Y_training_nb[Y_training_nb<1400]=-1
Y_training_nb[Y_training_nb>=1400]=1




Y_testing_nb[Y_testing_nb<1400]=-1
Y_testing_nb[Y_testing_nb>=1400]=1
#Naive Bayes

#data_training[,c("title_sentiment_polarity")][data_training[,c("title_sentiment_polarity")]<=0]=0
#data_training[,c("title_sentiment_polarity")][data_training[,c("title_sentiment_polarity")]>0]=1

highlow <- function(arg1,med){
  arg1[arg1<=med]=0
  arg1[arg1>med]=1
  
  return(arg1)
}



for(i in c("n_tokens_title","n_tokens_content","n_unique_tokens","n_non_stop_unique_tokens","num_hrefs","num_self_hrefs","num_imgs","num_videos","average_token_length","num_keywords","self_reference_min_shares","self_reference_max_shares","self_reference_avg_sharess"))
{
  med <- summary(data_training_nb[,c(i)])[3]
  # print(i)
  #print(med)
  # print(data_testing[1:25,c(i)])
  data_training_nb[,c(i)]<-highlow(data_training_nb[,c(i)],med)
  
  
  data_testing_nb[,c(i)]<-highlow(data_testing_nb[,c(i)],med)
}
for(i in c("LDA_00","LDA_01","LDA_02","LDA_03","LDA_04","global_subjectivity","global_sentiment_polarity","global_rate_positive_words","global_rate_negative_words","rate_positive_words","rate_negative_words","avg_positive_polarity","min_positive_polarity","max_positive_polarity","avg_negative_polarity","min_negative_polarity","max_negative_polarity","abs_title_sentiment_polarity","kw_min_min","kw_min_max","kw_min_avg","kw_max_min","kw_avg_min","kw_avg_avg","kw_avg_max","kw_max_max","kw_max_avg","abs_title_subjectivity"))
{
  med <- summary(data_training_nb[,c(i)])[4]
  # print(i)
  #print(med)
  # print(data_testing[1:25,c(i)])
  data_training_nb[,c(i)]<-highlow(data_training_nb[,c(i)],med)
  
  
  data_testing_nb[,c(i)]<-highlow(data_testing_nb[,c(i)],med)
}

pc0<-length(which(Y_training_nb==-1))/length(Y_training_nb)
pc1<-length(which(Y_training_nb==1))/length(Y_training_nb)


prediction_nb <- function(arg1,py0,py1)
{
  
  answer<-1:dim(data_testing_nb)[1]
  answer1<-1:dim(data_testing_nb)[1]
  pri<-1:dim(data_testing_nb)[1]
  for(l in 1:dim(data_testing_nb)[1])
  {
    answer[l]<-1
    answer1[l]<-1
    #print(p)
    for(u in 1:55)
    { abc=arg1[l,u]
    m=data_training_nb[which(Y_training_nb==-1),c(u)]
    m1=data_training_nb[which(Y_training_nb==1),c(u)]
    pam=length(which(m==abc))/length(m)
    pam1=length(which(m1==abc))/length(m1)
    answer[l]=(answer[l])*pam
    answer1[l]=(answer1[l])*pam1
    #print(answer[l])
    #print(answer1[l])
    }
    
    
    print(l)
    
    
    if((answer1[l]*py1)>(answer[l]*py0))
    { 
      pri[l]=1
    }
    if((answer[l]*py0)>=(answer1[l]*py1))
    {
      pri[l]=-1
    }
    
  }
  return(pri)
}

ansfinal<-prediction_nb(data_testing_nb,pc0,pc1)


#length(which(ansfinal==Y_testing_nb))/length(Y_testing_nb)
#actualright=Y_testing_nb[ansfinal==1];
#a=length(which(actualright==1));
#c=length(which(actualright==-1));
#actualwrong=Y_testing_nb[ansfinal==-1];
#b=length(which(actualright==1));
#d=length(which(actualright==-1));
#precision_nb=a/(a+c)
#recall_nb=a/(a+b)
#F_nb=(2*precision_nb*recall_nb)/(recall_nb+precision_nb)
#accuracy_nb=(a+d)/(a+b+c+d)


p_nb<-prediction(Y_testing_nb,ansfinal)
precision_nb<-p_nb@tp[[1]][2]/(p_nb@tp[[1]][2]+p_nb@fp[[1]][2])
accuracy_nb<-(p_nb@tp[[1]][2]+p_nb@tn[[1]][2])/(p_nb@tp[[1]][2]+p_nb@tn[[1]][2]+p_nb@fn[[1]][2]+p_nb@fp[[1]][2])
recall_nb<-p_nb@tp[[1]][2]/(p_nb@tp[[1]][2]+p_nb@fn[[1]][2])
fmeasure_nb<-2*recall_nb*precision_nb/(recall_nb+precision_nb)
tabulated_results_nb<-data.frame(accuracy_nb,precision_nb,recall_nb,fmeasure_nb)

