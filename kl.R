
Y_kl[Y_kl<1400]=-1
Y_kl[Y_kl>=1400]=1


drop<-c('shares','timedelta','url',"n_non_stop_words","title_subjectivity","title_sentiment_polarity")
data_kl <- data_kl[,!names(data_kl) %in% drop ]

data_kl$min_negative_polarity[data_kl$min_negative_polarity<0]=data_kl$min_negative_polarity[data_kl$min_negative_polarity<0]*-1
data_kl$max_negative_polarity[data_kl$max_negative_polarity<0]=data_kl$max_negative_polarity[data_kl$max_negative_polarity<0]*-1
data_kl$avg_negative_polarity[data_kl$avg_negative_polarity<0]=data_kl$avg_negative_polarity[data_kl$avg_negative_polarity<0]*-1







#Naive Bayes

#data_kl[,c("title_sentiment_polarity")][data_kl[,c("title_sentiment_polarity")]<=0]=0
#data_kl[,c("title_sentiment_polarity")][data_kl[,c("title_sentiment_polarity")]>0]=1

highlow <- function(arg1,med){
  arg1[arg1<=med]=0
  arg1[arg1>med]=1
  
  return(arg1)
}



for(i in c("n_tokens_title","n_tokens_content","n_unique_tokens","n_non_stop_unique_tokens","num_hrefs","num_self_hrefs","num_imgs","num_videos","average_token_length","num_keywords","self_reference_min_shares","self_reference_max_shares","self_reference_avg_sharess"))
{
  med <- summary(data_kl[,c(i)])[3]
  # print(i)
  #print(med)
  # print(data_testing[1:25,c(i)])
  data_kl[,c(i)]<-highlow(data_kl[,c(i)],med)
  
  
}
for(i in c("LDA_00","LDA_01","LDA_02","LDA_03","LDA_04","global_subjectivity","global_sentiment_polarity","global_rate_positive_words","global_rate_negative_words","rate_positive_words","rate_negative_words","avg_positive_polarity","min_positive_polarity","max_positive_polarity","avg_negative_polarity","min_negative_polarity","max_negative_polarity","abs_title_sentiment_polarity","kw_min_min","kw_min_max","kw_min_avg","kw_max_min","kw_avg_min","kw_avg_avg","kw_avg_max","kw_max_max","kw_max_avg","abs_title_subjectivity"))
{
  med <- summary(data_kl[,c(i)])[4]
  # print(i)
  #print(med)
  # print(data_testing[1:25,c(i)])
  data_kl[,c(i)]<-highlow(data_kl[,c(i)],med)
  
  

}




kl<-matrix(nrow = 2, ncol = length(data_kl))

pc0<-length(which(Y_kl==-1))/length(Y_kl)
pc1<-length(which(Y_kl==1))/length(Y_kl)
a<-colSums(data_kl[which(Y_kl==-1),])
b<-colSums(data_kl[which(Y_kl==1),])
c<-colSums(data_kl)/dim(data_kl)[1]
d<-length(which(Y_kl==-1))-colSums(data_kl[which(Y_kl==-1),])
e<-length(which(Y_kl==1))-colSums(data_kl[which(Y_kl==1),])
f<-(dim(data_kl)[1]-colSums(data_kl))/dim(data_kl)[1]
pc01=(pc0*(a/length(which(Y_kl==-1))))/c
pc11=(pc1*(b/length(which(Y_kl==1))))/c
pc00=(pc0*(d/length(which(Y_kl==-1))))/f
pc10=(pc1*(e/length(which(Y_kl==1))))/f
kl[2,]<-pc01*log(pc01/pc0,base = 2)+pc11*log(pc11/pc1,base = 2)
kl[1,]<-pc00*log(pc00/pc0,base = 2)+pc10*log(pc10/pc1,base = 2)

wavg<-((colSums(data_kl)/dim(data_kl)[1])*kl[2,]) +((dim(data_kl)[1]-colSums(data_kl)/dim(data_kl)[1])*kl[1,])


wi1=(c*log(c,base = 2))+(f*log(f,base = 2))

wi=wavg/wi1
z=(1/length(data_kl))*sum(wi)
wifinal=wi/z