RNGversion(vstr = '3.6.1')
rm(list=ls())

###### Data Cleaning ######
#insert dataset
data=read.csv("/Users/zhangsiyang/Desktop/USVideos.csv")
dim(data)

#delete useless column: <thumbnail_link>
data=data[,-c(12)]
dim(data)

#check whether there's missing data in each column: No
summary(is.na(data))
#data=na.omit(data)

#check whether there's ambiguous data in key numerical and logical columns: No
summary(data[,8:11])
summary(data[,12:14])

#unite the date type as 'yy-mm-dd' for <trending_date>
library(stringr)
data$trending_date=str_c(substr(data$trending_date,1,2),
                         substr(data$trending_date,7,8),
                         substr(data$trending_date,4,5),sep='-')
str(data$trending_date)

#unite the date type as 'yy-mm-dd' for <publish_time>
data$publish_time=substr(data$publish_time,3,10)
str(data$publish_time)

#create a new column <number_of_tags> to record the number of tags
data$number_of_tags <- str_count(data$tags,'|')+1
str(data$number_of_tags)

#create a new column <length_of_description> to record the length of descriptive words
data$length_of_description <- str_count(data$description)
str(data$length_of_description)

write.csv(data, '/Users/zhangsiyang/Desktop/Cleaned_USVideos.csv',row.names = F)

###### Project Deliverable 2 ###########
###### Data Analytical Techniques ######

########Text Mining - Sentiment Analysis: <title>########

#correlation between <views>/<likes>/<comment_count> and number of words in <title>
install.packages("stringr")
library(stringr)
cor(str_count(string = data$title,pattern = '\\S+'),data$views)
cor(str_count(string = data$title,pattern = '\\S+'),data$likes)
cor(str_count(string = data$title,pattern = '\\S+'),data$comment_count)
#conclusion: number of words in title is negatively correlated with views/likes/comment_count
#longer title, less views/likes/comment

#correlation between <views>/<likes>/<comment_count> and proportion of uppercase letters in <title>
data$proportionUpper0 = str_count(data$title,pattern='[A-Z]')/nchar(as.character(data$title))
data[is.na(proportionUpper0),'proportionUpper0']<-0
data$proportionUpper0
cor(data$proportionUpper0,data$views)
cor(data$proportionUpper0,data$likes)
cor(data$proportionUpper0,data$comment_count)
#conclusion: -0.01333196/0.02469032/0.04629761, negatively correlated with views, positively correlated with likes/comment_count
#more uppercase letters in title, less views

#top common words in <title> (stop words excluded)
data%>%
  unnest_tokens(input = as.character(title), output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()
#too complex, not in character vector form; need corpus

########Text Mining - Sentiment Analysis: <tag>########

#correlation between <views>/<likes>/<comment_count> and number of words in <tags>
cor(str_count(string = data$tags,pattern = '\\S+'),data$views)
cor(str_count(string = data$tags,pattern = '\\S+'),data$likes)
cor(str_count(string = data$tags,pattern = '\\S+'),data$comment_count)
#conclusion: number of words in tags is only positively correlated with views, 0.0244
#more tags, more views

#top common words in <tags> (stop words excluded)
install.packages("tidytext")
install.packages("dplyr")
install.packages("ggplot2")
library(tidytext)
library(dplyr)
library(ggplot2)
data%>%
  unnest_tokens(input = as.character(tags), output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()
#too complex, not in character vector form; need corpus

########Text Mining - Sentiment Analysis: <description>########

#correlation between <views>/<likes>/<comment_count> and number of words in <description>
cor(str_count(string = data$description,pattern = '\\S+'),data$views)
cor(str_count(string = data$description,pattern = '\\S+'),data$likes)
cor(str_count(string = data$description,pattern = '\\S+'),data$comment_count)
#conclusion: -0.01701241/-0.02914061/-0.02044243, not strongly/positively correlated

#correlation between <views>/<likes>/<comment_count> and number of sentences in <description>
cor(str_count(string = data$description,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),data$views)
cor(str_count(string = data$description,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),data$likes)
cor(str_count(string = data$description,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),data$comment_count)
#conclusion: -0.01028317/0.002376531/0.002795731, not strongly correlated

#correlation between <views>/<likes>/<comment_count> and proportion of uppercase letters in <description>
data$proportionUpper = str_count(data$description,pattern='[A-Z]')/nchar(as.character(data$description))
data[is.na(proportionUpper),'proportionUpper']<-0
data$proportionUpper
cor(data$proportionUpper,data$views)
cor(data$proportionUpper,data$likes)
cor(data$proportionUpper,data$comment_count)
#conclusion: 0.043803/0.06953897/0.04691962, positively correlated
#more uppercase letters in description, more views/likes/comment

#correlation between <views>/<likes>/<comment_count> and proportion of exclamations in <description>         
summary(str_count(data$description,pattern='!')) 
data$proportionExclamation = str_count(data$description,pattern='!')/nchar(as.character(data$description))    
data[is.na(data$proportionExclamation),'proportionExclamation']<-0
data$proportionExclamation
cor(data$proportionExclamation,data$views)
cor(data$proportionExclamation,data$likes)
cor(data$proportionExclamation,data$comment_count)
#-0.03218878/-0.02328519/-0.004441792, not positively correlated
         
#top common words in <description> (stop words excluded)
data%>%
  unnest_tokens(input = as.character(description), output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()
#too complex, not in character vector form; need corpus

########Text Mining - Predictive Analysis: <description>########

install.packages("tm")
install.packages("NLP")
library(NLP)
library(tm)
corpus = Corpus(VectorSource(data$description))
#change to lowercase
corpus = tm_map(corpus,FUN = content_transformer(tolower))
#exclude URL
corpus = tm_map(corpus,
                FUN = content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*',
                                                                replacement = ' ',x = x)))       
#exclude punctuation
corpus = tm_map(corpus,FUN = removePunctuation)
#exclude stopwords
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
#exclude space
corpus = tm_map(corpus,FUN = stripWhitespace)

#define dictionary of words
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(data$description))),
                     lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))
corpus = tm_map(corpus,FUN = stemDocument)
corpus[[500]][1]#random check
dtm = DocumentTermMatrix(corpus)
dtm
inspect(dtm[,"good"])#random check

#define dictionary of most common words
xdtm = removeSparseTerms(dtm,sparse = 0.9)
xdtm
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))
sort(colSums(xdtm),decreasing = T)
#conclusion: most common words in description are video, music, new,...

#find most common words when views>10000000
data_data = cbind(views = data$views,xdtm)
sort(colSums(data_data[data_data$views>10000000,-data_data$views]),decreasing = T)

#Document Term Matrix - tfidf 
#weight of most common words in description
dtm_tfidf = DocumentTermMatrix(x=corpus,
                               control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf = removeSparseTerms(dtm_tfidf,sparse = 0.95)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))
colnames(xdtm_tfidf) = stemCompletion(x = colnames(xdtm_tfidf),
                                      dictionary = dict_corpus,
                                      type='prevalent')
colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing = T)

#use regression tree (cart model) to predice number of views
youtube_data = cbind(views = data$views,xdtm)
set.seed(617)
split = sample(1:nrow(youtube_data),size = 0.7*nrow(youtube_data))
train = youtube_data[split,]
test = youtube_data[-split,]
library(rpart)
library(rpart.plot)
tree = rpart(views~.,train)
rpart.plot(tree)

#use regression tree (cart model) to predice number of likes
youtube_data2 = cbind(likes = data$likes,xdtm)
set.seed(617)
split2 = sample(1:nrow(youtube_data2),size = 0.7*nrow(youtube_data2))
train2 = youtube_data2[split2,]
test2 = youtube_data2[-split2,]
tree2 = rpart(likes~.,train2)
rpart.plot(tree2)

#use regression tree (cart model) to predice number of dislikes
youtube_data3 = cbind(dislikes = data$dislikes,xdtm)
set.seed(617)
split3 = sample(1:nrow(youtube_data3),size = 0.7*nrow(youtube_data3))
train3 = youtube_data3[split3,]
test3 = youtube_data3[-split3,]
tree3 = rpart(dislikes~.,train3)
rpart.plot(tree3)

#use regression tree (cart model) to predice number of comments
youtube_data4 = cbind(comments = data$comment_count,xdtm)
set.seed(617)
split4 = sample(1:nrow(youtube_data4),size = 0.7*nrow(youtube_data4))
train4 = youtube_data4[split4,]
test4 = youtube_data4[-split4,]
tree4= rpart(comments~.,train4)
rpart.plot(tree4)


########Clustering: <tags> and <description>########

#split data, 10% in train (in order to run dist function successfully, cut observations)
install.packages("ggplot2")
install.packages("lattice")
library(caret)
set.seed(1706)
split = createDataPartition(y=data$video_id,p = 0.1,list = F,groups = 100)
train = data[split,]
test = data[-split,]
#omit missing value
dim(train)
sum(is.na(train))
data=na.omit(train)
#keep 2 numeric values about <tags> and <description>; normalize data
str(train)
train0=train[,c('number_of_tags','length_of_description')]
train0
clus_data = scale(train0)
clus_data

#Hierarchical Clustering
distances = dist(x = clus_data, method = "euclidean")
str(distances)
clust = hclust(distances,method = "ward.D2")
plot(clust)
#detect how well clust dendrogram matchs true distances
cor(cophenetic(clust),distances) 
#0.5687104, quite well

#cluster visualization
#from the plot chart it's reasonable to have 4 clusters
rect.hclust(tree=clust,k = 4)
install.packages("dendextend")
library(dendextend)
plot(color_branches(as.dendrogram(clust),k=4))
#number of observations in each cluster
x=cutree(tree=clust,k=4)
table(x) 
#2982,2268,1560,700

#observation visualization
install.packages("psych")
library(psych)
temp = data.frame(cluster = factor(x),
                  factor1 = fa(clus_data,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(clus_data,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

#compare between clusters
data_withcluster=cbind(train,x)
data_withcluster
str(data_withcluster)
install.packages("dplyr")
library(dplyr)
data_withcluster %>%
  select(views:comment_count,number_of_tags:length_of_description, x)%>%
  group_by(x)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

#conclusion: 
#cluster1 has the least tags and description
#cluster4 has the most tags and description
#simply comparing cluster 1 and 4 can do

#effect on <views>
library(tidyr)
data_withcluster %>%
  select(views:comment_count,number_of_tags:length_of_description, x)%>%
  group_by(x)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,views)%>%
  ggplot(aes(x=var,y=value,fill=factor(x)))+
  geom_col(position='dodge')+
  coord_flip()
#conclusion: the more tags and description, the more views

#effect on <likes>
data_withcluster %>%
  select(views:comment_count,number_of_tags:length_of_description, x)%>%
  group_by(x)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,likes)%>%
  ggplot(aes(x=var,y=value,fill=factor(x)))+
  geom_col(position='dodge')+
  coord_flip()
#conclusion: no obvious difference

#effect on <dislikes>
data_withcluster %>%
  select(views:comment_count,number_of_tags:length_of_description, x)%>%
  group_by(x)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,dislikes)%>%
  ggplot(aes(x=var,y=value,fill=factor(x)))+
  geom_col(position='dodge')+
  coord_flip()
#conclusion: the more tags and description, the less dislikes

#effect on <comments>
data_withcluster %>%
  select(views:comment_count,number_of_tags:length_of_description, x)%>%
  group_by(x)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,comment_count)%>%
  ggplot(aes(x=var,y=value,fill=factor(x)))+
  geom_col(position='dodge')+
  coord_flip()
#conclusion: no obvious difference

