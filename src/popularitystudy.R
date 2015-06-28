install.packages(c("caTools","tm","rpart","gplots","limSolve","quadprog","tcltk","discretization","lsa","lattice","zoo","TTR"))
library('caTools')
library('tm')
library('rpart')
library('gplots')
library('limSolve')
library('quadprog')
library('igraph')
library('tcltk')
library('discretization')
library('lsa')
library('lattice')
library('zoo')
library('TTR')

dir= "~/Dropbox/learning_algo_on_Kelly/assonam/data/lemondebigdata.csv";
dir= "~/Dropbox/learning_algo_on_Kelly/assonam/data/testbigdata.csv";
file=read.csv(dir, header = TRUE, sep = ";" ,encoding="UTF-8", stringsAsFactors=FALSE);
posts<-c(file$posts);
likes<-c(file$likes);
#k<-1
#n<-60
#posts<-posts[(1+(k-1)*n):(k*n)]
#likes<-likes[(1+(k-1)*n):(k*n)]
comments<-c(file$comments);
logposts<-array(0,c(length(likes)))
meanlikes<-array(0,c(length(likes)))
meanmeanlikes<-array(0,c(50))
X<-array(0,max(posts))
Y<-array(0,max(posts)) 
logposts<-log(posts)
loglikes<-log(likes)
plot(posts,likes,pch=20)
logmeanlikes<-array(0,c(length(likes)))
for(i in 1:length(likes)){
  if(posts[i]==0){
    meanlikes[i]<-0}
  else{meanlikes[i]<-(likes[i])/(posts[i])}
}

for(i in 1:max(posts)){
  if(meanlikes[posts=i]==0){X[i]<-0;
  Y[i]<-i}
  else{X[i]<-mean(likes[posts=i]);
  #else{X[i]<-mean(meanlikes[posts=i]);
  Y[i]<-i}  
}
Z<-rollapply(X, width = 20, FUN = mean, align = "left")
plot(Z, type="o", col="blue")
model<-lm(meanmeanlikes~X)
model<-lm(X~Y
          + I(Y^2))

length(X)
#plot(posts,meanlikes)
print(model)
abline(model)
summary(model)







topics1<-unique(topics);
d<-length(topics);
d1<-length(topics1)
n<-10
K<-round(d/n)-1
X<-array(0,c(K))
Z<-array(0,c(K,d1))
for(f in 1:K){
  for(i in (1+(f-1)*n):(f*n)){
    for(j in 1:d1){
      if(topics[i]==topics1[j]){
        Z[f,j]<-Z[f,j]+1}
      else{
        Z[f,j]<-Z[f,j]
        }
      X[f]<-ranks[i]+X[f]
      }
    }
  }
plot(Z[,2],(1/n)*X)
