#LIBRARIES----------------------------------------------------------------------
library(readxl) #read_excel
library(randomForest) #randomForest
library(cluster) #clara
library(factoextra) #fviz_cluster
library(ggplot2) #ggplot
library(writexl) #write_xlsx
library(caret) #createDataPartition, confusionMatrix
library(adabag) #boosting
library(pROC) #roc

#DATA---------------------------------------------------------------------------
setwd("C:/Users/mutte/Downloads/grad school/mat 400")
IH.df <- read_excel("cleanedIHdata.xlsx") #only read in first sheet of file

#DATA CLEANING------------------------------------------------------------------
#remove features not taken into consideration
df <- IH.df[ ,-c(1:4,6:7,10:13,19:23)]

#remove any observations with missing values
clean.df <- na.omit(df)

#remove outliers determined by outside analysis
IH.data <- clean.df[-c(14,18,26,28,29,39,42,44,61), ] #B,AST,AST,B,B,B,B,C,B

#data analysis
mean(IH.data$Age)
mean(IH.data$Crtn_Peak)
mean(IH.data$INR_Peak)
mean(IH.data$ALT_Peak)
mean(IH.data$AST_Peak)
mean(IH.data$Blrb_Peak)

sum(IH.data$Ethnicity==1) #hispanic
sum(IH.data$Ethnicity==2) #nonhispanic

sum(IH.data$Gender=="male")
sum(IH.data$Gender=="female")

sum(IH.data$Outcome==0) #death
sum(IH.data$Outcome==1) #survival

median(IH.data$Age)
median(IH.data$Crtn_Peak)
median(IH.data$INR_Peak)
median(IH.data$ALT_Peak)
median(IH.data$AST_Peak)
median(IH.data$Blrb_Peak)


#CLUSTER------------------------------------------------------------------------

#applied CORNELIOUS function to get clusters and attach as column to data
cluster.data <- function(data.in,n.clusters){
  set.seed(3)
  rf.unsup <- randomForest(x=data.in[ ,-9], mtry=ncol(data.in)-2, ntree=500, proximity=TRUE)
  rf.prox <- rf.unsup$proximity
  clara.rf <- clara(rf.prox,n.clusters, cluster.only=FALSE, samples=5)
  data.in.cluster <- cbind(data.in,clara.rf$clustering)
  print(summary(silhouette(clara.rf)))
  return(data.in.cluster)
}

#tune for number of clusters by largest mean individual silhouette width
for (i in 2:8){
  cluster.data(IH.data, i)
}

#get clustered data settling on 2 clusters
set.seed(3)
data.clustered <- cluster.data(IH.data, 2)

rf.unsup <- randomForest(x=IH.data[ ,-9], mtry=ncol(IH.data)-2, ntree=500, proximity=TRUE)
rf.prox <- rf.unsup$proximity
clara.df <- clara(rf.prox, 2, cluster.only=FALSE, samples=5)

#plot clusters
fviz_cluster(clara.df) + xlab("") + ylab("") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  ggtitle("Clusters") + theme(plot.title=element_text(hjust=0.5)) + 
  scale_colour_manual(values = c("firebrick3", "dodgerblue3")) +
  scale_fill_manual(values = c("firebrick3", "dodgerblue3"))

#death.index <- which(IH.data$Outcome==1)

#cluster 1 analysis
cluster1 <- IH.data[clara.df$clustering==1,] #size 84

mean(cluster1$Age)
mean(cluster1$Crtn_Peak)
mean(cluster1$INR_Peak)
mean(cluster1$ALT_Peak)
mean(cluster1$AST_Peak)
mean(cluster1$Blrb_Peak)

sum(cluster1$Ethnicity==1) #hispanic
sum(cluster1$Ethnicity==2) #nonhispanic

sum(cluster1$Gender=="male")
sum(cluster1$Gender=="female")

sum(cluster1$Outcome==0) #death
sum(cluster1$Outcome==1) #survival

#cluster 2 analysis
cluster2 <- IH.data[clara.df$clustering==2,] #size 70

mean(cluster2$Age)
mean(cluster2$Crtn_Peak)
mean(cluster2$INR_Peak)
mean(cluster2$ALT_Peak)
mean(cluster2$AST_Peak)
mean(cluster2$Blrb_Peak)

sum(cluster2$Ethnicity==1) #hispanic
sum(cluster2$Ethnicity==2) #nonhispanic

sum(cluster2$Gender=="male")
sum(cluster2$Gender=="female")

sum(cluster2$Outcome==0) #death
sum(cluster2$Outcome==1) #survival


#write_xlsx(data.clustered,"test_out.xlsx")


#length(intersect(which(IH.data$Gender=="male"), which(IH.data$Outcome==0)))
#length(intersect(which(IH.data$Gender=="male"), which(IH.data$Outcome==1)))
#length(intersect(which(IH.data$Gender=="female"), which(IH.data$Outcome==0)))
#length(intersect(which(IH.data$Gender=="female"), which(IH.data$Outcome==1)))

#length(intersect(which(IH.data$Ethnicity==1), which(IH.data$Outcome==0)))
#length(intersect(which(IH.data$Ethnicity==1), which(IH.data$Outcome==1)))
#length(intersect(which(IH.data$Ethnicity==2), which(IH.data$Outcome==0)))
#length(intersect(which(IH.data$Ethnicity==2), which(IH.data$Outcome==1)))



#SUBSETS------------------------------------------------------------------------
set.seed(1)
index <- createDataPartition(unlist(data.clustered[ ,9]), p=0.8, list=FALSE, times=1)
data.clustered$Outcome <- as.factor(data.clustered$Outcome)
train <- data.clustered[index,-10]
test <- data.clustered[-index,-10]

test.index.mat <- row(test, as.factor=TRUE)
test.index.vec <- as.vector(test.index.mat[,1])
test.index <- as.numeric(test.index.vec)


#RANDOM FORESTS-----------------------------------------------------------------
set.seed(2)

#baseline RF
rf <- randomForest(Outcome~., data=train, ntree=1000, mtry=3)

pred.rf <- predict(rf, newdata=test, type="prob")
prob.rf <- pred.rf[,2] #probability of 1 (survival)
yhat.rf <- round(prob.rf)

confusionMatrix(data=factor(yhat.rf), reference=factor(test$Outcome))
roc.rf <- roc(test$Outcome, prob.rf, plot=TRUE, print.auc=TRUE, print.auc.y=0.15)

#baseline with adaboost
rf.ada <- boosting(Outcome~., data=train, mfinal=100) 

pred.rf.ada <- predict(rf.ada, newdata=test)
prob.rf.ada <- pred.rf.ada$prob[,2] #probability of 1 (survival)
yhat.rf.ada <- pred.rf.ada$class

confusionMatrix(data=factor(yhat.rf.ada), reference=factor(test$Outcome))
roc.rf.ada <- roc(test$Outcome, prob.rf.ada, plot=TRUE, print.auc=TRUE, print.auc.y=0.15)

#cluster 1 RF
train1.index <- intersect(index, which(data.clustered[,10]==1))
test1.index <- intersect(test.index, which(data.clustered[,10]==1))

train1 <- data.clustered[train1.index,-10]
test1 <- data.clustered[test1.index,-10]

rf1 <- randomForest(Outcome~., data=train1, ntree=1000, mtry=3)

pred.rf1 <- predict(rf1, newdata=test1, type="prob")
prob.rf1 <- pred.rf1[,2] #probability of 1 (survival)
yhat.rf1 <- round(prob.rf1)

confusionMatrix(data=factor(yhat.rf1), reference=test1$Outcome)
roc.rf1 <- roc(test1$Outcome, prob.rf1, plot=TRUE, print.auc=TRUE, print.auc.y=0.15)

#cluster 2 RF
train2.index <- intersect(index, which(data.clustered[,10]==2))
test2.index <- intersect(test.index,  which(data.clustered[,10]==2))

train2 <- data.clustered[train2.index,-10]
test2 <- data.clustered[test2.index,-10]

rf2 <- randomForest(Outcome~., data=train2, ntree=1000, mtry=3)

pred.rf2 <- predict(rf2, newdata=test2, type="prob")
prob.rf2 <- pred.rf2[,2] #probability of 1 (survival)
yhat.rf2 <- round(prob.rf2)

confusionMatrix(data=factor(yhat.rf2, levels=c(0,1)), reference=test2$Outcome)
roc.rf2 <- roc(test2$Outcome, prob.rf2, levels=c(0,1), plot=TRUE, print.auc=TRUE, print.auc.y=0.15)

#clustered RF
test3 <- rbind(test1, test2)

prob.rf3 <- c(prob.rf1, prob.rf2)
yhat.rf3 <- round(prob.rf3)

confusionMatrix(data=factor(yhat.rf3), reference=test3$Outcome)
roc.rf3 <- roc(test3$Outcome, prob.rf3, levels=c(0,1), plot=TRUE, print.auc=TRUE, print.auc.y=0.15)


#ROC CURVES-----------------------------------------------------------------
plot(roc.rf, col="forestgreen", lty=1, lwd=4, cex.lab=1, cex.axis=1, main="ROC Curves") 
plot(roc.rf.ada, col="goldenrod2", lty=1, lwd=4, add=TRUE)
plot(roc.rf1, col="firebrick3", lty=1, lwd=4, add=TRUE) 
plot(roc.rf2, col="dodgerblue3", lty=1, lwd=4, add=TRUE) 
plot(roc.rf3, col="darkmagenta", lty=1, lwd=4, add=TRUE) 
legend("bottomright", cex=1,
       legend=c("RF", "AdaBoost RF", "Cluster 1 RF", "Cluster 2 RF", "Clusterd RF"),
       col=c("forestgreen","goldenrod2","firebrick3","dodgerblue3","darkmagenta"), lty=(1), lwd=(4))


#HERE---------------------------------------------------------------------------
library(dplyr) #bind_col
library(data.table) #transpose
library(geometry) #

#CLUSTER ANALYSIS---------------------------------------------------------------

#CORNELIOUS cluster variable mean
getcluster.var.mean<-function(data.in,n.clusters,var.num){
  storage.list<-list()
  if(class(data.in[,var.num])=="factor"){
    cnt<-nlevels(data.in[,var.num])
    for(i in 1:cnt){
      storage.vec<-rep(NA,n.clusters)
      for(j in 1:n.clusters){
        storage.vec[j]<-nrow(data.in[(data.in[,ncol(data.in)]==j) & (data.in[,var.num]==levels(data.in[,var.num])[i]),])/nrow(data.in[data.in[,ncol(data.in)]==j,])
      }
      storage.list[[i]]<-storage.vec
    }
    out.df<-as.data.frame(storage.list,col.names = levels(data.in[,var.num]))
  } else{
    storage.vec<-rep(NA,n.clusters)
    for(k in 1:n.clusters){
      storage.vec[k]<-mean(data.in[data.in[,ncol(data.in)]==k,var.num])
    }
    storage.list[[1]]<-storage.vec
    out.df<-as.data.frame(storage.list,col.names = colnames(data.in)[var.num])
  }
  return(out.df)
}

#CORNELIOUS data frame with all variable means
get.means.df <- function(data.in,n.vars,n.clusters){
  list.store <- list()
  for(i in 1:(n.vars)){
    if(i!=10){
      list.store[[i]] <- getcluster.var.mean(data.in,n.clusters,i)
    }
  }
  return(bind_cols(list.store))
}
data.cluster.df <- get.means.df(data.clustered,10,2)

#CORNELIOUS data means
get.sample.means<-function(data.in,n.vars){
  storage.list<-list()
  for(i in 1:n.vars){
    if(class(data.in[,i])=="factor"){
      cnt <- nlevels(data.in[,i])
      storage.vec <- rep(NA,cnt)
      for(j in 1:cnt){
        storage.vec[j] <- nrow(data.in[data.in[,i]==levels(data.in[,i])[j],])/nrow(data.in)
      }
      storage.list[[i]] <- transpose(as.data.frame(storage.vec,col.names = levels(data.in[,i])))
    } else {
      storage.num <- mean(data.in[,i])
      storage.list[[i]] <- transpose(as.data.frame(storage.num,col.names = colnames(data.in)[i]))
    }
  }
  return(storage.list)
}

test.df <- get.sample.means(data.clustered,10)
sample.means.df <- bind_cols(test.df)

#outputting sample means
write_xlsx(sample.means.df, "test_out.xlsx")
write_xlsx(data.cluster.df, "test_out.xlsx")
summary(as.factor(data.clustered$clara.rf))


#EXPLORATION--------------------------------------------------------------------
data.mat <- data.clustered #format non numerical columns

#change gender to 0 for female and 1 for male
mat.gender <- rep(1, nrow(data.mat))
for (i in 1:nrow(data.mat)){
  if (data.mat$Gender[i]=="female"){
    mat.gender[i] <- 0
  }
  else{
    mat.gender[i] <- 1
  }
}
data.mat$Gender <- mat.gender
data.mat$Outcome <- as.numeric(data.clustered$Outcome)-1

data.matrix <- data.mat[,-10]

k.means <- kmeans(data.matrix, 2)
data.explor <- cbind(data.matrix, k.means$cluster)

#CORNELIOUS 2d projection
norm_vec<-function(x){
  sqrt(sum(x^2))
}

get_plot_pt<-function(obs){
  obs.mod<-rep(NA,9)
  for (i in 1:9){
    obs.mod[i]<-obs[i]
  }
  w1<-rep(1,9)
  w2<-rep(0,9)
  #w2[c(3,5,7,9)]<-1
  w1hat<-w1/norm_vec(w1)
  w2apo<-w2-dot(w2,w1hat)*w1hat
  w2hat<-w2apo/norm_vec(w2apo)
  plotpt<-c(dot(obs.mod,w1hat),dot(obs.mod,w2hat))
  return(plotpt)
}

two_dim <- setNames(data.frame(matrix(ncol=3, nrow=154)), c("x", "y", "cluster"))

for(i in 1:154){
  obs <- get_plot_pt(data.explor[i,])
  two_dim[i,1] <- obs[1]
  two_dim[i,2] <- obs[2]
}

for(i in 1:154){
  two_dim[i,3] <- k.means$cluster[i]
}

#density plots for deaths v survivors
deaths <- which(data.explor[,9]==1)
survive <- (-deaths)

#death group density plot
p1 <- ggplot(data=two_dim[deaths,], aes(x=two_dim[deaths,1], y=two_dim[deaths,2]), bins=100) +
  geom_bin2d() + scale_fill_gradient(low="lightpink2", high="darkred") + xlab("") + ylab("") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle("Deaths") + theme(plot.title=element_text(hjust=0.5))

#survivor group density plot
p2 <- ggplot(data=two_dim[survive,], aes(x=two_dim[survive,1], y=two_dim[survive,2]), bins=100) +
  geom_bin2d() + scale_fill_gradient(low="lightblue2", high="darkblue") + xlab("") + ylab("") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  ggtitle("Survivors") + theme(plot.title=element_text(hjust=0.5))

#density plots for clusters
fviz_cluster(k.means,two_dim[,-3]) + xlab("") + ylab("") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  ggtitle("Clusters") + theme(plot.title=element_text(hjust=0.5))

#get percentage of membership in certain feature levels by cluster
age.cluster <- plot(x=data.clustered$Age, y=as.factor(data.explor[,10]))
sex.cluster <- plot(x=data.clustered$Gender, y=as.factor(data.explor[,10]))
race.cluster <- plot(x=data.clustered$Ethnicity, y=as.factor(data.explor[,10]))

