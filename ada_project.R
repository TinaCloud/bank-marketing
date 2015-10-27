
setwd('C:/Users/Eric Drew/Documents/R/data')
data <- read.table("bank-additional-full.csv", sep=';', header=T) #41188
data$duration <- NULL #remove duration

library(ggplot2)
library(car)
library(MASS)
library(caret)
library(fpc)
library(RColorBrewer)
library(kohonen)
set.seed(1)

#logistic regression ON loan********************************************************************************************************
sp <- split(data, data$loan)
train <- rbind(data.frame(sp$no), rbind(data.frame(sp$yes))) 
test <- data.frame(sp$unknown)

model <- glm( loan ~ job  + emp.var.rate	+ cons.price.idx	+ cons.conf.idx, data=train, family = binomial)
summary(model)

pred <- predict(model, newdata = train, type = "response")
pred <- ifelse(pred >= .5, "yes", "no")
train$pred <- pred
train$pred <- NULL

pred <- predict(model, newdata = test, type = "response")
pred <- ifelse(pred >= .5, "yes", "no")
test$loan <- pred

data <- rbind(train, test) 
data <- droplevels(data)

#logistic regression ON housing********************************************************************************************************
sp <- split(data, data$housing)
train <- rbind(data.frame(sp$no), rbind(data.frame(sp$yes))) 
test <- data.frame(sp$unknown)

model <- glm( housing ~ job	+ loan	+ month	+ day_of_week	+ emp.var.rate	+ cons.price.idx	+ cons.conf.idx	+ euribor3m	+ nr.employed, data=data, family = binomial)
summary(model)

pred <- predict(model, newdata = train, type = "response")
pred <- ifelse(pred >= .5, "yes", "no")
train$pred <- pred
train$pred <- NULL

pred <- predict(model, newdata = test, type = "response")
pred <- ifelse(pred >= .5, "yes", "no")
test$housing <- pred

data <- rbind(train, test) 
data <- droplevels(data)

#logistic regression ON default********************************************************************************************************
sp <- split(data, data$default)
train <- rbind(data.frame(sp$no), rbind(data.frame(sp$yes))) 
test <- data.frame(sp$unknown)

model <- glm( default ~ age + job + marital  + education	+ month	+ poutcome	+ emp.var.rate	+ cons.price.idx	+ cons.conf.idx	+ euribor3m + y, data=data, family = binomial)
summary(model)

pred <- predict(model, newdata = train, type = "response")
pred <- ifelse(pred >= .5, "yes", "no")
train$pred <- pred
train$pred <- NULL

pred <- predict(model, newdata = test, type = "response")
pred <- ifelse(pred >= .5, "yes", "no")
test$default <- pred

data <- rbind(train, test) 
data <- droplevels(data)

#LDA regression ON education********************************************************************************************************
sp <- split(data, data$education)
train <- rbind(data.frame(sp$basic.4y), data.frame(sp$basic.6y), data.frame(sp$basic.9y), data.frame(sp$high.school), data.frame(sp$illiterate), data.frame(sp$professional.course), data.frame(sp$university.degree)) 
test <- data.frame(sp$unknown)
train <- droplevels(train)


ldaModel<- lda(education ~ age + job + marital  + default	+ housing	+ loan	+ contact	+ month	+ day_of_week	+ campaign	+ pdays	+ previous	+ poutcome	+ emp.var.rate	+ cons.price.idx	+ cons.conf.idx	+ euribor3m	+ nr.employed + y, data = train)
ldaModel

pred <- predict(ldaModel, train)
train$pred <- pred$class
train$pred <- NULL

matrix <- confusionMatrix(train$education, pred$class)
matrix$overall[1]

pred <- predict(ldaModel, test)
test$education <- pred$class

data <- rbind(train, test) 
data <- droplevels(data)

#LDA regression ON job********************************************************************************************************
sp <- split(data, data$job)
train <- rbind(data.frame(sp$admin.), data.frame(sp$'blue-collar'), data.frame(sp$entrepreneur), data.frame(sp$housemaid), data.frame(sp$management), data.frame(sp$retired), data.frame(sp$'self-employed'), data.frame(sp$services), data.frame(sp$student), data.frame(sp$technician), data.frame(sp$unemployed)) 
test <- data.frame(sp$unknown)
train <- droplevels(train)


ldaModel<- lda(job ~ age + education + marital  + default  + housing	+ loan	+ contact	+ month	+ day_of_week	+ campaign	+ pdays	+ previous	+ poutcome	+ emp.var.rate	+ cons.price.idx	+ cons.conf.idx	+ euribor3m	+ nr.employed + y, data = train)
ldaModel

pred <- predict(ldaModel, train)
train$pred <- pred$class
train$pred <- NULL

matrix <- confusionMatrix(train$job, pred$class)
matrix$overall[1]


pred <- predict(ldaModel, test)
test$job <- pred$class

data <- rbind(train, test) 
data <- droplevels(data)

#LDA regression ON marital********************************************************************************************************
sp <- split(data, data$marital)
train <- rbind(data.frame(sp$divorced), data.frame(sp$married), data.frame(sp$single)) 
test <- data.frame(sp$unknown)
train <- droplevels(train)


ldaModel<- lda(marital ~ age + education + job  + default  + housing  + loan	+ contact	+ month	+ day_of_week	+ campaign	+ pdays	+ previous	+ poutcome	+ emp.var.rate	+ cons.price.idx	+ cons.conf.idx	+ euribor3m	+ nr.employed + y, data = train)
ldaModel

pred <- predict(ldaModel, train)
train$pred <- pred$class
train$pred <- NULL

matrix <- confusionMatrix(train$marital, pred$class)
matrix$overall[1]

pred <- predict(ldaModel, test)
test$marital <- pred$class

data <- rbind(train, test) 
data <- droplevels(data)

#EXPLORATION***************************************************************************************************************************************
#age variable - numeric
summary(data$age)
boxplot(data$age) #SHOULD THIS BE TRANSFORMED BECAUSE IT"S SKEWED??? COMPARE MODEL BEFORE TO AFTER
ggplot(data, aes(x=age, fill=y)) + geom_density(alpha=.3)

#marital variable - categorical
summary(data$marital) # 80 unknown
plot(data$marital)
prop.table(table(data$marital)) * 100

#job variable - categorical
summary(data$job) #330 unknown
plot(data$job)
prop.table(table(data$job)) * 100

#education variable - categorical
summary(data$education) #1731 unknown
plot(data$education)
prop.table(table(data$education)) * 100

#default variable - categorical
summary(data$default) #8597 unknown
plot(data$default)
prop.table(table(data$default)) * 100

#housing variable - categorical 
summary(data$housing) #990 unknown
plot(data$housing)
prop.table(table(data$housing)) * 100

#loan variable - categorical
summary(data$loan) #990 unknown
plot(data$loan)
prop.table(table(data$loan)) * 100
levels(data$loan)

#contact variable - categorical
summary(data$contact)
plot(data$contact)
prop.table(table(data$contact)) * 100

#month variable - categorical
summary(data$month) #33.42% in May
plot(data$month)
prop.table(table(data$month)) * 100

#day_of_week variable - categorical
summary(data$day_of_week) #NO WEEKEND CALLS
plot(data$day_of_week)
prop.table(table(data$day_of_week)) * 100

#campaign variable - numeric
summary(data$campaign) #TRANSFORM????
ggplot(data, aes(x=campaign, fill=y)) + geom_density(alpha=.3) #HIGHLY SKEWED. number of contacts performed during this campaign and for this client

#pdays variable - numeric - number of days that passed by after the client was last contacted from a previous campaign
summary(data$pdays) #999 MEANS THE CLIENT WAS NOT PREVIOUSLY CONTACTED IN PREVIOUS CAMPAIGNS
hist(data$pdays) #TRANSFORM INTO A BINARY???? BECAUSE OF THE LARGE PERCENT OF NOT PREVIOUSLY CONTACTED????
prop.table(table(data$pdays)) * 100 #96.3% not previously contacted

#previous variable - numeric
summary(data$previous) #number of contacts performed before this campaign and for this client
hist(data$previous) #TRANSFORM INTO A BINARY???? BECAUSE OF THE LARGE PERCENT OF NOT PREVIOUSLY CONTACTED????
prop.table(table(data$previous)) * 100 #86.34% not previously contacted

#poutcome variable - categorical - outcome of the previous marketing campaign
summary(data$poutcome)
plot(data$poutcome)
prop.table(table(data$poutcome)) * 100 #86.34% not previously contacted

#emp.var.rate variable - numeric
summary(data$emp.var.rate)
ggplot(data, aes(x=emp.var.rate, fill=y)) + geom_density(alpha=.3)

#cons.price.idx variable - numeric
summary(data$cons.price.idx)
ggplot(data, aes(x=cons.price.idx, fill=y)) + geom_density(alpha=.3)

#cons.conf.idx variable - numeric
summary(data$cons.conf.idx)
ggplot(data, aes(x=cons.conf.idx, fill=y)) + geom_density(alpha=.3)

#euribor3m variable - numeric
summary(data$euribor3m)
ggplot(data, aes(x=euribor3m, fill=y)) + geom_density(alpha=.3)

#nr.employed variable - numeric
summary(data$nr.employed)
ggplot(data, aes(x=nr.employed, fill=y)) + geom_density(alpha=.3)

#y variable - categorical
summary(data$y)
plot(data$y)
prop.table(table(data$y)) * 100

#correlation analysis
library(corrplot)
corrplot(cor(data[sapply(data, is.numeric)]))
plot(data[sapply(data, is.numeric)])

#convert variables****************************************************************************************************************
#TRANSFORM pdays
plot(sapply(data, var))
data$pdays <- ifelse(data$pdays== 999, 0, 1) #CHANGE TO A BINARY VARIABLE

#transform default
data$default <- ifelse(data$default=="yes", 1, 0) # 1 is yes

#transform housing
data$housing <- ifelse(data$housing=="yes", 1, 0) # 1 is yes

#transform loan
data$loan <- ifelse(data$loan=="yes", 1, 0) # 1 is yes

#transfrom contact
data$contact_cellular <- ifelse(data$contact=="cellular", 1, 0) # 1 is yes
data$contact <- NULL

for(i in names(data)){
  if(is.factor(data[[i]])){
    for(level in unique(data[[i]])){
      data[paste(i, level, sep = "_")] <- ifelse(data[[i]] == level, 1, 0)
    }
  }
}
data = subset(data, select=-c(job,marital,education, month, day_of_week, poutcome,y_no, y_yes ))

data$education_illiterate <- NULL
data$marital_divorced <- NULL
data$job_student <- NULL
data$month_sep <- NULL
data$day_of_week_mon <- NULL
data$poutcome_success <- NULL

#move Y
data$y_temp <- ifelse(data$y=="yes", 1, 0) # 1 is yes
data$y <- NULL
data$y <- data$y_temp
data$y_temp <- NULL

#PCA on all****************************************************************************************************************

pca_fit <- princomp(data[,1:46], cor=TRUE, scores=TRUE)
summary(pca_fit)
screeplot(pca_fit, 63) #first 15
pca_fit$scores
pca_fit$loadings
pca_dims <- pca_fit$scores[,1:9]

#FACTOMINER PCA***********************************************************************************************************
library(FactoMineR)
res.pca <- PCA(data[1:19], quali.sup = c(2:10, 14), ncp= 5, scale.unit=TRUE, graph=T)
summary(res.pca)
plot(res.pca$eig$eigenvalue, type='l', , main='Scree Plot', xlab='dimensions', ylab='eigen values') 
pca_dims <- data.frame(res.pca$ind$coord)
som <- res.pca$ind$coord

qplot(x=pca_dims[,1], y=pca_dims[,5], data=pca_dims, color=factor(data$job)) + xlab('PCA1')+ylab('PCA5')
qplot(x=pca_dims[,1], y=pca_dims[,5], data=pca_dims, color=factor(data$marital)) + xlab('PCA1')+ylab('PCA5') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$education)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$default)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$housing)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$loan)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$contact)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$month)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$day_of_week)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$poutcome)) + xlab('PCA1')+ylab('PCA2')

description <- dimdesc(res.pca, axes=c(1,2,3,4,5))
dim1 <- rbind(matrix(sqrt(description$Dim.1$quali[,1])), matrix(description$Dim.1$quanti[,1]))
row.names(dim1) <- c(dimnames(description$Dim.1$quali)[[1]], dimnames(description$Dim.1$quanti)[[1]])

dim2 <- rbind(matrix(sqrt(description$Dim.2$quali[,1])), matrix(description$Dim.2$quanti[,1]))
row.names(dim2) <- c(dimnames(description$Dim.2$quali)[[1]], dimnames(description$Dim.2$quanti)[[1]])

dim3 <- rbind(matrix(sqrt(description$Dim.3$quali[,1])), matrix(description$Dim.3$quanti[,1]))
row.names(dim3) <- c(dimnames(description$Dim.3$quali)[[1]], dimnames(description$Dim.3$quanti)[[1]])

dim4 <- rbind(matrix(sqrt(description$Dim.4$quali[,1])), matrix(description$Dim.4$quanti[,1]))
row.names(dim4) <- c(dimnames(description$Dim.4$quali)[[1]], dimnames(description$Dim.4$quanti)[[1]])

dim5 <- rbind(matrix(sqrt(description$Dim.5$quali[,1])), matrix(description$Dim.5$quanti[,1]))
row.names(dim5) <- c(dimnames(description$Dim.5$quali)[[1]], dimnames(description$Dim.5$quanti)[[1]])

description$Dim.3$category
description$Dim.5$category
description$Dim.6$category

#PCA LOADINGS***********************************************************************************************************

#PCA 1
pca1 = dim1
cutoff = .5
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 1", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA 2
pca1 = dim2
cutoff = .4
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 2", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA 3
pca1 = dim3
cutoff = .3
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 3", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA 4
pca1 = dim4
cutoff = .25
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 4", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA 5
pca1 = dim5
cutoff = .2
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 5", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA 6
pca1 = dim6
cutoff = .2
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 6", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA ANALYSIS*********************************************************************************************************************************

#PCA1
df <- data.frame(pca_dims[,1], data$previous, data$poutcome, data$nr.employed, data$month, data$euribor3m, data$emp.var.rate, data$cons.price.idx)
df <- df[order(-df[,1]),]
#low END - customers not contacted in previous campaigns, when interest rates are high, employment is high, the economy in general is strong, they were contacted by telephone, 

#PCA2
df <- data.frame(pca_dims[,2], data$previous, data$poutcome, data$pdays, data$cons.conf.idx)
df <- df[order(-df[,1]),]
#LOW END

#PCA3
df <- data.frame(pca_dims[,3], data$month, data$job, data$cons.conf.idx, data$campaign, data$age)
df <- df[order(-df[,1]),]
#LOW END

#PCA4
df <- data.frame(pca_dims[,4], data$campaign, data$age)
df <- df[order(df[,1]),]
#LOW END

#PCA5
df <- data.frame(pca_dims[,5], data$month, data$marital, data$job, data$cons.price.idx, data$cons.conf.idx, data$campaign, data$age)
df <- df[order(df[,1]),]
#HIGH END

#PCA6
df <- data.frame(pca_dims[,6], data$previous, data$poutcome_nonexistent, data$poutcome_failure, data$nr.employed, data$month_may, data$month_jul, data$contact_cellular, data$cons.conf.idx)
df <- df[order(df[,1]),]
#LOW END

#PCA7
df <- data.frame(pca_dims[,7], data$job_services, data$job_retired, data$'job_blue-collar', data$education_university.degree, data$education_high.school, data$education_basic.9y)
df <- df[order(df[,1]),]
#LOW END

#PCA8
df <- data.frame(pca_dims[,8], data$marital_single, data$marital_married, data$job_management, data$job_blue-collar, data$education_basic.4y, data$default)
df <- df[order(df[,1]),]
#LOW END

#PCA9
df <- data.frame(pca_dims[,9], data$month_may, data$month_jun, data$job_retired, data$cons.conf.idx)
df <- df[order(df[,1]),]
#LOW END

#DETERMINE NUMBER OF CLUSTERS******************************************************************************************
ls <- matrix(nrow = 15, ncol = 2)
for(i in 1:15){
  clusters.kmeans <- kmeans(data, i)
  ls[i,1] <- mean(clusters.kmeans$withinss)
  ls[i,2] <- mean(clusters.kmeans$betweenss)
}

matplot(ls, type='l')

#KMEANS****************************************************************************************************************
features <- data.frame(pca_dims)
clusters <- kmeans(features, 3, iter.max = 10000, nstart = 10) #3

qplot(x = pca_dims[,1], y=pca_dims[,2], data=data.frame(pca_dims), color=factor(clusters$cluster), xlab="comp.1", ylab="comp.2", main="K-Means Clustering",) + geom_point() + theme(legend.position="none") + geom_point(x= clusters$centers[,1], y=clusters$centers[,2], data=data.frame(clusters$centers), color='purple', size=100, alpha=.2) + annotate("text", x = clusters$centers[1,1], y = clusters$centers[1,2], label="group 1", colour = "blue", size=7) + annotate("text", x = clusters$centers[2,1], y = clusters$centers[2,2], label="group 2", colour = "blue", size=7) + annotate("text", x = clusters$centers[3,1], y = clusters$centers[3,2], label="group 3", colour = "blue", size=7)

plot(clusters$centers[,1], clusters$centers[,2])

clusters$centers


#DBSCAN****************************************************************************************************************
data_sample <- pca_dims[sample(1:nrow(pca_dims), 10000,replace=FALSE),] 
db <- dbscan(data_sample, 1.27,  MinPts = 6) #1.9 3
db
qplot(x = data_sample[,1], y=data_sample[,2], data=data.frame(data_sample), color=factor(db$cluster), , xlab="comp.1", ylab="comp.2", main="Density Based Clustering") + geom_point()

density <- data.frame(data_sample)
density$clust <- db$cluster
sp <- split(data.frame(data_sample), density$clust)
clust1 <- rbind(data.frame(sp$"1"))
clust2 <- rbind(data.frame(sp$"2"))
means <- apply(clust1,2,mean)
means
means <- apply(clust2,2,mean)
means

#SOM****************************************************************************************************************
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

set.seed(250)
model <-som(som, grid = somgrid(20, 20, "hexagonal"), keep.data = TRUE)
plot(model, type="count")

h.clust <- hclust(dist(model$codes))
plot(h.clust,main='node dendrogram')

par(mfrow=c(1,1))
mypalette<-brewer.pal(10,"Paired")
som_cluster <- cutree(hclust(dist(model$codes)), 4)
plot(model, type="mapping", bgcol = mypalette[som_cluster], main='SOM Cluster') 

par(mfrow=c(2,3))
plot(model, type = "property", property = model$codes[,1], main='comp.1', palette.name=coolBlueHotRed)
add.cluster.boundaries(model, som_cluster)
plot(model, type = "property", property = model$codes[,2], main='comp.2', palette.name=coolBlueHotRed)
add.cluster.boundaries(model, som_cluster)
plot(model, type = "property", property = model$codes[,3], main='comp.3', palette.name=coolBlueHotRed)
add.cluster.boundaries(model, som_cluster)
plot(model, type = "property", property = model$codes[,4], main='comp.4', palette.name=coolBlueHotRed)
add.cluster.boundaries(model, som_cluster)
plot(model, type = "property", property = model$codes[,5], main='comp.5', palette.name=coolBlueHotRed)
add.cluster.boundaries(model, som_cluster)
par(mfrow=c(1,1))







































#PCA YES/NO SPLIT*********************************************************************************************************************************************
#*****************************************************************************************************************************************************
sp <- split(data, data$y)
no <- rbind(data.frame(sp$"no"))
yes <- rbind(data.frame(sp$"yes"))
data <- yes
data <- no

#FACTOMINER PCA***********************************************************************************************************
library(FactoMineR)
res.pca <- PCA(data[1:19], quali.sup = c(2:10, 14), ncp= 3, scale.unit=TRUE, graph=T)
summary(res.pca)
plot(res.pca$eig$eigenvalue, type='l')
pca_dims <- data.frame(res.pca$ind$coord)
som <- res.pca$ind$coord

qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$job)) + xlab('PCA1')+ylab('PCA52')
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$marital)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$education)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$default)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$housing)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$loan)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$contact)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$month)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$day_of_week)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$poutcome)) + xlab('PCA1')+ylab('PCA2') 

description <- dimdesc(res.pca, axes=c(1,2,3))
dim1 <- rbind(matrix(sqrt(description$Dim.1$quali[,1])), matrix(description$Dim.1$quanti[,1]))
row.names(dim1) <- c(dimnames(description$Dim.1$quali)[[1]], dimnames(description$Dim.1$quanti)[[1]])

dim2 <- rbind(matrix(sqrt(description$Dim.2$quali[,1])), matrix(description$Dim.2$quanti[,1]))
row.names(dim2) <- c(dimnames(description$Dim.2$quali)[[1]], dimnames(description$Dim.2$quanti)[[1]])

dim3 <- rbind(matrix(sqrt(description$Dim.3$quali[,1])), matrix(description$Dim.3$quanti[,1]))
row.names(dim3) <- c(dimnames(description$Dim.3$quali)[[1]], dimnames(description$Dim.3$quanti)[[1]])

description$Dim.1$category
description$Dim.2$category
description$Dim.3$category

#PCA LOADINGS***********************************************************************************************************

#PCA 1
pca1 = dim1
cutoff = .5
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 1", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA 2
pca1 = dim2
cutoff = .4
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 2", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA 3
pca1 = dim3
cutoff = .3
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 3", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA ANALYSIS*********************************************************************************************************************************

#PCA1
df <- data.frame(pca_dims[,1], data$previous, data$poutcome ,data$nr.employed, data$month, data$euribor3m, data$euribor3m, data$emp.var.rate, data$cons.price.idx)
df <- df[order(-df[,1]),]
#low END - customers not contacted in previous campaigns, when interest rates are high, employment is high, the economy in general is strong, they were contacted by telephone, 

#PCA2
df <- data.frame(pca_dims[,2], data$previous, data$poutcome, data$pdays, data$cons.conf.idx)
df <- df[order(df[,1]),]
#LOW END

#PCA3
df <- data.frame(pca_dims[,3], data$month, data$job, data$cons.conf.idx, data$campaign, data$age)
df <- df[order(df[,1]),]
#LOW END

#PCA4
df <- data.frame(pca_dims[,4], data$previous, data$poutcome_nonexistent, data$pdays, data$month_jun, data$month_apr, data$contact_cellular, data$cons.price.idx, data$cons.conf.idx)
df <- df[order(-df[,1]),]
#LOW END

#PCA5
df <- data.frame(pca_dims[,5], data$job_technician, data$job_admin., data$education_university.degree, data$education_professional.course)
df <- df[order(df[,1]),]
#HIGH END

#PCA6
df <- data.frame(pca_dims[,6], data$previous, data$poutcome_nonexistent, data$poutcome_failure, data$nr.employed, data$month_may, data$month_jul, data$contact_cellular, data$cons.conf.idx)
df <- df[order(df[,1]),]
#LOW END

#PCA7
df <- data.frame(pca_dims[,7], data$job_services, data$job_retired, data$'job_blue-collar', data$education_university.degree, data$education_high.school, data$education_basic.9y)
df <- df[order(df[,1]),]
#LOW END

#PCA8
df <- data.frame(pca_dims[,8], data$marital_single, data$marital_married, data$job_management, data$job_blue-collar, data$education_basic.4y, data$default)
df <- df[order(df[,1]),]
#LOW END

#PCA9
df <- data.frame(pca_dims[,9], data$month_may, data$month_jun, data$job_retired, data$cons.conf.idx)
df <- df[order(df[,1]),]
#LOW END

#DETERMINE NUMBER OF CLUSTERS******************************************************************************************
wss <- (nrow(pca_dims)-1)*sum(apply(pca_dims,2,var))
for (i in 2:9) wss[i] <- sum(kmeans(pca_dims, centers=i)$withinss)
plot(1:9, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

#KMEANS****************************************************************************************************************
features <- data.frame(pca_dims)
clusters <- kmeans(features, 3, iter.max = 10000, nstart = 10) #3

qplot(x = pca_dims[,1], y=pca_dims[,2], data=data.frame(pca_dims), color=factor(clusters$cluster)) + geom_point() + theme(legend.position="none") + geom_point(x= clusters$centers[,1], y=clusters$centers[,2], data=data.frame(clusters$centers), color='purple', size=100, alpha=.2) + annotate("text", x = clusters$centers[1,1], y = clusters$centers[1,2], label="group 1", colour = "blue", size=7) + annotate("text", x = clusters$centers[2,1], y = clusters$centers[2,2], label="group 2", colour = "blue", size=7) + annotate("text", x = clusters$centers[3,1], y = clusters$centers[3,2], label="group 3", colour = "blue", size=7)

plot(clusters$centers[,1], clusters$centers[,2])

clusters$centers

clust1 <- rbind(data)
clust1$cluster <- clusters$cluster
sp <- split(clust1, clust1$cluster)
clust1 <- rbind(data.frame(sp$"1"))
mean(clust1$y) * 100

clust2 <- rbind(data.frame(sp$"2"))
mean(clust2$y) * 100

clust3 <- rbind(data.frame(sp$"3"))
mean(clust3$y) * 100

#DBSCAN****************************************************************************************************************
data_sample <- pca_dims[sample(1:nrow(pca_dims), 10000,replace=FALSE),] 
db <- dbscan(data_sample, 1.9,  MinPts = 3) #1.9 3
db
qplot(x = data_sample[,1], y=data_sample[,2], data=data.frame(data_sample), color=factor(db$cluster)) + geom_point()

density <- data.frame(data_sample)
density$clust <- db$cluster
sp <- split(data.frame(data_sample), density$clust)
clust1 <- rbind(data.frame(sp$"1"))
clust2 <- rbind(data.frame(sp$"2"))
means <- apply(clust1,2,mean)
means
means <- apply(clust2,2,mean)
means

#SOM****************************************************************************************************************
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

set.seed(1)
model <-som(som, grid = somgrid(20, 20, "hexagonal"), keep.data = TRUE)
plot(model, type="count")

h.clust <- hclust(dist(model$codes))
plot(h.clust,main='node dendrogram')

par(mfrow=c(1,1))
mypalette<-brewer.pal(10,"Paired")
som_cluster <- cutree(hclust(dist(model$codes)), 4)
plot(model, type="mapping", bgcol = mypalette[som_cluster], main='SOM Cluster') 

par(mfrow=c(2,2))
plot(model, type = "property", property = model$codes[,1], main='comp.1', palette.name=coolBlueHotRed)
add.cluster.boundaries(model, som_cluster)
plot(model, type = "property", property = model$codes[,2], main='comp.2', palette.name=coolBlueHotRed)
add.cluster.boundaries(model, som_cluster)
plot(model, type = "property", property = model$codes[,3], main='comp.3', palette.name=coolBlueHotRed)
add.cluster.boundaries(model, som_cluster)
par(mfrow=c(1,1))

#**************************************************************************************************************************************************
#**************************************************************************************************************************************************
#**************************************************************************************************************************************************

#FACTOMINER PCA***********************************************************************************************************
library(FactoMineR)
res.pca <- PCA(data[1:19], quali.sup = c(2:10, 14), ncp= 3, scale.unit=TRUE, graph=T)
summary(res.pca)
plot(res.pca$eig$eigenvalue, type='l')
pca_dims <- data.frame(res.pca$ind$coord)
som <- res.pca$ind$coord

qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$job)) + xlab('PCA1')+ylab('PCA52')
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$marital)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$education)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$default)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$housing)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$loan)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$contact)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$month)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$day_of_week)) + xlab('PCA1')+ylab('PCA2') 
qplot(x=pca_dims[,1], y=pca_dims[,2], data=pca_dims, color=factor(data$poutcome)) + xlab('PCA1')+ylab('PCA2') 

description <- dimdesc(res.pca, axes=c(1,2,3))
dim1 <- rbind(matrix(sqrt(description$Dim.1$quali[,1])), matrix(description$Dim.1$quanti[,1]))
row.names(dim1) <- c(dimnames(description$Dim.1$quali)[[1]], dimnames(description$Dim.1$quanti)[[1]])

dim2 <- rbind(matrix(sqrt(description$Dim.2$quali[,1])), matrix(description$Dim.2$quanti[,1]))
row.names(dim2) <- c(dimnames(description$Dim.2$quali)[[1]], dimnames(description$Dim.2$quanti)[[1]])

dim3 <- rbind(matrix(sqrt(description$Dim.3$quali[,1])), matrix(description$Dim.3$quanti[,1]))
row.names(dim3) <- c(dimnames(description$Dim.3$quali)[[1]], dimnames(description$Dim.3$quanti)[[1]])

description$Dim.1$category
description$Dim.2$category
description$Dim.2$category

#PCA LOADINGS***********************************************************************************************************

#PCA 1
pca1 = dim1
cutoff = .5
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 1", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA 2
pca1 = dim2
cutoff = .4
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 2", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA 3
pca1 = dim3
cutoff = .3
qplot(row.names(pca1), pca1, geom= "bar", stat= "identity", fill= pca1, xlab="Attributes", ylab="Loadings", main="Component 3", ylim=c(-1,1)) + scale_fill_continuous(low="#FF3300", high="#0099FF", limits=c(-1,1)) + coord_flip() + theme(axis.text.y = element_text(color =ifelse(abs(data.frame(pca1)[order(row.names(data.frame(pca1))),]) > cutoff, "#000000", "#CACACA"))) + theme(legend.position="none") 

#PCA ANALYSIS*********************************************************************************************************************************

#PCA1
df <- data.frame(pca_dims[,1], data$previous, data$poutcome,data$pdays, data$euribor3m, data$emp.var.rate)
df <- df[order(-df[,1]),]
#low END - customers not contacted in previous campaigns, when interest rates are high, employment is high, the economy in general is strong, they were contacted by telephone, 

#PCA2
df <- data.frame(pca_dims[,2], data$marital_single, data$marital_married, data$'job_blue-collar', data$job_admin., data$education_university.degree, data$education_basic.4y, data$default, data$age)
df <- df[order(-df[,1]),]
#LOW END

#PCA3
df <- data.frame(pca_dims[,3], data$month, data$marital, data$job, data$cons.price.idx, data$age)
df <- df[order(-df[,1]),]
#LOW END

#PCA4
df <- data.frame(pca_dims[,4], data$previous, data$poutcome_nonexistent, data$pdays, data$month_jun, data$month_apr, data$contact_cellular, data$cons.price.idx, data$cons.conf.idx)
df <- df[order(-df[,1]),]
#LOW END

#PCA5
df <- data.frame(pca_dims[,5], data$job_technician, data$job_admin., data$education_university.degree, data$education_professional.course)
df <- df[order(df[,1]),]
#HIGH END

#PCA6
df <- data.frame(pca_dims[,6], data$previous, data$poutcome_nonexistent, data$poutcome_failure, data$nr.employed, data$month_may, data$month_jul, data$contact_cellular, data$cons.conf.idx)
df <- df[order(df[,1]),]
#LOW END

#PCA7
df <- data.frame(pca_dims[,7], data$job_services, data$job_retired, data$'job_blue-collar', data$education_university.degree, data$education_high.school, data$education_basic.9y)
df <- df[order(df[,1]),]
#LOW END

#PCA8
df <- data.frame(pca_dims[,8], data$marital_single, data$marital_married, data$job_management, data$job_blue-collar, data$education_basic.4y, data$default)
df <- df[order(df[,1]),]
#LOW END

#PCA9
df <- data.frame(pca_dims[,9], data$month_may, data$month_jun, data$job_retired, data$cons.conf.idx)
df <- df[order(df[,1]),]
#LOW END

#DETERMINE NUMBER OF CLUSTERS******************************************************************************************
wss <- (nrow(pca_dims)-1)*sum(apply(pca_dims,2,var))
for (i in 2:9) wss[i] <- sum(kmeans(pca_dims, centers=i)$withinss)
plot(1:9, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

#KMEANS****************************************************************************************************************
features <- data.frame(pca_dims)
clusters <- kmeans(features, 3, iter.max = 10000, nstart = 10) #3

qplot(x = pca_dims[,1], y=pca_dims[,2], data=data.frame(pca_dims), color=factor(clusters$cluster)) + geom_point() + theme(legend.position="none") + geom_point(x= clusters$centers[,1], y=clusters$centers[,2], data=data.frame(clusters$centers), color='purple', size=100, alpha=.2) + annotate("text", x = clusters$centers[1,1], y = clusters$centers[1,2], label="group 1", colour = "blue", size=7) + annotate("text", x = clusters$centers[2,1], y = clusters$centers[2,2], label="group 2", colour = "blue", size=7) + annotate("text", x = clusters$centers[3,1], y = clusters$centers[3,2], label="group 3", colour = "blue", size=7)

plot(clusters$centers[,1], clusters$centers[,2])

clusters$centers

clust1 <- rbind(data)
clust1$cluster <- clusters$cluster
sp <- split(clust1, clust1$cluster)
clust1 <- rbind(data.frame(sp$"1"))
mean(clust1$y) * 100

clust2 <- rbind(data.frame(sp$"2"))
mean(clust2$y) * 100

clust3 <- rbind(data.frame(sp$"3"))
mean(clust3$y) * 100

#DBSCAN****************************************************************************************************************
data_sample <- pca_dims[sample(1:nrow(pca_dims), 10000,replace=FALSE),] 
db <- dbscan(data_sample, 1.9,  MinPts = 3) #1.9 3
db
qplot(x = data_sample[,1], y=data_sample[,2], data=data.frame(data_sample), color=factor(db$cluster)) + geom_point()

density <- data.frame(data_sample)
density$clust <- db$cluster
sp <- split(data.frame(data_sample), density$clust)
clust1 <- rbind(data.frame(sp$"1"))
clust2 <- rbind(data.frame(sp$"2"))
means <- apply(clust1,2,mean)
means
means <- apply(clust2,2,mean)
means

#SOM****************************************************************************************************************
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

set.seed(1)
model <-som(som, grid = somgrid(20, 20, "hexagonal"), keep.data = TRUE)
plot(model, type="count")

h.clust <- hclust(dist(model$codes))
plot(h.clust,main='node dendrogram')

par(mfrow=c(1,1))
mypalette<-brewer.pal(10,"Paired")
som_cluster <- cutree(hclust(dist(model$codes)), 4)
plot(model, type="mapping", bgcol = mypalette[som_cluster], main='SOM Cluster') 

par(mfrow=c(2,2))
plot(model, type = "property", property = model$codes[,1], main='comp.1', palette.name=coolBlueHotRed)
add.cluster.boundaries(model, som_cluster)
plot(model, type = "property", property = model$codes[,2], main='comp.2', palette.name=coolBlueHotRed)
add.cluster.boundaries(model, som_cluster)
plot(model, type = "property", property = model$codes[,3], main='comp.3', palette.name=coolBlueHotRed)
add.cluster.boundaries(model, som_cluster)
par(mfrow=c(1,1))


