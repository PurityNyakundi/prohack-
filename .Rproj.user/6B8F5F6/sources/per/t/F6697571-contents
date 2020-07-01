#Lets start
library(tidyverse)
#load the data
train<-read.csv("train.csv",stringsAsFactors = F)
test<-read.csv("test.csv",stringsAsFactors = F)
head(train)
sample<-read.csv("sample_submit.csv",stringsAsFactors = F)
sample

colSums(is.na(train))

library(mice)
install.packages("HotDeckImputation")
library(remotes)
install_version("HotDeckImputation", "3.6.3")
install_github("cran/HotDeckImputation")

install.packages("installr")
hot.deck::
impute.mean(train)
impute.mean(DATA=train)
for (i in which(sapply(train, is.numeric))) {
  train[is.na(train[, i]), i] <- mean(train[, i],  na.rm = TRUE)
}
train=train[,which(colMeans(!is.na(train))>0.7)]
head(train)
dim(train)
colSums(is.na(train))
p<-train[,-c(12,1)]
head(p)
#df = transform(p, y = ifelse(is.na(p), mean(p, na.rm=TRUE), y))

#train_p<-preProcess(p,method = "knnImpute",k=10)
#t<-predict(train_p,newdata = p)
#head(t)
y=train$y
t<-cbind(p,y)
dim(t)
t = as.data.frame(t)
glimpse(t)
library(tidyverse)
library(caret)
t<-select(t,-1)
head(t)

set.seed(107)
inTrain <- createDataPartition(
  y = t$y,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)
## The format of the results

## The output is a set of integers for the rows of Sonar
## that belong in the training set.
str(inTrain)
#>  int [1:157, 1] 1 2 3 4 5 7 10 11 12 13 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr "Resample1"

  
  
  training <- t[ inTrain,]
testing  <- t[-inTrain,]
train$existence.expectancy.index

glimpse(t)
library(caret)
library(tidyverse)
test%>%
  filter(existence.expectancy.index<0.7)%>%
  select(existence.expectancy.index)%>%
  count()
trainControl <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(y~., data=training, method="gbm", metric="RMSE", trControl=trainControl)
# display results
print(fit)
#summary(fit,cBars=10,method = relative.influence,las =2)
testing$prt<-predict(fit,testing)

head(prt)
testing%>%
  ggplot(aes(x = prt,y = y))+
  geom_point(size=0.5,pch = 21)+
  theme_classic()+geom_smooth(method = "lm",se=F)

r<-list(l=prt,u = testing$y)
dim(train)
