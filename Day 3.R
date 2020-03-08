
#till now

#Data structures
#Manipulate
#Summarise
#Graph
#Import from sheet, google sheet

#Day 3: 23 Feb

#missing Values
(x=c(1,2,3,4))
(x2=c(1,2,3,NA,4,5,6))
(x3=c(1,2,NA, 4, 5, NA))
sum(x3, na.rm = T) # set na to remove as True

is.na(x3)
sum(is.na(x3))
(sum(is.na(x3))/length(x3))*100

x3
mean(x3, na.rm = T)
x3[is.na(x3)]
x3[is.na(x3)]=mean(x3, na.rm = T)
x3



library(VIM)
install.packages("VIM")



data(sleep)
sleep; ?sleep
head(sleep)
tail(sleep)
str(sleep)
summary(sleep)

quantile(sleep$BrainWgt)

(x=200:300)
quantile(x)
quantile(x, seq(0,1,.25))
quantile(x, seq(0,1,.1))
quantile(x, seq(0,1,.01))

head(sleep)
is.na(sleep)
sum(is.na(sleep))
colSums(is.na(sleep))
rowSums(is.na(sleep))
sum(rowSums(is.na(sleep)))

#which row is completely filled

complete.cases(sleep)
sum(complete.cases(sleep))
sleep[complete.cases(sleep), ]
sleep[!complete.cases(sleep), ]

library(dplyr)

(xy = colSums(is.na(sleep)))
xy[xy >0]
(c1 <- names(xy[xy >0]))
sleep[, c1]
sleep %>% select(c1) %>% length()

#data partitioning
(x= trunc(rnorm(100, mean = 60, sd = 15)))
set.seed(134)
s1 <- sample(x, size=70)
length(s1)
sum(s1)

s2 <- sample(x, size=.7, length(x))
length(s2)
x


mtcars
mtcars %>% sample_n(24)
mtcars %>% sample_frac(.7)
dim(mtcars); nrow(mtcars)
(index = sample(1:nrow(mtcars), size=.7 * nrow(mtcars)))
mtcars[index, ]
dim(mtcars[index, ])
mtcars[-index, ]  
  

#mine----------------------------
pinstall <- c('rpart','rpart.plot', 'caTools', 'caret', 'arules', 'arulesViz')
version
install.packages(pinstall)

library(caTools)

p1install <- c('arulesViz', 'factoextra', 'dendextend', 'NbClust', 'cluster', 'fpc', 'amap', 'animation')
install.packages(p1install)
install.packages('gsheet')

p2install <- c('readx1', 'rJava', 'xlsx', 'wordcloud', 'wordcloud2', 'modeest', 'fdth', 'e1071')
install.packages(p2install)

tspackages <- c('timeSeries', 'xts', 'zoo', 'forecast' , 'TTR')
tmpackages <- c('rtweet', 'curl', 'twitterR', 'ROAuth', 'syuzhet')
lppackages <- c('lpsolve', 'linprog', 'lpSolveAPI')

install.packages(tspackages)
install.packages(tmpackages)
install.packages(lppackages)

#Packages by sir-----------------
clusterpackages <- c('factoextra', 'dendextend','NbClust', 'cluster','fpc', 'amap','animation')
install.packages(clusterpackages)
arulepackages <- c('arules','arulesViz')
install.packages(arulepackages)
classpackages <- c('rpart','rpart.plot')
install.packages(classpackages)
iepackages <- c('gsheet', 'readxl', 'rJava', 'xlsx')
install.packages(iepackages)
statspackages <- c('modeest','fdth','e1071','caTools', 'caret' )
install.packages(statspackages)
tspackages <- c('timeSeries','xts','zoo','forecast','TTR','quantmod', 'lubridate','smooth','Mcomp')
install.packages(tspackages)
tmpackages <- c('rtweet',"curl", 'twitterR', 'ROAuth', 'syuzhet','wordcloud', 'wordcloud2')
install.packages(tmpackages)
lppackages <- c('lpSolve', 'linprog', 'lpSolveAPI')
install.packages(lppackages)


install.packages(pinstall)  #replace pinstall with name of vector of package list

#Multiple Install
list.of.packages <- plist #substitute plist with name of list of packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load if avl, install if not avl---
#Load if available, install packages if not available in the system & then load
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}
#---------------------------------------------


library(caTools)
sample=sample.split(Y=mtcars$am, SplitRatio = .7)
sample
table(sample)
prop.table(table(sample))
mtcars[sample==T,] # True Set
mtcars[sample==F,] # True Set

y1=mtcars[sample==T,] # True Set
y2=mtcars[sample==F,] # True Set
prop.table(table(y1$am))
prop.table(table(y2$am))

prop.table(table(mtcars$am))


library(caret)

(intrain <- createDataPartition(y=mtcars$am, p=.7, list = F))
train<-mtcars[intrain,]
test <- mtcars[- intrain,]
prop.table(table(train$am)); prop.table(table(test$am))



#Linear Regression

women
head(women)
model =lm(weight ~ height, data=women)
summary(model)

#y = mx +c 
#weigth = 3.45 * height +- 87.51

plot(x=women$height, y=women$weight)
abline(model)
residuals(model)
women$weight

predwt <- predict(model, newdata=women, type='response')
head(women)
3.45 * 58-87
cbind(women, predwt, res = women$weight - predwt, res2 =residuals(model))

summary(model)

sqrt(sum(residuals(model)^2))  #SSE

range(women$height)

ndata= data.frame(height=66.5)
predict(model, newdata=ndata, type= 'response')

ndata= data.frame(height=c(66.5, 75.8))
predict(model, newdata=ndata, type= 'response')


#Case2 : Linear Regression-------

link ="https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=2023826519"

library(gsheet)
df=as.data.frame(gsheet2tbl(link))
head(df)

(model1 =lm(Y ~ X, data=df))
preddf <- predict(model1, newdata=df, type='response')
preddf

plot(x=df$X, y=df$Y)
abline(model1)
residuals(model1)

cbind(df, preddf, res = df$Y - preddf, res2 =residuals(model1))
summary(model1)
sqrt(sum(residuals(model1)^2))  #SSE

ndata1= data.frame(X=c(3, 4))
predict(model1, newdata=ndata1, type= 'response')

#Assumptions of linear regression model
plot(model)

#New Example : Sales Quantity

Link2= 'https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=1595306231'


df2=as.data.frame(gsheet2tbl(Link2))
df2

head(df2)
model2 <- lm(sqty ~ price + promotion, data=df2)
model2
plot(model2)

range(df2$price)
range(df2$promotion)

plot(df2$price, df2$sqty)
abline(model2, col=2)

plot(df2$promotion, df2$sqty)
abline(model2, col=2)

range(df2$price) ; range(df2$promotion)
(ndata3= data.frame(price=c(60,75), promotion=c(300,500)))
predict(model2, newdata = ndata3, type = 'response')


#import data from a online site

# Decision Tree - Classification--------
#we want predict for combination of input variables, is a person likely to survive or not

#import data from online site
path = 'https://raw.githubusercontent.com/DUanalytics/datasets/master/csv/titanic_train.csv'
titanic <- read.csv(path)
head(titanic)
names(titanic)
data = titanic[,c(2,3,5,6,7)]  #select few columns only
head(data)
dim(data)
#load libraries
library(rpart)
library(rpart.plot)
str(data)
#Decision Tree
names(data)
table(data$Survived)
prop.table(table(data$Survived))

str(data)
data$Pclass = factor(data$Pclass)
fit <- rpart(Survived ~ ., data = data, method = 'class')
fit
rpart.plot(fit, extra = 104, cex=.8,nn=T)  #plot
head(data)
printcp(fit) #select complexity parameter
prunetree2 = prune(fit, cp=.017544)
rpart.plot(prunetree2, cex=.8,nn=T, extra=104)
prunetree2
nrow(data)
table(data$Survived)
# predict for Female, pclass=3, siblings=2, what is the chance of survival

library(dplyr)
#Predict class category or probabilities
(testdata = sample_n(data,2))
predict(prunetree2, newdata=testdata, type='class')
predict(prunetree2, newdata=testdata, type='prob')
str(data)
testdata2 = data.frame(Pclass=factor(2), Sex=factor('male'), Age=15, SibSp=2)
testdata2
predict(prunetree2, newdata = testdata2, type='class')
predict(prunetree2, newdata = testdata2, type='prob')

?rpart.plot

#Use decision trees for predicting
#customer is likely to buy a product or not with probabilities
#customer is likely to default on payment or not with probabilities
#Student is likely to get selected, cricket team likely to win etc

#Imp steps
#select columns for prediction
#load libraries, create model y ~ x1 + x2 
#prune the tree with cp value
#plot the graph
#predict for new cases

#rpart, CART, classification model
#regression decision = predict numerical value eg sales




#Clustering ----------

(marks1 = trunc(rnorm(n=30, mean = 70, sd = 8)))
sum(marks1)
df5 <- data.frame(marks1=marks1)
head(df5)
#

km3 <- kmeans(df5, centers=3)
attributes(km3)
km3$cluster
km3$centers
km3$size
sort(df5$marks)
cbind(df5, km3$cluster) #Which row which cluster
df5$cluster=km3$cluster
head(df5)
df5 %>% arrange(cluster)
dist(df5[1:5,])


set.seed(1234)
(marks1= trunc(rnorm(n=30, mean=70, sd=8)))
(marks2= trunc(rnorm(n=30, mean=120, sd=10)))
df6 <- data.frame(marks1, marks2)
head(df6)
df68 <-scale(df6)
df68
#

km38<- kmeans(df6, centers=5)
attributes(km38)
km38$cluster
km38$centers
km38$size
sort(df5$marks)
cbind(df6, km38$cluster) #Which row which cluster
df6$cluster=km38$cluster
head(df6)
df6 %>% arrange(cluster)
df6[1:5,]
dist(df6[1:5,])
plot(df6$marks1, df6$marks2, col= df6$cluster)

#scale data and then do clustering

km38<- kmeans(df68, centers=5)
attributes(km38)
km38$cluster
km38$centers
km38$size
sort(df5$marks)
cbind(df68, km38$cluster) #Which row which cluster
df68$cluster=km38$cluster
head(df68)
df68 %>% arrange(cluster)
df6[1:5,]
dist(df6[1:5,])
plot(df6$marks1, df6$marks2, col= df6$cluster)
