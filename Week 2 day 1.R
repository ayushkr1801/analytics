
#library install

pinstall <- c('cluster', 'factoextra', 'dendextend', 'fpc','NbClust', 'amap', 'arules', 'arulesViz' )
install.packages(pinstall)

set.seed(1234)
subject1=trunc(rnorm(30, mean = 60, sd = 15))
range(subject1)
subject1
marks = data.frame(subject1)
head(marks)
marks
sort(marks$subject1)
k2 = kmeans(marks, centers=2)
k2
k2$size
k2$iter
cbind(marks, k2$cluster) #which data row into which cluster
length(marks[k2$cluster==1,])
marks[k2$cluster==2,]
marks[k2$cluster==2,]

mean(marks[k2$cluster==1,])
mean(marks[k2$cluster==2,])
k2$centers

k2a = kmeans(marks, centers = c(50,70))
k2a
k2a$centers

#create marks of students
#perform clustering to group students with similllar marks


#-------testing----
scale(mtcars)

test=data.frame(mtcars)
k3=kmeans(test, centers = 3)
k3


k3$size
k3$iter
cbind(test, k3$cluster) #which data row into which cluster
length(test[k3$cluster==1,])
test[k3$cluster==2,]
test[k3$cluster==2,]

mean(test[k3$cluster==1,])
mean(test[k3$cluster==2,])
mean(test[k3$cluster==3,])

k3$centers


#iris clustering ---------

iris
dim(iris)
head(iris)
table(iris$Species)
data = iris[-5]
head(data)

km1 = kmeans(data, centers=1)
km1$withinss
km1$tot.withinss

km2 = kmeans(data, centers=2)
km2$withinss
km2$tot.withinss

km3 = kmeans(data, centers=3)
km3$withinss
km3$tot.withinss

km4 = kmeans(data, centers=4)
km4$withinss
km4$tot.withinss

library(NbClust)
nc= NbClust(data, distance='euclidean', min.nc = 2, max.nc = 15, method = "average")

library(cluster)
library(fpc)

data(iris)
data = iris[,-5] 
#without known classification
#kmeans cluster analysis

km1 = kmeans(data, centers = 3)
plotcluster(data, km1$cluster)

#plot 2
#more Complex
clusplot(data, km1$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 3)

?clusplot

pacman::p_load(gsheet, dplyr, bupaR, evendataR, edeaR, processmapR, processanimateR,  DiagrammeR, lubridate)

animate_process(patients)
head(patients,5) %>% as.data.frame()
patients

pacman::p_load(gsheet, lubridate, zoo, xts, quantmod, TTR, forecast)

install.packages(pinstall)

bdy='18-Jan-1992'
str(bdy)
bday1<- as.Date(bdy, format='%d-%b-%Y')
bday1
weekdays(bday1)

Sys.Date()- bday1

weekdays(Sys.Date())
internship = seq.Date(from=Sys.Date(), to = as.Date('2020-04-01'), by=1)
weekdays(internship)
range(internship)

internship

as.Date('2020-04-01') - Sys.Date()

start <- as.Date("2020-01-01")
end <- as.Date("2020-02-29")
end - start

quantmod :: getSymbols("SBIN.NS", src = "yahoo", from= start, to = end)
head(SBIN.NS)
SBIN.NS

df=SBIN.NS
df
head(df)
names(df)
colnames(df)
#create new column names
unlist(strsplit("a.b.c","\\."))
unlist(strsplit(names(df),"\\."))
(newColNames <- unlist(strsplit(names(df), "\\."))[seq(3,18,3)])
names(df) =newColNames
#names(df) = c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted' )
head(df)
str(df)
index(df) #rownames
coredata(df) #column values

#time series
plot(df$Open)
plot(df, legend.loc = 'left')
plot(df, legend.loc = 'left', multi.panel = T)
plot(df[,c('Open', 'Close')], legend.loc = 'top')
plot(df[,c('Open', 'Close')], legend.loc = 'top', multi.panel = T)
plot(df[,c('Open', 'Close')], legend.loc = 'right', subset = "2020-01-01/2020-01-15")

plot(df[1:4], multi.panel  = T)
plot(df[1:4], multi.panel  = T, type = 'h')
plot(df[1:4], multi.panel  = F, legend.loc = 'top')
candleChart(df, up.col="green", dn.col="red", theme = "white")

#properties

periodicity(df)
to.weekly(df)
to.monthly(df)
to.quarterly(df)
to.yearly(df)
to.period(df,period ="weeks")
nyears(df)
nmonths(df)
nweeks(df)
ndays(df)
.indexwday(df) #weekday number
weekday(index(df))
start(df)
end(df)
time(df)
head(df)
tail(df)

#apply functions
apply.weekly(df, FUN=mean)
apply.monthly(df, FUN=mean)
apply.monthly(df$Open, FUN=sd)
apply.quaterly(df, FUN=mean)

#end points
endpoints(df, on='weeks')
endpoints(df, on='months')
df[endpoints(df, on='months'),]
df

df[.indexwday(df)==5]

#means
(wep <- endpoints(df, on='weeks'))
period.apply(df,INDEX=wep, FUN=mean)

#split data
(sdata <- split(df,f='months'))
sdata[[1]]
sdata[[2]]

(sdata <- split(df,f='weeks'))
sdata[[3]]
sdata[[2]]
sdata

#subset

df['2020']
df['2020-01']
df['2020-01-16']

#first & last

first(df, '2 week') # extract first 1 week
first(last(df, '1 week'), '3 days') #get first 3 days of the last week of data

df[c('2020-01-16', '2020-02-17')]
#weekend
.indexwday(df)
df[.indexwday(df)==5]

df[.indexwday(df)%%2!=0]
df[seq(as.Date('2020-01-01'),as.Date('2020-01-31'),2),]

#TS Data

AirPassengers
monthplot(AirPassengers)
class(AirPassengers)
(inputdata= as.vector(AirPassengers))
length(inputdata, frequency= 12, start= 1990)






#Monthly TS
(inputData= as.vector(AirPassengers))
(monTS <- ts (inputData, frequency = 12, start = c(2010,3)))

#monthly data

(monTSlagged <- stats::lag(monTS, k=-1))
monTS
monTSlagged
(monTS -monTSlagged)
diff(monTS, lag=1)
cbind(monTS, monTSlagged, difference=(monTS - monTSlagged), diff(monTS))
head(monTS)
head(diff(monTS,2))
c((132-112),(129-118), (121-132))


#Rolling Mean----
monTS
zoo::rollmean(monTS, k=1)
zoo::rollmean(monTS, k=2)
head(monTS)
c((112+118)/2,(118+132)/2)
zoo::rollmean(monTS, k=2, align='right') #mean should start from Feb




#Moving Average, SMA, ES-----------
library(TTR)
head(monTS)
SMA(monTS, n=3)
EMA(monTS, n=3)

plot(monTS)
lines(SMA(monTS, n=12), col='red')

#exponential

AirPassengers
plot(AirPassengers)
plot(decompose(AirPassengers))

#forecast & arima

library(forecast)
auto.arima(monTS)

autoplot(monTS)

auto.arima(monTS,seasonal = T)

#model
arimaModel = auto.arima(monTS)

#forecast
fcModel <- forecast(arimaModel)
fcModel$mean # 10 elements ahead
autoplot(fcModel)


#lag
df.Open = df[,'Open']
df.Open
str(df.Open)


