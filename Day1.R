#IIMLN
#day1

mtcars
?mtcars   #help on mtcars
class(mtcars)   #data frame/structure
x=1:5
class(x)
x
y= 1.5:3.6
class(y)
str(mtcars) #structure
?str
summary(mtcars)
y
as.integer(y)
y
z=as.integer(y)
z




#vectors ----

x=1:100

x

a="ayush"
a=c(a,"singh")
a
a[2]

x4= rnorm(n=100,mean = 60,sd=10)

x4
plot(a)

plot(density(x4))

plot(women)

x4= rnorm(n=1000,mean = 60,sd=10)

plot(density(x4))

hist(x4)

hist(x4, breaks=10, col=51:100)
range(x4)
mean(x4)
median(x4)
boxplot(x4)

sum(x4>66)

sum(x4)
x4[x4>66 & x4<70]
length(x4)

#matrics----

(data=c(10,20,40,6,5,53,4))

(m1=matrix(data=data,nrow=2))

(m1=matrix(data=data,nrow=2, byrow=T))

?matrix

rownames(m1)=c('R1','R2')

colnames(m1)=c('C1','C2','C3','C4')

m1[2,3:4]

rowSums(m1)
colSums(m1)
?apply

colMeans(m1)

apply(m1, 2, FUN=max.col())

max.col(m1)
?max.col

?max

m1


#dataframe----

roll=c(1:13)
roll=c(roll,'IIML')

(rollno=paste('IIMLN',1:13, sep='-'))
(name=paste('Student',1:13, sep=' & '))
(age=round(runif(13,min = 24,max = 32),2))
(marks=trunc(rnorm(13, mean = 60,sd = 10)))
(gender= sample(c('M','F'),size = 13, replace=T))


set.seed(34)
(gender= sample(c('M','F'),size = 13, replace=T, prob = c(.7,.3)))
table(gender)

?runif


floor(x)
ceiling(x)
trunc(x)

table(gender)

prop.table(table(gender))


(grade=sample(c('ex','Good','Sat'), size = 13, replace=T, prob = c(.6,.3,.1)))
table(grade)

sapply(list(rollno, name, age, marks, gender, grade), length)
student=data.frame(rollno, name, age, marks, gender, grade)
student
write.csv(student, 'data/student.csv', row.names = F)

df1=read.csv('data/student.csv')
df1

df2=read.csv('https://raw.githubusercontent.com/DUanalytics/rAnalytics/master/data/student.csv')
df2

df3=read.csv(file.choose())
df3

student
summary(student)

install.packages("dplyr")

library(dplyr) #loading the library

class(student)

summary(student)
str(student)

student$gender= factor(student$gender)
student


student %>% group_by(gender) %>% tally()
student %>% group_by(gender) %>% summarise(mean(age), n())
student %>% group_by(gender) %>% summarise(mean(age), n(), min(marks), max(marks))

mtcars
