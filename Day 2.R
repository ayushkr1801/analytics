mtcars
summary(mtcars)
summary(mtcars$cyl)
library(dplyr)
mtcars %>% group_by(cyl) %>% tally()
mtcars %>% group_by(cyl) %>%  summarise(count=n())

xtabs(~ cyl, data = mtcars)
ftable(mtcars$cyl)

table(mtcars$cyl, mtcars$gear)

table(mtcars$cyl, mtcars$gear, dnn=c('cylinder', 'gear'))
mtcars %>% group_by(cyl, gear) %>% tally()
mtcars %>% group_by(cyl, gear) %>% summarise(count=n())

xtabs( ~cyl + gear, data=mtcars)
ftable(mtcars$cyl, mtcars$gear)
table (mtcars$cyl, mtcars$gear, mtcars$am, dnn=c('cyl', 'Gear', 'Automannual'))

df=mtcars
head(df)
df$amNew= ifelse(df$am==0, 'Auto' , 'Manual')

mtcars %>% mutate(TxType = ifelse(df$am==0, 'Auto' , 'Manual')) %>% group_by(TxType) %>% summarise(count =n())
df$mpg
df[,'mpg']
df

head(df)

df=df %>% mutate(TxType = ifelse(am==0, 'Auto', 'Manual'))
head(df)

df1= mtcars
df1 = df1 %>% mutate(con= c( wt))
df1

#increase mileage by 10%

df$mpg *1.1

#add mpg + wr into new column MGPWT
df$mpg + df$wt
df$MPGWT=df$mpg * 1.1 + df$wt
df

head(df)

mtcars %>% group_by(mpg, gear)

df %>% group_by(gear) %>% top_n(n=2, wt =mpg)
df %>% group_by(gear) %>% arrange (-mpg) %>% select (gear, mpg)


df %>% group_by(gear) %>% top_n(n=2, wt =mpg)
#list out details of any 2 cars picked randomly: then do 25% of the cars

df %>% sample_n(2)
df %>% sample_frac(.25)

df %>% sample_frac(.25) %>% arrange(mpg)

df %>% sample_frac(.25) %>% top_n(n=2, wt =mpg)
df[order(df$mpg),]

#find mean of eacg gear power and weigth

df %>% select(gear, mpg, wt,hp,disp) %>% group_by(gear) %>% summarise_all(mean)

df %>% select(gear, mpg, wt) %>% group_by(gear) %>% summarise_each(min,max)

#graphs
hist(df$mpg)
barplot(table(df$gear), df$gear)
barplot(table(df$gear), col=1:3)
pie(table(df$gear))
plot(df$wt, df$mpg)

library(ggplot2)
install.packages("reshape")
library(reshape2)


df=mtcars
head(df)
df
(rollno=paste('IIM', 1:10, sep='_'))
(name = paste('SName', 1:10, sep=' '))
(gender= sample(c('M','F'), size = 10, replace=T))
(program= sample(c('BBA', 'MBA'), size = 10, replace = T))
(marketing = trunc(rnorm(10, mean=60, sd=10)))
(operations = trunc(rnorm(10, mean=70, sd=5)))
(finance= trunc(rnorm(10, mean = 55, sd=12)))
students <-data.frame(rollno, name, program, gender, marketing, finance, operations, stringsAsFactors = F)
students
head(students)

(meltsum1 <- melt(students, id.vars = c('rollno', 'name', 'gender', 'program'), variable.name = 'subject', value.name='marks'))
meltsum1
head(meltsum1)

#ggplot-----

students
students %>% group_by(gender) %>% summarise(count=n())
ggplot(students %>% group_by(gender) %>% summarise(count=n()), aes(x=gender, y=count, fill=gender))+ geom_bar(stat='identity')

ggplot(students %>% group_by(gender) %>% summarise(count=n()), aes(x=gender, y=count, fill=gender))+ geom_bar(stat='identity') +geom_text(aes(label=count))+ labs(title = 'Gender wise count')
#stacked Bar
ggplot(students %>% group_by(program) %>% summarise(count=n()), aes(x=program, y=count, fill=program))+ geom_bar(stat='identity') +geom_text(aes(label=count))+ labs(title = 'Program wise count')

#side by side
ggplot(students %>% group_by(program,gender) %>% summarise(count=n()), aes(x=gender, y=count, fill=program))+ geom_bar(stat='identity',position = position_dodge2(.7)) +geom_text(aes(label=count), position = position_dodge2(.7))+ labs(title = 'Gender wise count')

#subjects - Program - Gender - Mean marks

names(students)
names(meltsum1)
head(meltsum1)

ggplot(meltsum1 %>% group_by(program, gender, subject) %>% summarise(meanMarks= round(mean(marks))), aes(x=gender, y=meanMarks, fill=program)) + geom_bar(stat = 'identity', position = position_dodge2(.7)) + labs(title = 'subject - Program - Gender - Mean Marks') + facet_grid(~subject)
  
  students %>% group_by(program,gender) %>% summarise(count=n()), aes(x=gender, y=count, fill=program))+ geom_bar(stat='identity',position = position_dodge2(.7)) +geom_text(aes(label=count), position = position_dodge2(.7))+ labs(title = 'Gender wise count')



ggplot(students, aes(x=gender, y=..count..))+geom_bar(stat = 'count')


meltsum1 %>% group_by(program, gender) %>% summarise(MeanMarks= mean(marks))


(meltsum1 <- melt(students, id.vars = c('rollno', 'name')))
(dcastsum1 <- dcast(meltsum1, rollno + name ~ varibale, value.var = 'Value'))
?recast
recast(data=students, gender ~ variable, fun.aggreagtor= mean


(sum1 <- df%>% group_by(gear, cyl) %>% summarise(MINMPG=min(mpg), MAXMPG=max(mpg)))

  