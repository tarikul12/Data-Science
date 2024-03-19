var1 =readline(prompt = "Enter the value : ")
var2 =readline(prompt = "Enter any number : ")

var2 =as.integer(var2)
print(var1)
print(var2)

x=scan()
print(x)
mydata<-data.frame(age=numeric(0),gender=character(0),weight=numeric(0))
mydata<-edit(mydata)
print(mydata)

mydata<-read.csv("C:/Users/tarik/Desktop/10 semester/data science/data.csv",header = TRUE,sep = ",")
mydata

names(mydata)

mydata$gender<-factor(mydata$gender,levels = c(1,2),labels = c("male","female"))

str(mydata)

summary(mydata)

s<-mydata$year
sd(s)


