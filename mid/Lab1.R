                         #string
str<- "Hello World"
nchar(str)

str<- "Hello World"
str1<- "Hi,"
paste(str1,str)

str2<-"Hello nayem,I am a junior \"programmer\", please 
read my cv ,then repaily
your ans I will wait your decsion!"

cat(str2)

                     # conditional

a<-80
b<-50
if(a>b){
  print("a is greater than b")
}else{
  print(" a is not greater than b ")
}

switch(2,"RED","GREEN","BLUE")

x="name"
strsplit(x,split = "")
tolower(x)

toupper(x)

dataset <- read.csv("C:/Users/tarik/Desktop/10 semester/Data_Science_Lab/Dataset_midterm_Section(C) (1).csv", na.strings = c(""))
dataset

# Filling missing values with mode for categorical variables
filled_data_mode <- dataset

filled_data_mode$Age <- ifelse(is.na(filled_data_mode$Age), 
                               names(sort(table(filled_data_mode$Age), decreasing = TRUE))[1], 
                               filled_data_mode$Age)

filled_data_mode$SystolicBP <- ifelse(is.na(filled_data_mode$SystolicBP), 
                                      names(sort(table(filled_data_mode$SystolicBP), decreasing = TRUE))[1], 
                                      filled_data_mode$SystolicBP)

filled_data_mode$DiastolicBP <- ifelse(is.na(filled_data_mode$DiastolicBP), 
                                       names(sort(table(filled_data_mode$DiastolicBP), decreasing = TRUE))[1], 
                                       filled_data_mode$DiastolicBP)

filled_data_mode$BS <- ifelse(is.na(filled_data_mode$BS), 
                              names(sort(table(filled_data_mode$BS), decreasing = TRUE))[1], 
                              filled_data_mode$BS)

filled_data_mode$BodyTemp <- ifelse(is.na(filled_data_mode$BodyTemp), 
                                    names(sort(table(filled_data_mode$BodyTemp), decreasing = TRUE))[1], 
                                    filled_data_mode$BodyTemp)

filled_data_mode$HeartRate <- ifelse(is.na(filled_data_mode$HeartRate), 
                                     names(sort(table(filled_data_mode$HeartRate), decreasing = TRUE))[1], 
                                     filled_data_mode$HeartRate)

filled_data_mode$RiskLevel <- ifelse(is.na(filled_data_mode$RiskLevel), 
                                     names(sort(table(filled_data_mode$RiskLevel), decreasing = TRUE))[1], 
                                     filled_data_mode$RiskLevel)

print(filled_data_mode)
