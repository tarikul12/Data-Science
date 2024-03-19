install.packages("dplyr")
library(dplyr)

dataset <- read.csv("C:/Users/tarik/Desktop/10 semester/Data_Science_Lab/Dataset_midterm_Section(C) (1).csv", na.strings = c(""))
dataset

names(dataset)

str(dataset)

summary(dataset)

dataset %>% summarise_if(is.numeric, sd)

dataset$Infection<-factor(dataset$Infection,levels = c("yes","no","marginal"),labels = c(1,2,3))
dataset

dataset$RiskLevel<-factor(dataset$RiskLevel,levels = c("high risk","low risk","mid risk"),labels = c(1,2,3))
dataset



colSums(is.na(dataset))

which(is.na(dataset$Age))
which(is.na(dataset$Infection))
which(is.na(dataset$Smoking))
which(is.na(dataset$SystolicBP))
which(is.na(dataset$DiastolicBP ))
which(is.na(dataset$BS))
which(is.na(dataset$BodyTemp))
which(is.na(dataset$HeartRate))
which(is.na(dataset$RiskLevel))


dataset <- na.omit(dataset)
mydata<-dataset
mydata

filled_data_mean <- dataset
filled_data_mean$Age <- ifelse(is.na(filled_data_mean$Age), mean(filled_data_mean$Age, na.rm = TRUE), filled_data_mean$Age)
filled_data_mean$Infection<- ifelse(is.na(filled_data_mean$Infection), mean(filled_data_mean$Infection, na.rm = TRUE), filled_data_mean$Infection)
filled_data_mean$Smoking<- ifelse(is.na(filled_data_mean$Smoking), mean(filled_data_mean$Smoking, na.rm = TRUE), filled_data_mean$Smoking)
filled_data_mean$DiastolicBP<- ifelse(is.na(filled_data_mean$DiastolicBP), mean(filled_data_mean$DiastolicBP, na.rm = TRUE), filled_data_mean$DiastolicBP)
print(filled_data_mean)

filled_data_median <- dataset
filled_data_median$Age <- ifelse(is.na(filled_data_median$Age), median(filled_data_median$Age, na.rm = TRUE), filled_data_median$Age)
filled_data_median$Smoking <- ifelse(is.na(filled_data_median$Smoking), median(filled_data_median$Smoking, na.rm = TRUE), filled_data_median$Smoking)
filled_data_median$SystolicBP <- ifelse(is.na(filled_data_median$SystolicBP), median(filled_data_median$SystolicBP, na.rm = TRUE), filled_data_median$SystolicBP)
filled_data_median$DiastolicBP <- ifelse(is.na(filled_data_median$DiastolicBP), median(filled_data_median$DiastolicBP, na.rm = TRUE), filled_data_median$DiastolicBP)
filled_data_median$BS <- ifelse(is.na(filled_data_median$BS), median(filled_data_median$BS, na.rm = TRUE), filled_data_median$BS)
filled_data_median$BodyTemp <- ifelse(is.na(filled_data_median$BodyTemp), median(filled_data_median$BodyTemp, na.rm = TRUE), filled_data_median$BodyTemp)
filled_data_median$HeartRate <- ifelse(is.na(filled_data_median$HeartRate), median(filled_data_median$HeartRate, na.rm = TRUE), filled_data_median$HeartRate)
filled_data_median$RiskLevel <- ifelse(is.na(filled_data_median$RiskLevel), median(filled_data_median$RiskLevel, na.rm = TRUE), filled_data_median$RiskLevel)
print(filled_data_median)


filled_data_mode <- dataset

filled_data_mode$Age <- ifelse(is.na(filled_data_mode$Age), 
                               names(sort(table(filled_data_mode$Age), decreasing = TRUE))[1], 
                               filled_data_mode$Age)
filled_data_mode$Infection  <- ifelse(is.na(filled_data_mode$Infection ), 
                               names(sort(table(filled_data_mode$Infection ), decreasing = TRUE))[1], 
                               filled_data_mode$Infection )
filled_data_mode$Smoking   <- ifelse(is.na(filled_data_mode$Smoking  ), 
                                      names(sort(table(filled_data_mode$Smoking  ), decreasing = TRUE))[1], 
                                      filled_data_mode$Smoking  )

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



missing_data <- data.frame(
  Missing = c("Available", "Missing"),
  Age = c(sum(!is.na(dataset$Age)), sum(is.na(dataset$Age)))
)

ggplot(missing_data, aes(x = Missing, y = Age, fill = Missing)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#619CFF", "#F8766D")) +
  labs(
    title = "Missing Instances of Age",
    x = "Data Availability",
    y = "Age"
  ) +
  theme_minimal()                                                                           gggvgv


missing_data <- data.frame(
  Missing = c("Available", "Missing"),
  DiastolicBP = c(sum(!is.na(dataset$Smoking)), sum(is.na(dataset$Smoking)))
)


ggplot(missing_data, aes(x = Missing, y = Smoking, fill = Missing)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "yellow")) +
  labs(
    title = "Missing Instances of Smoking",
    x = "Data Availability",
    y = "Smoking
"
  ) +
  theme_minimal()


install.packages("ggplot2")
install.packages("tidyr")
library(ggplot2)
library(tidyr)


missing_values <- colSums(is.na(dataset[c("Age", "Infection", "Smoking", "DiastolicBP")]))


missing_data <- data.frame(attribute = names(missing_values), count = missing_values)

 
ggplot(missing_data, aes(x = attribute, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Missing Values for Attributes",
       x = "Attribute", y = "Count of Missing Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(missing_data)


mean_age <- mean(mydata$Age, na.rm = TRUE)
mean_smoking <- mean(mydata$Smoking, na.rm = TRUE)
mean_systolicBP <- mean(mydata$SystolicBP, na.rm = TRUE)
mean_diastolicBP <- mean(mydata$DiastolicBP, na.rm = TRUE)
mean_bs <- mean(mydata$BS, na.rm = TRUE)
mean_bodytemp <- mean(mydata$BodyTemp, na.rm = TRUE)
mean_heartrate <- mean(mydata$HeartRate, na.rm = TRUE)
mean_age
mean_smoking
mean_systolicBP
mean_diastolicBP
mean_bs
mean_bodytemp
mean_heartrate
mean_data <- data.frame(attribute = c("Age", "Smoking", "SystolicBP", "DiastolicBP", "BS", "BodyTemp", "HeartRate"),
  mean_value = c(mean_age, mean_smoking, mean_systolicBP, mean_diastolicBP, mean_bs, mean_bodytemp, mean_heartrate))


ggplot(mean_data, aes(x = attribute, y = mean_value)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Mean Values of Attributes",
       x = "Attribute", y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


median_age <- median(mydata$Age, na.rm = TRUE)
median_smoking <- median(mydata$Smoking, na.rm = TRUE)
median_systolicBP <- median(mydata$SystolicBP, na.rm = TRUE)
median_diastolicBP <- median(mydata$DiastolicBP, na.rm = TRUE)
median_bs <- median(mydata$BS, na.rm = TRUE)
median_bodytemp <- median(mydata$BodyTemp, na.rm = TRUE)
median_heartrate <- median(mydata$HeartRate, na.rm = TRUE)
median_age
median_smoking
median_systolicBP
median_diastolicBP
median_bs
median_bodytemp
median_heartrate

mean_data$median_value <- c(median_age, median_smoking, median_systolicBP, median_diastolicBP, 
                            median_bs, median_bodytemp, median_heartrate)


ggplot(mean_data, aes(x = attribute)) +
  geom_bar(aes(y = median_value), stat = "identity", fill = "red") +
  labs(title = "Median Values of Attributes",
       x = "Attribute", y = "Median Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




calculate_mode <- function(x) {
  unique_x <- unique(x)
  freq <- tabulate(match(x, unique_x))
  mode_value <- unique_x[which.max(freq)]
  return(mode_value)
}


mode_age <- calculate_mode(mydata$Age)
mode_smoking <-calculate_mode(mydata$Smoking)
mode_systolicBP <- calculate_mode(mydata$SystolicBP)
mode_diastolicBP <- calculate_mode(mydata$DiastolicBP)
mode_bs <- calculate_mode(mydata$BS)
mode_bodytemp <- calculate_mode(mydata$BodyTemp)
mode_heartrate <- calculate_mode(mydata$HeartRate)
mode_age
mode_smoking
mode_systolicBP
mode_diastolicBP
mode_bs
mode_bodytemp
mode_heartrate

# Add mode values to the mean_data dataframe
mean_data$mode_value <- c(mode_age, mode_age, mode_systolicBP, mode_diastolicBP, 
                          mode_bs, mode_bodytemp, mode_heartrate)

# Plot mean and mode values using a bar plot
# Plot mode values using a bar plot
ggplot(mean_data, aes(x = attribute)) +
  geom_bar(aes(y = mode_value), stat = "identity", fill = "green") +
  labs(title = "Mode Values of Attributes",
       x = "Attribute", y = "Mode Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


dataset$RiskLevel <- factor(mydata$RiskLevel,
                           levels = c("highrisk", "lowrisk", "midrisk"),
                           labels = c("1", "2", "3"))
dataset



BodyTemp<- mydata$BodyTemp
min_max_normalization <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
normalized_age <- min_max_normalization(BodyTemp)
normalized_age


ageBoxplot <- boxplot(mydata$HeartRate)
outliers <- ageBoxplot$out
cat("Outliers are", outliers)
ageMean <- mean(mydata$HeartRate, na.rm = TRUE)
outlierPositions <- match(outliers, mydata$HeartRate)
mydata$HeartRate[outlierPositions] <- as.integer (ageMean)

mydata
negative_indices <- which(mydata < 0, arr.ind = TRUE)
print("Indices of negative values:")
negative_indices



min_non_negative <- min(mydata[mydata >= 0], na.rm = TRUE)
mydata[mydata < 0] <- min_non_negative
print("Invalid (negative) values have been successfully recovered.")
print(mydata)

