dataset <- read.csv("C:/Users/tarik/Desktop/10 semester/Data_Science_Lab/Data-Science/cancer.csv",
                    header=TRUE, sep=",")
dataset
names(dataset)

remove<- na.omit(dataset)
dataset
colSums(is.na(dataset))


library(ggplot2)

dataset$diagnosis <- as.factor(dataset$diagnosis)
ggplot(dataset, aes(x = smoothness_mean, fill = diagnosis)) +
  geom_histogram(color = "black", bins = 30) +
  scale_fill_manual(values = c("#A865C9", "#f6abb6")) +
  labs(title = "Histogram of Smoothness Mean", x = "Smoothness Mean", y = "Frequency")

dataset$diagnosis <- as.factor(dataset$diagnosis)
ggplot(dataset, aes(x = compactness_mean, fill = diagnosis)) +
  geom_histogram(color = "black", bins = 30) +
  scale_fill_manual(values = c("#A865C9", "#f6abb6")) +
  labs(title = "Histogram of Compactness Mean", x = "Compactness Mean", y = "Frequency")



barplot(table(dataset$diagnosis), main="Diagnosis Distribution", xlab="Grade", ylab="Frequency",col = "lightpink")



windows(width = 10, height = 8)  
par(mar = c(5, 5, 2, 2)) 

boxplot(dataset$radius_mean, main = "Box Plot of Column Data", xlab = "Radius",ylab = "Value",col = "lightgreen",border = "black")


boxplot_stats <- boxplot.stats(dataset$radius_mean)
print(boxplot_stats$stats[5])
invalid_radius <- which(dataset$radius_mean > 21.16)
cat(invalid_radius)
valid_radius <- dataset$radius_mean <= 21.16
dataset <- dataset[valid_radius, ]
boxplot(dataset$radius_mean, main = "Box Plot of Column Data", xlab = "Radius",ylab = "Value",col = "lightgreen",border = "black")
invalid_radius <- which(dataset$radius_mean < 6.981)
cat(invalid_radius)
valid_radius <- dataset$radius_mean >= 6.981
dataset <- dataset[valid_radius, ]
boxplot(dataset$radius_mean, main = "Box Plot of Radius Data", xlab = "Radius",ylab = "Value",col = "lightgreen",border = "black")




library(ggplot2)
dataset$diagnosis <- as.factor(dataset$diagnosis)
ggplot(dataset, aes(x = symmetry_mean, y = smoothness_mean, color = diagnosis)) +
  geom_point() +
  scale_color_manual(values = c("green", "red")) +
  labs(title = "Scatter Plot of Symmetry Worst", x = "Symmetry Worst", y = "smoothness_mean")

library(ggplot2)
dataset$diagnosis <- as.factor(dataset$diagnosis)
ggplot(dataset, aes(x = id, y = radius_mean, color = diagnosis)) +
  geom_point() +geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Scatter Plot of ID vs Radius Mean", x = "ID", y = "Radius Mean")

library(ggplot2)
ggplot(dataset, aes(x = id, y = smoothness_mean, color = diagnosis)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot ", x = "ID", y = "Smoothness Mean")

library(ggplot2)
dataset$diagnosis <- as.factor(dataset$diagnosis)
ggplot(dataset, aes(x = id, y = radius_worst, color = diagnosis)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Scatter Plot", x = "ID", y = "Radius Worst")

library(ggplot2)
ggplot(dataset, aes(x = perimeter_worst)) +
  geom_density(fill = "magenta", color = "black") +
  labs(title = "Density Plot of Perimeter Worst", x = "Perimeter Worst", y = "Density")



windows(width = 10, height = 10) 
par(mar = c(5, 5, 2, 2))
plot(dataset$diagnosis,type = "l",main = "Line Plot of Area",xlab = "Area",ylab = " Symmetry",col = "red")



library(ggplot2)
ggplot(data = dataset, aes(x = compactness_mean, y = concavity_mean)) +
  geom_area() +
  labs(title = "Area Chart", x = "Compactness Mean", y = "Concavity Mean")

ggplot(data = dataset, aes(x = compactness_mean, y = concavity_mean, fill = diagnosis)) +
  geom_area(color = "red") +
  labs(title = "Area Chart", x = "Compactness Mean", y = "Concavity Mean", fill = "Diagnosis")




library(corrplot)
library(dplyr)
numeric_cols <- sapply(dataset, is.numeric)
numeric_data <- dataset[, numeric_cols]
correlation_matrix <- cor(numeric_data)
corrplot(correlation_matrix, method = "Heatmap color", col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200), tl.cex = 0.7)
cor_target <- abs(cor(dataset$diagnosis, numeric_data))
relevant_features <- cor_target[cor_target > 0.2]
names <- names(relevant_features)
names <- names[names != 'diagnosis']



library(ggplot2)
ggplot(data = dataset, aes(x = diagnosis, y = fractal_dimension_se, size = 20, color = 10)) +
  geom_point(alpha = 0.7) +
  labs(title = "Bubble Chart", x = "Diagnosis ", y = "Fractal Dimension SE", size = "Bubble Size", color = "Bubble Color")



library(ggplot2)
library(tidyr)
library(dplyr)

dataset <- data.frame(
  diagnosis= rep(c("M", "B"), each = 5),
  smoothness_mean = c(2.5, 2.1),
  compactness_mean = c(3.0, 3.4),
  concavity_mean = c(3.5, 2.7),
  concave_points_mean = c(2.8, 2.9),
  symmetry_mean = c(3.2, 3.1)
)
data_long <- dataset %>%
  pivot_longer(cols = -diagnosis, names_to = "malignant", values_to = "benign")

ggplot(data_long, aes(x = malignant, y = benign, fill = diagnosis)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +
  labs(title = "violin plot Comparison of Mean Values between Cancer Diagnosis M and B",
       x = "malignant",
       y = "benign") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(fmsb)
data <- data.frame(
  row.names = c("M", "B"),
  smoothness_worst= c(4, 5),
  concavity_worst= c(3, 4),
  texture_se= c(5, 3),
  symmetry_se= c(4, 4),
  smoothness_mean = c(4, 3),
  compactness_mean = c(3, 5),
  concavity_mean = c(5, 4),
  concave_points_mean = c(4, 5),
  symmetry_mean = c(3, 4)
)

data <- rbind(rep(5, 9), rep(1, 9), data)
radarchart(data, 
           axistype = 1,
           pcol = c("red", "blue"), 
           pfcol = c(rgb(1,0,0,0.2), rgb(0,0,1,0.2)), 
           plwd = 2,
           plty = 1,
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           caxislabels = seq(1, 5, 1), 
           calcex = 0.7,
           title = "Performance Comparison of Cancer Patient  M and B"
)
legend(x = "topright", legend = rownames(data[-c(1,2),]), col = c("red", "blue"), pch = 20, bty = "n", pt.cex = 2, cex = 0.8, text.col = "black", horiz = F, inset = c(0.1, 0.1))






library(ggplot2)
diagnosis_counts <- table(dataset$diagnosis)
pie(diagnosis_counts, 
    main = "Diagnosis Distribution pic chart", 
    labels = c("Benign", "Malignant"),  
    col = c("lightblue", "pink"),  
    clockwise = TRUE,  
    density = NULL,  
    angle = 45  
)



library(treemap)
library(dplyr)
counts <- dataset %>%
  count(diagnosis)
data <- data.frame(
  diagnosis = counts$diagnosis,
  category = "instance",
  value = counts$n
)
treemap(data,
        index=c("diagnosis", "category"),
        vSize="value",
        title="Treemap of Diagnosis and Category",
        fontsize.title=20,
        fontsize.labels=15,
        fontfamily.labels="Arial")






