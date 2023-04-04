#loading libraraies
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

# Loading the data sets
white_wine = read.csv(file="/Users/sahilgawande/Downloads/white_wines.csv", sep=";")
red_wine = read.csv(file="/Users/sahilgawande/Downloads/red_wines.csv", sep=";")

#combining data set
red_wine$type <- "red"
white_wine$type <- "white"
wine_data <- rbind(red_wine, white_wine)

#Checking for data summary
str(wine_data)
summary(wine_data)

#plotting the scores
ggplot(wine_data, aes(x = quality)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~type) +
  labs(title = "Distribution of Wine Quality Scores", x = "Quality", y = "Count") +
  theme_minimal()

#Scatter PLot
pairs(wine_data[,1:11], 
      main = "Scatterplot Matrix of Wine Data",
      col = ifelse(wine_data$type == "red", "red", "blue"),
      pch = 20)

#Correlation Matrix
cor_matrix <- cor(wine_data[,1:11])
print(cor_matrix)

#Using library to plot the correlation matrix
install.packages("ggcorrplot")
library(ggcorrplot)

corrplot <- ggcorrplot(cor_matrix,
                       method = "circle",
                       type = "lower",
                       lab = TRUE,
                       lab_size = 3,
                       title = "Correlation Heatmap",
                       colors = c("blue", "white", "red"),
                       legend.title = "Correlation")
print(corrplot)

# Check if there are any missing valuse
sum(is.na(wine_data))

# Margins to make the labes visible
par(mar=c(8,3,1,1))

# Box-and-Whisker plot
boxplot(wine_data[,1:11], las=2)

# Let's check if there any duplicated entries
anyDuplicated(wine_data)

# install.packages('e1071', dependencies = TRUE)
library(e1071)
library(matrixStats)

#SVM for_white_Wines
# Loading the data sets
white_wine = read.csv(file="/Users/sahilgawande/Downloads/white_wines.csv", sep=";")
red_wine = read.csv(file="/Users/sahilgawande/Downloads/red_wines.csv", sep=";")

df=white_wine
categories = character()

for(i in 1:nrow(df)){
  if(df$quality[i]<5){
    categories[i] = 'Bad'
  }else if(df$quality[i]>=5 & df$quality[i]<7){
    categories[i] = 'Good'
  }else
    categories[i] = 'Excellent'
}

df$quality = categories
head(df)
table(df$quality)

df = as.data.frame(df)
df$quality = as.factor(df$quality)
set.seed(160)
indis = sample(1:nrow(df), size = 0.8*nrow(df))
TrainSet = df[indis,]
TestSet = df[-indis,]
nrow(TrainSet)
nrow(TestSet)

svmControl = trainControl(method = 'cv', number = 10,
                          search = 'grid'
)

svmGrid = expand.grid(Gamma = 10^seq(-2,2,0.01), Cost = 10^seq(-2,2,0.01))

ModelSvm = train(quality~., data = TrainSet, method = 'svmRadial',
                 tune.Grid = svmGrid,
                 trControl = svmControl )


TahminSvm = predict(ModelSvm, TestSet)
confusionMatrix(TahminSvm, TestSet$quality)


#for Red wines
df=red_wine
categories = character()

for(i in 1:nrow(df)){
  if(df$quality[i]<5){
    categories[i] = 'Bad'
  }else if(df$quality[i]>=5 & df$quality[i]<7){
    categories[i] = 'Good'
  }else
    categories[i] = 'Excellent'
}

df$quality = categories
head(df)
table(df$quality)

df = as.data.frame(df)
df$quality = as.factor(df$quality)
set.seed(160)
indis = sample(1:nrow(df), size = 0.8*nrow(df))
TrainSet = df[indis,]
TestSet = df[-indis,]
nrow(TrainSet)
nrow(TestSet)

svmControl = trainControl(method = 'cv', number = 10,
                          search = 'grid'
)

svmGrid = expand.grid(Gamma = 10^seq(-2,2,0.01), Cost = 10^seq(-2,2,0.01))

ModelSvm = train(quality~., data = TrainSet, method = 'svmRadial',
                 tune.Grid = svmGrid,
                 trControl = svmControl )


TahminSvm = predict(ModelSvm, TestSet)
confusionMatrix(TahminSvm, TestSet$quality)

#Random Forest
install.packages("randomForest")
library(randomForest)

# Train a Random Forest model
model_rf <- randomForest(quality ~ ., data = TrainSet)

# Predict the test set using the Random Forest model
predictions_rf <- predict(model_rf, TestSet)

# Create a confusion matrix
cm_rf <- confusionMatrix(predictions_rf, TestSet$quality)

# Print the confusion matrix
print(cm_rf)

# Calculate the accuracy of the Random Forest model
accuracy_rf <- cm_rf$overall["Accuracy"]
cat("Random Forest Accuracy:", accuracy_rf)

# Load required libraries
library(ggplot2)

# Print the column names of the importance matrix
print(colnames(model_rf$importance))

# Extract variable importance data
importance_data <- data.frame(Variable = row.names(model_rf$importance),
                              Importance = model_rf$importance[, "MeanDecreaseGini"])

# Create a ggplot2 variable importance plot
ggplot(importance_data, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable Importance in the Random Forest Model",
       x = "Variable",
       y = "Importance",
       fill = "Importance") +
  scale_fill_gradient(low = "blue", high = "red")





