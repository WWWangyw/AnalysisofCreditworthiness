library(kohonen)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(dplyr)

creditdata<-read.csv("creditworthiness.csv",header=TRUE,sep=",")
head(creditdata)  # View the first few rows of the dataset
summary(creditdata)
sapply(creditdata, function(x) length(unique(x)))


# Step 2: Statistical analysis of attributes and values
# Assuming the attributes are columns 1 to 46 and the credit worthiness is column 47

# Count unique values for each attribute
attribute_value_counts <- sapply(creditdata[, 1:45], function(x) length(unique(x)))
attribute_value_counts

sort(attribute_value_counts, decreasing = TRUE)

# Sort attributes by their unique value counts
sorted_attributes <- names(sort(attribute_value_counts, decreasing = TRUE))

# Display the five most interesting attributes
top_5_attributes <- sorted_attributes[1:5]
top_5_attributes

#R3:0.38
#R2:0.26
#R8:0.32
#R22:0.06
#R12:0.05
credit_copy<-read.csv("creditworthiness-copy.csv",header=TRUE,sep=",")

credit_copy_copy<-read.csv("creditworthiness-copy-copy.csv",header=TRUE,sep=",")
corrplot(corr=cor(credit_copy),method = "number",col="black",cl.pos = "n")
corrplot(corr=cor(credit_copy),method = "color")
corrplot(corr=cor(credit_copy_copy[10:20]),method = "number",col="black",cl.pos = "n")

#rs1<-eigen(rm1)
#val<-rs1$values
#(standard_deviation<-sqrt(val))
#(proportion_of_variance<-val/sum(val))
#(cumulative_proportion<-cumsum(proportion_of_variance))

# plot(rs1$values,type="b",
#      cex=2,
#      cex.lab=2,
#      cex.axis=2,
#      lty=2,
#      lwd=2,
#      xlab="主成分编号",
#      ylab="特征值（主成分方差）")
# 
# #提取结果中的特征向量(也称为Loadings,载荷矩阵)；
# (U<-as.matrix(rs1$vectors))
# #进行矩阵乘法，获得PC score；
# PC <-as.matrix(creditdata) %*% U
# head(PC)
# 
# #提取主成分的方差贡献率，生成坐标轴标题；
# xlab<-paste0("PC1(",round(Proportion_of_Variance[1]*100,2),"%)")
# ylab<-paste0("PC2(",round(Proportion_of_Variance[2]*100,2),"%)")
# #绘制散点图并添加置信椭圆；
# p1<-ggplot(data = df,aes(x=PC1,y=PC2,color=iris.Species))+
#   stat_ellipse(aes(fill=iris.Species),
#                type ="norm", geom ="polygon",alpha=0.2,color=NA)+
#   geom_point()+labs(x=xlab,y=ylab,color="")+
#   guides(fill=F)
# p1

#view the stracture of creditdata: no factor so 1:46
#prepare the data for  the som function
credit_som<-as.matrix(scale(creditdata[,1:45]))
scale(creditdata[, 1:45])
#creadit grid to be used for the som. Because we have 2500 observations, to make sure we dont make the grid too big, otherwise we will have some girds that have no observations on it.
#hexagonal:六边形
credit_grid<-somgrid(xdim=10,ydim=10, topo="hexagonal")
#use the som function
credit_som_model<-som(credit_som,
                      grid=credit_grid,
                      rlen = 100, alpha = c(0.9, 0.01))

#plot the results
#1.已分配给地图中每个节点的观测值的数量,越红的节点越少
plot(credit_som_model,type="counts")
#2.heatmap:从原始数据中选择的特定变量
plot(credit_som_model,
     type="property",
     property = getCodes(credit_som_model)[,22],
     main = colnames(creditdata[22]))
#3.fan diagram:和热图一样，但展示所有变量
plot(credit_som_model,type="codes")

plot(credit_som_model,type="mapping")

som_grid <- som(scale(creditdata[, 1:45]), 
                grid = somgrid(5, 5, "hexagonal"))




# X <- creditdata[, -46]
# 
# # Normalize the data
# X <- scale(creditdata[, -46])
# 
# # Define SOM parameters
# grid_size <- c(10, 10)
# sigma <- 1.0
# learning_rate <- 0.5
# num_epochs <- 100
# 
# # Initialize SOM
# som_model <- somgrid(grid_size[1], grid_size[2], "hexagonal")
# som_result <- som(scale(creditdata[, -46]), grid = som_model, rlen = num_epochs, alpha = learning_rate, keep.data = TRUE)
# 
# # Plot SOM
# plot(som_result, type="codes")


# Assuming 'creditdata' is preprocessed and split into training and test sets
# train_X, train_y: Training features and labels
# test_X, test_y: Test features and labels

#training:testing=7:3
#set training set
#SOM 是一种无监督分类算法，因此您不应期望它在包含分类器标签的数据集上进行训练(如果您这样做，它将需要此信息才能工作，并且对未标记的数据集毫无用处)
sample.size <- 1750
credit.set <- as.matrix(creditdata[ -46])

grid.size <- 10

# create the grid
som.grid <- somgrid(xdim = grid.size, ydim = grid.size, topo = 'hexagonal', neighbourhood.fct = 'gaussian', toroidal = T)

#train the SOM...depending on sample.size this may take a while
som.model <- supersom(data.matrix(credit.set), grid = som.grid, rlen = 100, alpha = c(0.9, 0.01), radius = c(grid.size/2,1), dist.fcts = "sumofsquares", keep.data = TRUE, mode='online', normalizeDataLayers = FALSE)
# Generate plots after training.
# 计算 quantization error
# 获取每次迭代后的 Mean distance to closest unit
mean_distance <- som.model$changes
# 获取迭代次数
iterations <- seq_along(mean_distance)
# 绘制 Mean distance to closest unit 随迭代次数的变化
plot(iterations, mean_distance, type = "l", xlab = "Iterations", ylab = "Mean Distance", main = "Mean Distance to Closest Unit")

plot(som.model, type = "changes")

# Plot the heatmap for a variable at scaled / normalised values
var <- 2
plot(som.model, type = "property", property = som.model$codes[V2], main=names(som.model$data)[V2])
som.model$codes

plot(som.model, type = 'counts')
plot(som.model, type = 'mapping', pchs=20)
credit.class0 <- credit.set[creditdata$credit.rating == '0',]
credit.mapped0 <- map(som.model, newdata = credit.class0)

# the map function maps a given set of samples onto a trained SOM. The function
# returns an element called unit.classif which contains the ID of the best
# matching codebook. From this we can derive the x,y coordinate of the
# winning codebook as follows.
credit.mapped0.x <- credit.mapped0$unit.classif / grid.size
credit.mapped0.y <- credit.mapped0$unit.classif %% grid.size

sum(credit.mapped0$unit.classif>0)
length(credit.mapped0$unit.classif)

library(RSNNS)

#Load dataset
fullDataSet <- read.csv("creditworthiness.csv")

#select all entries for which the credit rating is known
knownData <- subset(fullDataSet, fullDataSet[,46] > 0)

#select all entries for which the credit rating is unknown
unknownData <- subset(fullDataSet, fullDataSet[,46] == 0)

#separate value from targets
trainValues <- knownData[,1:45]
unknownsValues <- unknownData[,1:45]
#对knowdata进行label编码
trainTargets <- decodeClassLabels(knownData[,46])


#split dataset into traing and test set
trainset <- splitForTrainingAndTest(trainValues, trainTargets, ratio=0.5)
trainset <- normTrainingAndTestSet(trainset)

#45 sensoryinput hidden neurons 把250改成了100
model <- mlp(trainset$inputsTrain, trainset$targetsTrain, size=5, learnFunc="Rprop",learnFuncParams=c(0.01), maxit=500, inputsTest=trainset$inputsTest, targetsTest=trainset$targetsTest)
predictTestSet <- predict(model,trainset$inputsTest)

confusionMatrix(trainset$targetsTrain,fitted.values(model))
sum_train_total<-sum(confusionMatrix(trainset$targetsTrain,fitted.values(model)))
sum_train_diag<-sum(diag(confusionMatrix(trainset$targetsTrain,fitted.values(model))))
sum_train_diag*100/sum_train_total

confusionMatrix(trainset$targetsTest,predictTestSet)

par(mfrow=c(2,2))
plotIterativeError(model)
plotRegressionError(predictTestSet[,2], trainset$targetsTest[,2])
plotROC(fitted.values(model)[,2], trainset$targetsTrain[,2])
plotROC(predictTestSet[,2], trainset$targetsTest[,2])

plotRegressionError(predictTestSet[,2], trainset$targetsTest[,2])

plotRegressionError(predictTestSet[,1], trainset$targetsTest[,1])
plotRegressionError(predictTestSet[,3], trainset$targetsTest[,3])

#confusion matrix with 402040-method
confusionMatrix(trainset$targetsTrain, encodeClassLabels(fitted.values(model),method="402040", l=0.4, h=0.6))


#show detailed information of the model
summary(model)
model
weightMatrix(model)
extractNetInfo(model)



# Load necessary libraries
library(randomForest)

# Step 1: Load data
credit_data <- read.csv("creditworthiness.csv")

# Step 2: Preprocess data
# Handling missing values (if any)
credit_data <- na.omit(credit_data)

# Convert categorical variables to factors
credit_data$credit.rating <- as.factor(credit_data$credit.rating) # Convert the target variable to factor

# Step 3: Split data into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(credit_data), 0.8*nrow(credit_data)) # 80% training data
train_data <- credit_data[train_index, ]
test_data <- credit_data[-train_index, ]

# Step 4: Train the random forest model
rf_model <- randomForest(credit.rating ~ ., data = train_data, ntree = 1750)

# Step 5: Evaluate the model
predicted_labels <- predict(rf_model, test_data)
conf_matrix <- table(predicted_labels, test_data$credit.rating)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Print evaluation metrics
print(conf_matrix)
print(paste("Accuracy:", accuracy))

# Step 6: Discuss strengths and weaknesses
# Strengths:
# - Random forests handle categorical variables well without the need for one-hot encoding.
# - They can handle large datasets with high dimensionality.
# - Random forests are less prone to overfitting compared to decision trees.
# - They provide estimates of variable importance, which can be useful for feature selection.

# Weaknesses:
# - Random forests can be computationally expensive, especially for large datasets and large numbers of trees.
# - They may not perform well if there are highly correlated features.
# - Interpretability can be challenging compared to simpler models like decision trees.
# - Random forests may not perform well with imbalanced datasets without proper balancing techniques.

