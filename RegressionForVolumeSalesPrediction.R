######################################################################################
# Multiple Regression in R                                                           #
#                                                                                    #
# Mizuki Kakei                                                                       #
#                                                                                    #
# Version 1.0                                                                        #
#                                                                                    #      
# Date : 10.05.2019                                                                  #     
#                                                                                    #
# We predict the data of new products based on existing producs data                 #     
# Use two of three different models to predicts the volume of new products           #
# And we choose 4 out of 25 produts.                                                 #
# Randomforest, KNN, SVM were used for modeling.                                     #                          
# In the end, the prediction was created based on SVM model.                         #
#                                                                                    #
######################################################################################

# Program setting ####
# calling package
library(readr)
library(caret)
library(C50)
library(inum)
library(plyr)
library(ggplot2)
library(pacman)
library(corrplot)
library(RColorBrewer)
library(e1071)
library(miscTools)


# Reading data ####
ExistData <- read.csv("existingproductattributes2017.csv", TRUE, sep =",")

# Preprocessing Data ####
# Dummyfiyng Data
ExistDataDummying <- dummyVars(" ~ .", data = ExistData)
ExistDataDummified <- data.frame(predict(ExistDataDummying,
                                         newdata = ExistData))

# Replacing NA values with median 
ExistData$BestSellersRank[is.na(ExistData$BestSellersRank)] <-
  median(ExistData$BestSellersRank, na.rm = TRUE)

# Checking correlation coefficient #####
# Preprocessing for correlation matrix
CorrelationExistData <- ExistData
CorrelationExistData$ProductNum <- NULL
CorrelationExistData$ProductType <- NULL

# Calculating correlation coefficient 
ExistProduct.cor <- cor(CorrelationExistData)

# Plotting correlation coefficient
ExistProduct.cor.plot <- corrplot(ExistProduct.cor, 
                                  method = "number", 
                                  type = "upper",
                                  tl.cex = 0.4,
                                  number.cex = 0.4,
                                  col = brewer.pal( n = 8, name="RdYlBu"))


# Excluding unimportant attributes based on correlation coefficient
ExistDataDummified$ProductNum <- NULL
ExistDataDummified$x1StarReviews <- NULL
ExistDataDummified$x3StarReviews <- NULL
ExistDataDummified$x5StarReviews <- NULL
ExistDataDummified$BestSellersRank <- NULL
ExistDataDummified$Recommendproduct <- NULL
ExistDataDummified$ProductHeight <- NULL
ExistDataDummified$ProductDepth <- NULL
ExistDataDummified$ProfitMargin <- NULL
ExistDataDummified$NegativeServiceReview <- NULL


# Modeling ####
# Sampling Data
set.seed(123)
ExistData.sample <- ExistDataDummified[sample(1:nrow(ExistDataDummified),
                                              80,
                                              replace = FALSE),]

# Splitting Data into Training and Test set
inTraining <- createDataPartition(ExistData.sample$Volume,
                                  p = .75,
                                  list = FALSE)
training <- ExistData.sample[inTraining,]
testing <- ExistData.sample[-inTraining,]

# Cross validation within training set
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 2)

# Modeling  with RandomForest ####
rfGrid <- expand.grid(mtry = c(5))
rfModel <- train(Volume~.,
                 data = training,
                 method = "rf",
                 trControl = fitControl,
                 tuneGrid = rfGrid,
                 tunelength = 1)

# Prediction based on Randomforest model
predictionrf <- predict(rfModel, testing)
predictionrf

# Checking accuracy of the prediction with test data
postResample(predictionrf, testing$Volume)

# Modeling with knn ####
knnGrid <- expand.grid(k=c(5))
knnmodel <- train(Volume~ .,
                  data = training,
                  method = "knn",
                  tuneGrid = knnGrid,
                  trControl = fitControl)

# Checking variable importance and accuracy between model and test set
varImp(knnmodel)

# Prediction based on KNN model
knnPrediction <- predict(knnmodel, testing)

# Checking the accuracy of the prediction with test data
postResample(knnPrediction, testing$Volume)


# Modeling with SVM ####
svmGrid <- expand.grid(C=c(5))
svmModel <- train(Volume~.,
                   data = training,
                   method = "svmLinear",
                   trControl = fitControl,
                   tuneGrid = svmGrid,
                   tunelength = 1)

# Prediction based on SVM model
svmPrediction <- predict(svmModel, testing)

# Checking variable importance for SVM Model
varImp(svmModel)

# Checking the accuracy between prediction and test set
postResample(svmPrediction, testing$Volume)

# Preparing data for New products ####
# Reading data
NewProductData <- read.csv("newproductattributes2017.csv",
                           TRUE,
                           sep =",")

# Preprocessing Data ####
# Dummifying Data
NewProductDummifying <- dummyVars(" ~ .", data = NewProductData)
NewProductDummified <- data.frame(predict(NewProductDummifying,
                                          newdata = NewProductData))

# Excluding unimportant attributes as same as Existing products data ####
NewProductDummified$ProductNum <- NULL
NewProductDummified$x1StarReviews <- NULL
NewProductDummified$x3StarReviews <- NULL
NewProductDummified$x5StarReviews <- NULL
NewProductDummified$Recommendproduct <- NULL
NewProductDummified$NegativeServiceReview <- NULL
NewProductDummified$ProductDepth <- NULL
NewProductDummified$ProductHeight <- NULL
NewProductDummified$BestSellersRank <- NULL
NewProductDummified$ProductWidth <- NULL
NewProductDummified$ProfitMargin <- NULL

# Making volume prediction of new products ####
NewProductPrediction <- predict(svmModel,
                                NewProductDummified)

# Replacing the value of Volume with predicted values
NewProductData$Volume <- NewProductPrediction

# Replacing Negative values with 0
NewProductData$Volume[NewProductData$Volume < 0 ] <- 0 

# Adding new coloumns and calculating sales of new products
NewProductData$Sales <- NewProductData$Volume *
                        NewProductData$Price *
                        NewProductData$ProfitMargin

# Correlation coefficients of new products with prediction
NewProduct.cor <- cor(NewProductDummified)
NewProduct.cor.plot <- corrplot(NewProduct.cor, 
                                method = "number", 
                                type = "upper",
                                tl.cex = 0.4,
                                number.cex = 0.6,
                                col = brewer.pal( n = 8, name="RdYlBu"))


# Visualization ####
# Predicted Sales Volume
NewProductDataframe <- data.frame(ProductType =  c("PC","Laptop","Netbook","Tablet","Smartphone",
                                                   "GameConsole","Display","Accessories","Software",
                                                   "Printer","PrinterSupplies","ExtendedWarranty"),
                                  Volume = c(394.4217952, 106.2870783, 1421.623014, 
                                             5229.928018,1600.195319,3780.146345,0,
                                             111.2153776,208.5854049,118.3693964,
                                             33.95003943,1253.982826))
barGraphvolume <- ggplot(NewProductDataframe, 
                         aes(x = ProductType, y = Volume)) +
                         geom_bar(stat = "identity",
                             fill = "#00AFBB", 
                             color = "#00AFBB") + 
                         xlab("Product Type") + 
                         ylab("Volume") + 
                         theme(axis.text.x = element_text(angle = 45,hjust = 1))
barGraphvolume

# Predicted Profit 
barGraphprofit <- ggplot(NewProductData, aes(x = Sales)) + 
                  geom_bar() + 
                  scale_fill_manual(values = c("skyblue"))

# Profit Margin
barGraphprofitmargin <- ggplot(NewProductData, aes(x = ProfitMargin)) + 
                        geom_bar() + 
                        scale_fill_manual(values = c("yellow"))
