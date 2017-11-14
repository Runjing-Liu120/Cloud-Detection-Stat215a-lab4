#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('resape2')
#install.packages('e1071')

library(dplyr)
library(ggplot2)
library(reshape2)
library(e1071)


# Get the data for three images
path <- 'image_data/'
image1 <- read.table(paste0(path, 'image1.txt'), header = F)
image2 <- read.table(paste0(path, 'image2.txt'), header = F)
image3 <- read.table(paste0(path, 'image3.txt'), header = F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs


# Getting rid of zero labels and making them factors
prep_image <- function(df){
  df$label <- as.factor(df$label)
  df <- df %>%
    filter(label != 0)
  return(df)
}

image1.prepped <- prep_image(image1)
image2.prepped <- prep_image(image2)
image3.prepped <- prep_image(image3)


# Specifying the covariates (need to include label)
col <- c('NDAI','SD','AF', 'label')

s <- sample(115229, 1000)

# Specifying train vs test set
image_train <- image2.prepped[s,col]
image_test <- image1.prepped[,col]


# Running the svm
svm.model <- svm(label~., 
              data = image_train, 
              kernel = 'polynomial', 
              cost = 100,
              scale = FALSE
              )

svm.model


# Predicting on test set
svm.predict <- predict(svm.model, image_test[,col], type = "class")

# Results of prediction
table(svm.predict, image_test[,4])
mean(svm.predict == image_test[,4])


# Tuning model
tuned <- tune(svm, label~., 
              data = image_train, 
              kernel = 'polynomial',
              ranges = list(cost = c(0.001, 0.01, .1, 10, 100)))
tuned

svm.model.tuned <- tuned$best.model

# Predicting on test set with tuned model
svm.predict <- predict(svm.model.tuned, image_test[,col], type = "class")

# Results of prediction with tuned model
table(svm.predict, image_test[,4])
mean(svm.predict == image_test[,4])
