# libs
library(caret)
library(ranger)
library(dplyr)

# loading data
ypath <- "data/"
image1 <- read.table(paste0(path, 'image1.txt'), header = F)
image2 <- read.table(paste0(path, 'image2.txt'), header = F)
image3 <- read.table(paste0(path, 'image3.txt'), header = F)

# add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

# plotting original data
ggplot(image1) + geom_point(aes(x = x, y = y, color = factor(label))) +
  scale_color_discrete(name = "Expert label")
ggplot(image1) + 
  geom_density(aes(x = AN, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  scale_fill_discrete(name = "Expert label")

# subsampling data for faster training
data_train <- sample_frac(rbind(image1, image2), 0.6) %>% 
  mutate(Class = factor(label)) %>% 
  select(-label)
# downsample data for class imbalance
data_train <- downSample(x = data_train[,-c(1,2,3)],
                         y = Class)
# test set
data_test <- image3[,-c(1,2)] %>% 
  mutate(Class = factor(label)) %>% 
  select(-label)

# use 5-fold cross validation
trControl <- trainControl(method = 'cv', number = 5)
# fit a model with all labels: +1, 0 -1
rf_fit <- train(Class ~ NDAI + SD + CORR + DF + CF + BF + AF + AN, 
                data = data_train, 
                method = "ranger",
                trControl=trControl)
# The final values used for the model were mtry = 8 and splitrule = extratrees.
# test the model
test_pred <- predict(rf_fit, data_test)
# compare predicted outcome and true outcome
confusionMatrix(test_pred, data_test$Class)

# add neighboring information and see effects
# mutate the data_train so that it contains information from neighboring pixels
add_neighbor_info <- function(image){
  image_l <- image %>%
    mutate(x = x - 1) %>% 
    select(-label)
  image_r <- image %>%
    mutate(x = x + 1) %>% 
    select(-label)
  image_u <- image %>%
    mutate(y = y + 1) %>% 
    select(-label)
  image_d <- image %>%
    mutate(y = y - 1) %>% 
    select(-label)
  image_neighbor <- merge(image, image_l, 
                          by = c('x', 'y'), suffixes = c('','_l'))
  image_neighbor <- merge(image_neighbor, image_r, 
                          by = c('x', 'y'), suffixes = c('','_r'))
  image_neighbor <- merge(image_neighbor, image_u, 
                          by = c('x', 'y'), suffixes = c('','_u'))
  image_neighbor <- merge(image_neighbor, image_d, 
                          by = c('x', 'y'), suffixes = c('','_d'))
  return(image_neighbor)
}
image1_neighbor <- add_neighbor_info(image1)
image2_neighbor <- add_neighbor_info(image2)
image3_neighbor <- add_neighbor_info(image3)

data_train_neighbor <- rbind(image1_neighbor, image2_neighbor) %>% 
  mutate(Class = factor(label)) %>% 
  select(-label, -y, -x) %>% 
  sample_frac(0.5)

data_test_neighbor <- image3_neighbor %>% 
  mutate(Class = factor(label)) %>% 
  select(-label, -y, -x)

# train a new random forest model
mtry <- 8 # number of variables selected randomly at each split
tunegrid <- expand.grid(.mtry=mtry, .splitrule='extratrees')
rf_fit_neighbor <- train(Class ~ ., 
                data = data_train_neighbor, 
                method = "ranger",
                trControl=trControl,
                tuneGrid=tunegrid)

test_pred_neighbor <- predict(rf_fit_neighbor, data_test_neighbor)
# compare predicted outcome and true outcome
confusionMatrix(test_pred_neighbor, data_test_neighbor$Class)

# If we don't keep unlabeled data:
# subsampling data for faster training
data_train_labeled <- rbind(image1, image2) %>% 
  filter(label != 0) %>% 
  mutate(Class = factor(label)) %>% 
  select(-label, -y, -x)
  
# test set
data_test_labeled <- image3 %>% 
  filter(label != 0) %>% 
  mutate(Class = factor(label)) %>% 
  select(-label, -y, -x)

mtry <- 8 # number of variables selected randomly at each split
tunegrid <- expand.grid(.mtry=mtry, .splitrule='extratrees')
rf_fit_labeled <- train(Class ~ ., 
                         data = data_train_labeled, 
                         method = "ranger",
                         trControl=trControl,
                         tuneGrid=tunegrid)

# compare predicted outcome and true outcome
test_pred_labeled <- predict(rf_fit_labeled, data_test_labeled)
confusionMatrix(test_pred_labeled, data_test_labeled$Class)

# plotting the prediction result
image3_predicted <- image3
image3_predicted$PredClass <- predict(rf_fit_labeled, image3_predicted)

ggplot(image3) + geom_point(aes(x = x, y = y, color = factor(label))) +
  scale_color_discrete(name = "Expert label")
ggplot(image3_predicted) + geom_point(aes(x = x, y = y, color = PredClass)) +
  scale_color_discrete(name = "Predicted label")
