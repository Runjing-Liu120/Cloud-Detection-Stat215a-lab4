library(FNN)

smooth_features <- function(coord, feature_vec, k = 64){
  # coord: the location of the pixels n x 2 matrix of coordinates
  # feature_vec: vector of predicted features to be smoothed 
  # k: number of neighbors to consider
  
  knn_object <- knn.reg(coord, coord, feature_vec)
  
  return(knn_object$pred)
}

smooth_all_features <- function(X, coord, k = 64){
  # a wrapper on the function "smooth_features" above 
  # appends columns to X with features that are smoothed with KNN
  # coord is the location of the pixels (ie how we determine neighbors)
  
  feature_names <- colnames(X)
  features <- X[, feature_names]
  
  for(i in 1:length(feature_names)){
    features[[paste(feature_names[i], '_smoothed', sep = '')]] <- 
      smooth_features(coord, features[, i])
  }
  
  return(features)
}

prep_image <- function(filename = 'image1.txt', path = 'image_data/'){
  #This function takes in the image file name and path and 
  #returns a dataframe including the original columns with 
  #additional columns representing each feature smoothed. It also
  #preps the dataframe for the models by making the labels into factors
  #and getting rid of the observations with labeled "0".
  
  collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
  
  # Reading-in image, setting column names
  image <- read.table(paste0(path, filename), header = F)
  names(image) <- collabs
  
  # Adding columns of smoothed features
  image.features <- image[, -c(1,2,3)]
  image.labels <- image[, 3]
  image.added <- smooth_all_features(image.features,
                                     coord = image[, 1:2])
  image <- cbind(image[1:3], scale(image.added))
  
  # Getting rid of zero labels and making them factors
  image$label <- as.factor(image$label)
  image <- image %>%
    filter(label != 0)
  
  return(image)
}
