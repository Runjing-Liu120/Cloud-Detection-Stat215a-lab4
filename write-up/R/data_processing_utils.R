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
