library(FNN)
library(dplyr)

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


load_image <- function(filename = 'image1.txt', path = 'image_data/'){
  # Loads image, adds column names, and makes labels into factors
  # prep for plotting
    
  collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
  
  # Reading-in image, setting column names
  image <- read.table(paste0(path, filename), header = F)
  names(image) <- collabs
  
  # Making labels factors
  image$label <- as.factor(image$label)
  
  # Adding columns of smoothed features
  image.features <- image[, -c(1,2,3)]
  image.labels <- image[, 3]
  image.added <- smooth_all_features(image.features,
                                     coord = image[, 1:2])
  image <- cbind(image[1:3], scale(image.added))
    
return(image)

}

prep_image <- function(image = image1.plots){
  # This function takes in an image dataframe and
  # returns a dataframe including the original columns with
  # additional columns representing each feature smoothed. It also
  # preps the dataframe for the models by making the labels into factors
  # and getting rid of the observations with labeled "0".
  
  # Reading-in image, setting column names, converting labels to factors
  #image <- load_image(filename, path)
  
  # Getting rid of zero labels
  image <- image %>%
    filter(label != 0)
  
  return(image)
}



plotr <- function(image = image1,
                  fill = image1$label,
                  legend.place = "bottom",
                  legend.title = "label"){
  # This function plots an image with a specified fill

  plot <- image %>%
    ggplot() +
    geom_point(aes(x = x, y = y, color = factor(fill))) +
    blank_theme +
    guides(color=guide_legend(legend.title,
    override.aes = list(size=10))) +
    theme(legend.position=legend.place)+
    coord_fixed(ratio = 1.11)
    
  return(plot)
}








