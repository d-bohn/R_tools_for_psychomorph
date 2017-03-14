# Take a .jpg file with an associated .tem file and compute 
# amount of sclera above and below the eye

# # FOR DEBUGGING ##
# tem <- 'data/CFD-BM-003-003-N.tem'
# threshold = .0011
# # END DEBUGGING ##

sclera <- function(tem, threshold = .0011){
  paks <- c('readr','dplyr','tidyr','foreach')
  lapply(paks, require, character.only=TRUE)
  
  ## Read .tem file
  df <- read.delim(tem, header=FALSE)
  rows <- nrow(df) - 1
  
  ## Read in image and convert
  library(imager)
  im <- sub('.tem','.jpg', tem)
  image <- load.image(im)
  image <- grayscale(image)
  #display(threshold(image, thr = .0011))
  pixels <- as.matrix(threshold(image, thr = threshold))
  #pixels <- as.data.frame(threshold(image, thr = threshold))
  
  if (length(df) == 1) {
    data <- df %>%
      separate(., V1, c('x','y'), sep = "\\s") %>% 
      mutate(., row = seq(0, rows)) %>% 
      filter(., row <= 179, is.na(y) == FALSE)
    data$x <- as.numeric(as.character(data$x))
    data$y <- as.numeric(as.character(data$y))
    
    ## Sclera below/above left iris
    leftPupil <- data$y[1]
    leftPupilX <- data$x[1]
    bottomLeft <- data$y[30]
    topLeft <- data$y[21]
    check1 <- seq(leftPupil,bottomLeft,1)
    check2 <- seq(topLeft,leftPupil,1)
    
    pupil_left_below <- foreach (i = check1[1]:max(check1), .combine='rbind') %do% pixels[leftPupilX,i]
    pupil_left_above <- foreach (i = check2[1]:max(check2), .combine='rbind') %do% pixels[leftPupilX,i]
    
    ## Sclera below right iris
    rightPupil <- data$y[2]
    rightPupilX <- data$x[2]
    bottomRight <- data$y[33]
    topRight <- data$y[26]
    check3 <- seq(rightPupil,bottomRight,1)
    check4 <- seq(topRight,rightPupil,1)
    
    pupil_right_below <- foreach (i = check3[1]:max(check3), .combine='rbind') %do% pixels[rightPupilX,i]
    pupil_right_above <- foreach (i = check4[1]:max(check4), .combine='rbind') %do% pixels[rightPupilX,i]
    
  } else if (length(df) == 2) {
    data <- df
    names(data) <- c('x','y')
    data <- data %>%
      mutate(., row = seq(0, rows)) %>% 
      filter(., is.na(y) == FALSE)
    data$x <- as.numeric(as.character(data$x))
    data$y <- as.numeric(as.character(data$y))
    
    ## Sclera below/above left iris
    leftPupil <- data$y[1]
    leftPupilX <- data$x[1]
    bottomLeft <- data$y[30]
    topLeft <- data$y[21]
    check1 <- seq(leftPupil,bottomLeft,1)
    check2 <- seq(topLeft,leftPupil,1)
    
    pupil_left_below <- foreach (i = check1[1]:max(check1), .combine='rbind') %do% pixels[leftPupilX,i]
    pupil_left_above <- foreach (i = check2[1]:max(check2), .combine='rbind') %do% pixels[leftPupilX,i]
    
    ## Sclera below right iris
    rightPupil <- data$y[2]
    rightPupilX <- data$x[2]
    bottomRight <- data$y[33]
    topRight <- data$y[26]
    check3 <- seq(rightPupil,bottomRight,1)
    check4 <- seq(topRight,rightPupil,1)
    
    pupil_right_below <- foreach (i = check3[1]:max(check3), .combine='rbind') %do% pixels[rightPupilX,i]
    pupil_right_above <- foreach (i = check4[1]:max(check4), .combine='rbind') %do% pixels[rightPupilX,i]
  
  }
  
  ## Return list and end
  return(list(
    image = tem,
    ## Create values
    left_below = sum(pupil_left_below)/nrow(pupil_left_below),
    left_above = sum(pupil_left_above)/nrow(pupil_left_above),
    right_below = sum(pupil_right_below)/nrow(pupil_right_below),
    right_above = sum(pupil_right_above)/nrow(pupil_right_above))
    )
  
  }
  