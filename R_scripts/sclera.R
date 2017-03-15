# Take a .jpg file with an associated .tem file and compute 
# amount of sclera above and below the eye

# # FOR DEBUGGING ##
# tem <- 'data/CFD-BM-003-003-N.tem'
# threshold = .0011
# # END DEBUGGING ##

sclera <- function(tem, threshold = .0011){
  paks <- c('readr','dplyr','tidyr','foreach','imager')
  lapply(paks, require, character.only=TRUE)
  
  ## Read .tem file
  df <- read.delim(tem, header=FALSE)
  rows <- nrow(df) - 1
  
  ## Read in image and convert
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
    
    ## Sclera below/above inner and outer potion of left iris
    outerLeft <- data$y[5]
    outerLeftX <- data$x[5]
    innerLeft <- data$y[9]
    innerLeftX <- data$x[9]
    check1.1 <- seq(outerLeft,bottomLeft,1)
    check1.2 <- seq(topLeft,outerLeft,1)
    check2.1 <- seq(innerLeft,bottomLeft,1)
    check2.2 <- seq(topLeft,innerLeft,1)
    
    outer_left_below <- foreach (i = check1.1[1]:max(check1.1), .combine='rbind') %do% pixels[outerLeftX,i]
    outer_left_above <- foreach (i = check1.2[1]:max(check1.2), .combine='rbind') %do% pixels[outerLeftX,i]
    
    inner_left_below <- foreach (i = check2.1[1]:max(check2.1), .combine='rbind') %do% pixels[innerLeftX,i]
    inner_left_above <- foreach (i = check2.2[1]:max(check2.2), .combine='rbind') %do% pixels[innerLeftX,i]
    
    ## Sclera below/above right iris
    rightPupil <- data$y[2]
    rightPupilX <- data$x[2]
    bottomRight <- data$y[33]
    topRight <- data$y[26]
    check3 <- seq(rightPupil,bottomRight,1)
    check4 <- seq(topRight,rightPupil,1)
    
    pupil_right_below <- foreach (i = check3[1]:max(check3), .combine='rbind') %do% pixels[rightPupilX,i]
    pupil_right_above <- foreach (i = check4[1]:max(check4), .combine='rbind') %do% pixels[rightPupilX,i]
    
    ## Sclera below/above inner and outer potion of right iris
    outerRight <- data$y[17]
    outerRightX <- data$x[17]
    innerRight <- data$y[13]
    innerRightX <- data$x[13]
    check3.1 <- seq(outerRight,bottomRight,1)
    check3.2 <- seq(topRight,outerRight,1)
    check4.1 <- seq(innerRight,bottomRight,1)
    check4.2 <- seq(topRight,innerRight,1)
    
    outer_right_below <- foreach (i = check3.1[1]:max(check3.1), .combine='rbind') %do% pixels[outerRightX,i]
    outer_right_above <- foreach (i = check3.2[1]:max(check3.2), .combine='rbind') %do% pixels[outerRightX,i]
    
    inner_right_below <- foreach (i = check4.1[1]:max(check4.1), .combine='rbind') %do% pixels[innerRightX,i]
    inner_right_above <- foreach (i = check4.2[1]:max(check4.2), .combine='rbind') %do% pixels[innerRightX,i]
    
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
    
    ## Sclera below/above inner and outer potion of left iris
    outerLeft <- data$y[5]
    outerLeftX <- data$x[5]
    innerLeft <- data$y[9]
    innerLeftX <- data$x[9]
    check1.1 <- seq(outerLeft,bottomLeft,1)
    check1.2 <- seq(topLeft,outerLeft,1)
    check2.1 <- seq(innerLeft,bottomLeft,1)
    check2.2 <- seq(topLeft,innerLeft,1)
    
    outer_left_below <- foreach (i = check1.1[1]:max(check1.1), .combine='rbind') %do% pixels[outerLeftX,i]
    outer_left_above <- foreach (i = check1.2[1]:max(check1.2), .combine='rbind') %do% pixels[outerLeftX,i]
    
    inner_left_below <- foreach (i = check2.1[1]:max(check2.1), .combine='rbind') %do% pixels[innerLeftX,i]
    inner_left_above <- foreach (i = check2.2[1]:max(check2.2), .combine='rbind') %do% pixels[innerLeftX,i]
    
    ## Sclera below/above right iris
    rightPupil <- data$y[2]
    rightPupilX <- data$x[2]
    bottomRight <- data$y[33]
    topRight <- data$y[26]
    check3 <- seq(rightPupil,bottomRight,1)
    check4 <- seq(topRight,rightPupil,1)
    
    pupil_right_below <- foreach (i = check3[1]:max(check3), .combine='rbind') %do% pixels[rightPupilX,i]
    pupil_right_above <- foreach (i = check4[1]:max(check4), .combine='rbind') %do% pixels[rightPupilX,i]
    
    ## Sclera below/above inner and outer potion of right iris
    outerRight <- data$y[17]
    outerRightX <- data$x[17]
    innerRight <- data$y[13]
    innerRightX <- data$x[13]
    check3.1 <- seq(outerRight,bottomRight,1)
    check3.2 <- seq(topRight,outerRight,1)
    check4.1 <- seq(innerRight,bottomRight,1)
    check4.2 <- seq(topRight,innerRight,1)
    
    outer_right_below <- foreach (i = check3.1[1]:max(check3.1), .combine='rbind') %do% pixels[outerRightX,i]
    outer_right_above <- foreach (i = check3.2[1]:max(check3.2), .combine='rbind') %do% pixels[outerRightX,i]
    
    inner_right_below <- foreach (i = check4.1[1]:max(check4.1), .combine='rbind') %do% pixels[innerRightX,i]
    inner_right_above <- foreach (i = check4.2[1]:max(check4.2), .combine='rbind') %do% pixels[innerRightX,i]
  }
  
  ## Return list and end
  return(list(
    image = tem,
    ## Create values
    left_below_pupil = sum(pupil_left_below)/nrow(pupil_left_below),
    left_above_pupil = sum(pupil_left_above)/nrow(pupil_left_above),
    outer_left_below = sum(outer_left_below)/nrow(outer_left_below),
    outer_left_above = sum(outer_left_above)/nrow(outer_left_above),
    inner_left_below = sum(inner_left_below)/nrow(inner_left_below),
    inner_left_above = sum(inner_left_above)/nrow(inner_left_above),
    right_below_pupil = sum(pupil_right_below)/nrow(pupil_right_below),
    right_above_pupil = sum(pupil_right_above)/nrow(pupil_right_above),
    outer_right_below = sum(outer_right_below)/nrow(outer_right_below),
    outer_right_above = sum(outer_right_above)/nrow(outer_right_above),
    inner_right_below = sum(inner_right_below)/nrow(inner_right_below),
    inner_right_above = sum(inner_right_above)/nrow(inner_right_above)
    ))
  
  }
  