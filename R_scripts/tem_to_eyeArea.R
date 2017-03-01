# Take a .tem with 179 or 189 points and convert [x,y]
# coordinates to eye area.

# # FOR DEBUGGING ##
# tem <- 'data/3dsk_female_avg_convert.tem'
# tem <- 'data/MN1.tem'
# tem <- files[4]
# # END DEBUGGING ##

tem_to_eyeArea <- function(tem) {
  paks <- c('readr','dplyr','tidyr')
  lapply(paks, require, character.only=TRUE)
  
  df <- read.delim(tem, header=FALSE)
  rows <- nrow(df) - 1
  
  if (length(df) == 1) {
    data <- df %>%
      separate(., V1, c('x','y'), sep = "\\s") %>% 
      mutate(., row = seq(0, rows)) %>% 
      filter(., is.na(y) == FALSE)
    data$x <- as.numeric(as.character(data$x))
    data$y <- as.numeric(as.character(data$y))
    
    ##Left eye
    a <- abs(data$x[1] - data$x[23])
    b <- abs(data$y[1] - data$y[21])
    eyeAreaL <- a*b*pi
    
    ##Right eye
    a <- abs(data$x[2] - data$x[24])
    b <- abs(data$y[2] - data$y[26])
    eyeAreaR <- a*b*pi
    
  } else if (length(df) == 2) {
    data <- df
    names(data) <- c('x','y')
    data <- data %>% mutate(., row = seq(0, rows)) %>% 
      filter(., is.na(y) == FALSE)
    data$x <- as.numeric(as.character(data$x))
    data$y <- as.numeric(as.character(data$y))
    
    ##Left eye
    a <- abs(data$x[1] - data$x[23])
    b <- abs(data$y[1] - data$y[21])
    eyeAreaL <- a*b*pi
    
    ##Right eye
    a <- abs(data$x[2] - data$x[24])
    b <- abs(data$y[2] - data$y[26])
    eyeAreaR <- a*b*pi
    
  } else {
    print("This template is unrecognized. Do you have the correct formatting?")
    
  }
  
  return(list(image = tem, right_eyeArea=eyeAreaR, left_eyeArea=eyeAreaL))
  
}