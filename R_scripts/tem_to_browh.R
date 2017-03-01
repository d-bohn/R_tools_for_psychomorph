# Take a .tem with 179 or 189 points and convert [x,y]
# coordinates to brow heights according to
#  Ruth Campbell (1996) Real Men Don't Look Down: Direction of Gaze Affects
#  Sex Decisions on Faces, Visual Cognition, 3:4, 393-412, DOI: 10.1080/135062896395643


## FOR DEBUGGING ##
# tem <- 'data/3dsk_female_avg_convert.tem'
# tem <- 'data/MN1.tem'
# tem <- files[4]
## END DEBUGGING ##

tem_to_browh <- function(tem) {
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
    
    
    d1 <- abs(data$y[19]-data$y[84])
    d2 <- abs(data$y[1]-data$y[85])
    d3 <- abs(data$y[23]-data$y[77])
    d4 <- abs(data$y[24]-data$y[78])
    d5 <- abs(data$y[2]-data$y[84])
    d6 <- abs(data$y[28]-data$y[87])
    d7 <- (abs(data$x[5]-data$x[9])+abs(data$x[13]-data$x[17]))/2
    
    min_browL <- min(d1,d2,d3)/d7
    max_browL <- max(d1,d2,d3)/d7
    
    min_browR <- min(d4,d5,d6)/d7
    max_browR <- max(d4,d5,d6)/d7
    
    min_brow <- (min_browL+min_browR)/2
    max_brow <- (max_browL+max_browR)/2
    
  } else if (length(df) == 2) {
    data <- df
    names(data) <- c('x','y')
    data <- data %>%
      mutate(., row = seq(0, rows)) %>% 
      filter(., is.na(y) == FALSE)
    data$x <- as.numeric(as.character(data$x))
    data$y <- as.numeric(as.character(data$y))
    
    d1 <- abs(data$y[19]-data$y[84])
    d2 <- abs(data$y[1]-data$y[85])
    d3 <- abs(data$y[23]-data$y[77])
    d4 <- abs(data$y[24]-data$y[78])
    d5 <- abs(data$y[2]-data$y[84])
    d6 <- abs(data$y[28]-data$y[87])
    d7 <- (abs(data$x[5]-data$x[9])+abs(data$x[13]-data$x[17]))/2
    
    min_browL <- min(d1,d2,d3)/d7
    max_browL <- max(d1,d2,d3)/d7
    
    min_browR <- min(d4,d5,d6)/d7
    max_browR <- max(d4,d5,d6)/d7
    
    min_brow <- (min_browL+min_browR)/2
    max_brow <- (max_browL+max_browR)/2
    
  } else {
    print("This template is unrecognized. Do you have the correct formatting?")
    
  }
  
  return(list(image = tem, min_brow = min_brow, max_brow = max_brow))
  
}