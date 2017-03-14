# Take a .tem with 179 or 189 points and convert [x,y]
# coordinates to fWH ratio.

# # FOR DEBUGGING ##
# tem <- 'data/3dsk_female_avg_convert.tem'
# tem <- 'data/MN1.tem'
# tem <- files[4]
# # END DEBUGGING ##

tem_to_all <- function(tem) {
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
    
    ##Facial width-to-height ratio
    fwhr <- with(data,
                 abs(max(x[114],x[113],x[115])-min(x[112],x[112],x[120]))/abs(y[91]-min(y[21],y[26])))
    
    ## Space between eyes
    eyespace <- with(data,
                     sqrt(((x[1]-x[2])^2) + ((y[1]-y[2])^2))
                     )
    
    ## Left eye area
    a <- abs(data$x[1] - data$x[23])
    b <- abs(data$y[1] - data$y[21])
    eyeAreaL <- a*b*pi
   
    ## Right eye area
    a <- abs(data$x[2] - data$x[24])
    b <- abs(data$y[2] - data$y[26])
    eyeAreaR <- a*b*pi
    
    ## Brow height
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
    
    ##Facial width-to-height ratio
    fwhr <- with(data,
                 abs(max(x[114],x[113],x[115])-min(x[112],x[112],x[120]))/abs(y[91]-min(y[21],y[26])))
    
    ## Space between eyes
    eyespace <- with(data,
                     sqrt(((x[1]-x[2])^2) + ((y[1]-y[2])^2))
    )
    
    ## Left eye area
    a <- abs(data$x[1] - data$x[23])
    b <- abs(data$y[1] - data$y[21])
    eyeAreaL <- a*b*pi
    
    ## Right eye area
    a <- abs(data$x[2] - data$x[24])
    b <- abs(data$y[2] - data$y[26])
    eyeAreaR <- a*b*pi
    
    ## Brow height
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
  
  return(list(image = tem, fwhr=fwhr, eye_spacing=eyespace, right_eyeArea=eyeAreaR,
              left_eyeArea=eyeAreaL, min_brow = min_brow, max_brow = max_brow))
  
}