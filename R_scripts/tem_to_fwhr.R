# Take a .tem with 179 or 189 points and convert [x,y]
# coordinates to fWH ratio.

## FOR DEBUGGING ##
tem <- 'data/3dsk_female_avg_convert.tem'
tem <- 'data/MN1.tem'
tem <- files[4]
## END DEBUGGING ##

tem_to_fwhr <- function(tem) {
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
    
    fwhr <- with(data,
                 abs(max(x[114],x[113],x[115])-min(x[112],x[112],x[120]))/abs(y[91]-min(y[21],y[26]))
    )
    
  } else if (length(df) == 2) {
    data <- df
    names(data) <- c('x','y')
    data <- data %>%
      mutate(., row = seq(0, rows)) %>% 
      filter(., is.na(y) == FALSE)
    data$x <- as.numeric(as.character(data$x))
    data$y <- as.numeric(as.character(data$y))
    
    fwhr <- with(data,
                 abs(max(x[114],x[113],x[115])-min(x[112],x[112],x[120]))/abs(y[91]-min(y[21],y[26]))
    )
    
  } else {
    print("This template is unrecognized. Do you have the correct formatting?")
  
    }
  
  return(list(image = tem, fwhr = fwhr))
  
}

tem_to_eyespace <- function(tem) {
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
    
    eyespace <- with(data,
                     sqrt(((x[1]-x[2])^2) + ((y[1]-y[2])^2))
    )
    
  } else if (length(df) == 2) {
    data <- df
    names(data) <- c('x','y')
    data <- data %>% mutate(., row = seq(0, rows)) %>% 
      filter(., is.na(y) == FALSE)
    data$x <- as.numeric(as.character(data$x))
    data$y <- as.numeric(as.character(data$y))
    
    eyespace <- with(data,
                     sqrt(((x[1]-x[2])^2) + ((y[1]-y[2])^2))
    )
    
  } else {
    print("This template is unrecognized. Do you have the correct formatting?")
    
  }
  
  return(list(image = tem, eye_spacing = eyespace))
  
}