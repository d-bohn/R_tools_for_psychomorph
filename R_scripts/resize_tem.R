resize_tem <- function(file, xpre, ypre, xpost, ypost){
  
  message('This function assumes equal pixels taken off of width and height')
  
  npoints <- readLines(file, 1)
  df <- read.delim(file, skip = 1, header = FALSE, sep = ' ')
  data <- df[1:npoints,]
  
  data$V1 <- as.numeric(as.character(data$V1))
  data$V2 <- as.numeric(as.character(data$V2))
  
  if(xpost != xpre){
    changex <- (xpre-xpost)/2
    
    data$xnew <- data$V1-changex
    
  } else data$xnew <- data$V1
  
  if(ypost != ypre){
    changey <- (ypre-ypost)/2
    
    data$ynew <- data$V2-changey
  } else data$ynew <- data$V2
  
  new_pts <- data[c('xnew','ynew')]
  names(new_pts) <- c('V1','V2')
  
  nnpoints <- as.numeric(npoints)+1
  df_new <- rbind(new_pts,df[nnpoints:nrow(df),])
  df_new_new <- rbind(data.frame(V1=npoints,V2=NA), df_new)
  
  df_new_new$V1 <- as.numeric(as.character(df_new_new$V1))
  df_new_new$V2 <- as.numeric(as.character(df_new_new$V2))
  
  write.table(df_new_new, paste0('new_',file), sep = ' ', col.names = FALSE, row.names = FALSE, na = '')
}