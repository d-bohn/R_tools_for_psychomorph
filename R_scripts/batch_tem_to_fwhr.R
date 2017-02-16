source('R_scripts/tem_to_fwhr.R')

files <- list.files(path = 'C:/Users/dalbohn/Desktop/neutral/', pattern = '.tem', full.names = TRUE)

list <- lapply(files, tem_to_fwhr)

data <- do.call(rbind, lapply(list, data.frame)) %>%
  select(., image, fwhr) %>%
  distinct(., image, fwhr)

write.table(data, 'data/nimh_mat_faces.csv', row.names = FALSE, sep = ',')
