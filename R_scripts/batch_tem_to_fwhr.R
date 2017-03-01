source('R_scripts/tem_to_all.R')

files <- list.files(path = 'C:/Users/dalbohn/Desktop/neutral/', pattern = '.tem', full.names = TRUE)

list <- lapply(files, tem_to_all)

data <- do.call(rbind, lapply(list, data.frame)) %>%
  select(., image, fwhr, eye_spacing, right_eyeArea, left_eyeArea,
         min_brow, max_brow)

write.table(data, 'data/mat_phys_measures.csv', row.names = FALSE, sep = ',')
