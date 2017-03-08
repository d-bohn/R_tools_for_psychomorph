# psychomorphR: R tools for psychomorph
R Scripts for obtaining various physical measurements from JPsychomorph's delineated points (templates)

Finding physical measurements from a face template is simple and easy, so long as the measurment you are looking for has been coded.
Presently, these measurements are supported:

- `tem_to_eyeArea()` (left and right eye area)
- `tem_to_browh()` (min and max brow height)
- `tem_to_fwhr()` (facial width-to-height ratio)
- `tem_to_eyespace()` (eye spacing/distance apart)

If you would like to see more measurements added, please submit an issue, or clone this repository and create it yourself (don't forget
to submit your additions!).

For batch obtaining all physical measurements (`tem_to_all()`) for a whole folder of face templates, utilize the following code:

````
source('R_scripts/tem_to_all.R')
require(tidyverse)

files <- list.files(path = 'path/to/template/folder', pattern = '.tem', full.names = TRUE)

list <- lapply(files, tem_to_all)

# Change list into dataframe
data <- do.call(rbind, lapply(list, data.frame)) %>%
  select(., image, fwhr, eye_spacing, right_eyeArea, left_eyeArea,
         min_brow, max_brow)
````
