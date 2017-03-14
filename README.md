# psychomorphR: R tools for psychomorph
R Scripts for obtaining various physical measurements from JPsychomorph's delineated points (templates)

Finding physical measurements from a face template is simple and easy, so long as the measurement you are looking for has been coded.
Presently, these measurements are supported:

- `tem_to_eyeArea(tem)` (left and right eye area)
    - Note that this measurement is highly dependent on the size of the original image templated. If you have different
    size images you will want to scale the values before analyzing.
- `tem_to_browh(tem)` (min and max brow height)
- `tem_to_fwhr(tem)` (facial width-to-height ratio)
- `tem_to_eyespace(tem)` (eye spacing/distance apart)
- `sclera(tem, threshold=0.0011)` (amount of sclera present above/below the iris of each eye)
    - Returns four values (top/bottom, left/right) estimating the amount of sclera present for each eye scaled by
    how large the distance is in pixels between the center of the pupil and the top/bottom of the eye.

If you would like to see more measurements added, please submit an issue, or clone this repository and create it yourself (don't forget
to submit your additions!).

For batch obtaining all physical measurements (`tem_to_all()`) for a whole folder of face templates, utilize the following code:

```
source('R_scripts/tem_to_all.R')
require(tidyverse)

files <- list.files(path = 'path/to/template/folder', pattern = '.tem', full.names = TRUE)

list <- lapply(files, tem_to_all)

# Change list into dataframe
data <- do.call(rbind, lapply(list, data.frame)) %>%
  select(., image, fwhr, eye_spacing, right_eyeArea, left_eyeArea,
         min_brow, max_brow)
```

Depending on the size and content of your template files, some of these functions might throw a warning message for 'Too many/few
values at X locations.' I assure you that if your template has 179 or 189 points, these warnings don't matter. We are effectively
separating each template file by (x,y) coordinates for each point. There is extra information at the end of each template file
(e.g., line information) that is unneeded, and thus discarded.