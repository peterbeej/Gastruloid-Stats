## Gastruloid-Stats

Code written and developed by Dr Peter Baillie-Benson & Dr Naomi Moris (2020-2021).

This code is to be implemented in R. We used RStudio version 1.2.5042.

## Preparing data:
Images of gastruloids from widefield microscopy should be quantified using the Python image processing pipeline available here: https://github.com/naomi-moris/humanGastruloids_shapeDescriptors
The pipeline outputs .csv files that can be processed with this code.

## QC:
Contours from the image processing pipeline should be manually curated to exclude any that capture the background of the image or fail to contour the gastruloid.
If rows are manually removed from the .csv file, the remaining rows should be moved up to remove any whitespace.
Similarly, now empty rows at the bottom of the table should be cleared (backspace) before running through this code, to avoid the creation of an apparently empty treatment condition.

## Output:
This code returns summary tables from the statistical tests to the Console in RStudio.
