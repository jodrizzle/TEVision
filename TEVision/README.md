Author:Johan Jansen van Vuuren
Version:0.1
Date:07/02/2016

Usage:
 - stack exec TEVision <filename.extension> <blur_region_size>

Features:
 - Load images (grayscale,color)
 - Canny edge detection
 - Blurring prior to canny edge detection
 - Find contour points
 - Draw contour on original image
 - Find approximate corners of document and indicate on original
 
Notes:
 - image source files should be in the /data directory.  Call the program from the /src directory.

TODO:
 - 4 point transform
 - Threshold
 
