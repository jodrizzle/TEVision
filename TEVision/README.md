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
 
Notes:
 - image source files should be in the /data directory.  Call the program from the /src directory.
 - edge detection works slightly better if a grayscale image is used.
 - blur region size can be made very big for good quality photos.  Has to be odd number.

TODO:
 - Draw contours
 - Threshold
 - Crop 
 - 4 point transform
 