Author:Johan Jansen van Vuuren
Version:0.1
Date:07/02/2016

Image source files should be in the "data/" directory.

Usage:
 - stack exec TEVision <filename.extension> <blur_region_size>

Features:
 - Load images (grayscale,color)
 - Canny edge detection
 - Blurring prior to canny edgeing
 
Notes:
 - edge detection works slightly better if a grayscale image is used.
 - blur region size can be made very big for good quality photos.  Has to be odd number.
