Author:Johan Jansen van Vuuren

Version:0.1

Date:21/02/2016

Usage:
 - ./TEVision

Features:
 - Cycles through all image files in /data
 - Load images (grayscale,color)
 - Canny edge detection
 - Blurring prior to canny edge detection
 - Find contour points
 - Draw contours on original image
 - Apply perspective transform to largest object found
 - Threshold
 - Write output to files
 
Notes:
 - image source files should be in the /data directory.  Call the program from the /bin directory.
 - the program assumes there's only one document in an input image.  It will reliably find the largest document
 - the output directory is /data/Output 
