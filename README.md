Author:Johan Jansen van Vuuren

Version:1.0

Date:01/03/2016

Usage:
 - Build:  stack init, stack setup ,stack build
 - Run  :  stack exec TEVision                      (Process all image files in data/)
 - Run  :  stack exec TEVision imagepath.extension  (Process specified image file in data/)
 
Features:
 - Cycles through image files in /data
 - Canny edge detection
 - Apply perspective transform to detected documents
 - Write output to files
 
Notes:
 - image source files should be in the /data directory.  Call the program from the /src directory.
 - the output directory is /data/Output 
 
TODO:
 - performance tuning
 
