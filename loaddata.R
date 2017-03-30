#================================================================================================|
# Author - Vikas Kukreti                                                                         |   
# This file Will load data for Retail Dataset                 |   
# into a R dataframe                                                                             |
#                                                                                                |       
#                                                                                                |     
#================================================================================================|
rm("data")
#Set the path and working directory
dpath <- "/home/vikask/r/cluster/data"
setwd(dpath)
#Load data into the matrix
data <- read.csv("data.csv",header=T)

ppath <- "/home/vikask/r/pack"
setwd(ppath)

path <- "/home/vikask/r/cluster"
setwd(path)










