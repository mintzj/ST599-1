#   4/10
#   First attempt at using Git and Exploring the Data

#'  Install package 'foreign' to read the data into R.
#'  The function we need is called read.arff()

#install.packages('foreign')
library(foreign)

#'  dplyr package provides some utility funcions for sorting data
#'  (According to charlotte it is faster for large data)
install.packages('dplyr')
library(dplyr)

stars <- read.arff(file = 'Z:/ST599/Project 1/code/data/AstronomyTrainingData.txt')

#  identify the 25 levels of star: (Column 87 holds the names.)
levels(stars$class)
barplot(summary(stars$class), horiz = TRUE, las = 1, cex.names = 0.5, oma=c(2,2,2,2))

#  Put names on the graph bars to identify
#text(x = 25, y = 1.19*1:25-0.25, labels = names(summary(stars[ ,87])))

# Lets try some dplyr functions
# Identify all the classical cepheids
cceph <- filter(stars, class == "Classical Cepheid")
#  What is the mean aplitude of a classical cepheid?
summarise(cceph, av_amplitude = mean(amplitude, na.rm = T))

#  Identify all the Beta Lyrae
beta_lyrae <- filter(stars, class == "Beta Lyrae")
#  What is the mean amplitude of Beta Lyrae?
summarise(beta_lyrae, bl_av_amplitude = mean(amplitude, na.rm = T))

#  Color is stored in variables 5-9
boxplot(beta_lyrae[ ,5:9])
boxplot(cceph[ ,5:9])
