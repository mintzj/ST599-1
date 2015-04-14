#   4/13
#   
#  First of 3 analyses:  PCA  ---------------------------

#install.packages('foreign')
library(foreign)  # to read the data we need function read.arff() from library foreign.

#install.packages('dplyr')
library(dplyr)  #'  dplyr package provides some utility funcions for sorting data

stars <- read.arff(file = 'AstronomyTrainingData.txt')

#  identify the 25 levels of star: (Column 87 holds the names.)
levels(stars$class)
barplot(summary(stars$class), horiz = TRUE, las = 1, cex.names = 0.5)



#  A PCA example by Sharmodeep  ---------------------------


#load("pca-examples.Rdata") 
#nyt.pca <- prcomp(nyt.frame[,-1]) 
#nyt.latent.sem <- nyt.pca$rotation

#sort(nyt.latent.sem[,1],decreasing=TRUE)[1:30]

#signif(sort(nyt.latent.sem[,1],decreasing=TRUE)[1:30],2)
#signif(sort(nyt.latent.sem[,1],decreasing=FALSE)[1:30],2)

#signif(sort(nyt.latent.sem[,2],decreasing=TRUE)[1:30],2)
#signif(sort(nyt.latent.sem[,2],decreasing=FALSE)[1:30],2)

#plot(nyt.pca$x[,1], nyt.pca$x[,2], col = factor(nyt.frame[,1]))

##nyt.fact <- factanal(nyt.frame[,-1], factors = 2)

#  Our Star Data  ---------------------------

# Remove NA entries with dplyr
complete_stars <- stars %>% filter(complete.cases(.))
# PCA excluding the first (id number) and last columns (star type)
star_pca <- prcomp(complete_stars[,c(-1,-dim(complete_stars)[2])])
star_rotation <- star_pca$rotation
sorted_rotation <- sort(star_rotation[,1],decreasing=T)[1:30]
plot(sorted_rotation)
# Looks like one of these is very useful and the others arent?  Could others be a linear multiple of this?
#  eclpoly_best_orb_chi2



