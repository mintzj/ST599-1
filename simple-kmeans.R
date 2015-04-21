#  4/14
#
# Only the essential components for Kmeans Analysis ---------------------------

#install.packages('foreign')
library(foreign)  # to read the data we need function read.arff() from library foreign.

#install.packages('dplyr')
library(dplyr)  #'  dplyr package provides some utility funcions for sorting data

stars <- read.arff(file = 'AstronomyTrainingData.txt')
#Filter stars for complete entries.
complete_stars <- stars %>% filter(complete.cases(.)) 
star_names <- levels(stars$class)

#  We could specify that there are 25 centers, and kmeans will attempt to 
#  locate 25 good centers, but it may be terrible!
centers <- 25

star_km <- kmeans(complete_stars[c(-1,-dim(complete_stars)[2])], centers, iter.max = 20, nstart = 1)
found_centers <- star_km$centers

#  There are 25 centers, one for each cluster.  The center contains 85 means representing each variable.
#  Cluster membership is identified by a a number.
# View(star_km$cluster)

#Numbers of stars per cluster
barplot(table(star_km$cluster), horiz = TRUE, las = 1, cex.names = 0.5, names.arg = levels(stars$class), main = "Random Center Clusters")
barplot(summary(complete_stars$class), horiz = TRUE, las = 1, cex.names = 0.5, main = "Hand Coded Clusters")


# I can make one of these by:  
#  1)  Sorting the data by star type
#  2)  taking the mean for each star type, for each variable.

# with dplyr:  group each star by class, then summarise all the variables with mean(x, rm.na)
sum_stars <- stars %>% group_by(class) %>% summarise_each(funs(mean(., na.rm=TRUE)))

star_km <-  kmeans(as.matrix(complete_stars[,c(-1,-87)]), 
                            centers = as.matrix(sum_stars[,c(-1,-2)]), iter.max = 200,
                            algorithm = "Lloyd")

barplot(table(star_km$cluster), horiz = TRUE, las = 1, cex.names = 0.5, main = "Trained Clusters")
barplot(summary(complete_stars$class), horiz = TRUE, las = 1, cex.names = 0.5, main = "Hand Coded Clusters")



# Top n Vars. ---------------------------

#Select the top n variables from PCA to use.
n <- 70
n_star_names <- as.character( pca_out[1:n,1] )

#Sort out the complete stars.
complete_stars <- stars %>% filter(complete.cases(.)) 

# Create a sorted list of training stars, by type.
sum_stars <- stars %>% group_by(class) %>% summarise_each(funs(mean(., na.rm=TRUE)))

# K-means with the Lloyd algorithm
top_var_star_km  <-  kmeans(as.matrix(complete_stars[,n_star_names]), 
                            centers = as.matrix(sum_stars[,n_star_names]), iter.max = 200,
                            algorithm = "Lloyd")

# Result of clustering using the best n variables, with means of clusters drawn from training.
barplot(table(top_var_star_km$cluster), horiz = TRUE, las = 1, cex.names = 0.5, names.arg = levels(stars$class), main = paste("Best ",n))
barplot(summary(complete_stars$class), horiz = TRUE, las = 1, cex.names = 0.5, main = "Hand Coded Clusters")



# Working with the whole data ---------------------------
# Full data: The location of this file will vary for you. 

all_stars <- read.arff(file = 'Z:/ST599/Project 1/data/AstronomyTestData.txt')

n <- 40
n_star_names <- as.character( pca_out[1:n,1] )

#Sort out the complete stars.
complete_stars <- all_stars %>% filter(complete.cases(.)) 

# Create a sorted list of training stars, by type.
sum_stars <- stars %>% group_by(class) %>% summarise_each(funs(mean(., na.rm=TRUE)))

# K-means with the Lloyd algorithm
all_star_km  <-  kmeans(as.matrix(complete_stars[,n_star_names]), 
                            centers = as.matrix(sum_stars[,n_star_names]), iter.max = 2000,
                            algorithm = "Lloyd")

# Result of clustering using the best n variables, with means of clusters drawn from training.
barplot(table(all_star_km$cluster), horiz = TRUE, las = 1, cex.names = 0.5, names.arg = levels(stars$class), main = paste("Full Set: Best ",n))
barplot(summary(complete_stars$class), horiz = TRUE, las = 1, cex.names = 0.5, main = "Hand Coded Clusters")

