#  4/14
#
# K means analysis on Star data ---------------------------

#install.packages('foreign')
library(foreign)  # to read the data we need function read.arff() from library foreign.

#install.packages('dplyr')
library(dplyr)  #'  dplyr package provides some utility funcions for sorting data

stars <- read.arff(file = 'AstronomyTrainingData.txt')
#Filter stars for complete entries.
complete_stars <- stars %>% filter(complete.cases(.)) 
star_names <- levels(stars$class)

#  identify the 25 levels of star: (Column 87 holds the names.)
levels(stars$class)
barplot(summary(stars$class), horiz = TRUE, las = 1, cex.names = 0.5)

#  We could specify that there are 25 centers, and kmeans will attempt to 
#  locate 25 good centers, but it may be terrible!
centers <- 25

star_km <- kmeans(complete_stars[c(-1,-dim(complete_stars)[2])], centers, iter.max = 20, nstart = 1)
found_centers <- star_km$centers

#  There are 25 centers, one for each cluster.  The center contains 85 means representing each variable.
#  Cluster membership is identified by a a number.
# View(star_km$cluster)

#Numbers of stars per cluster
table(factor(star_km$cluster))
plot(table(factor(star_km$cluster)),xlab = "Cluster Number", ylab = "Stars per Cluster")




# I can make one of these by:  
#  1)  Sorting the data by star type
#  2)  taking the mean for each star type, for each variable.

# with dplyr:  group COMPLETE stars by class, then summarise with the mean across all variables.
#sum_stars <- complete_stars %>% group_by(class) %>% summarise_each(funs(mean))

# with dplyr:  group each star by class, then summarise all the variables with mean(x, rm.na)
sum_stars <- stars %>% group_by(class) %>% summarise_each(funs(mean(., na.rm=TRUE)))

#star_km$size
#star_km$centers
#star_km$tot.withinss
#table(factor(star_km$cluster), factor(unlist(star_names)))

#full_star_km  <-  kmeans(complete_stars[c(-1,-dim(complete_stars)[2])], centers = sum_stars, 
 #                        iter.max = 20, nstart = 1)

# Setting our own centers ---------------------------
# We had to switch to "Lloyd" method, as "Hartigan-Wong" cannot handle size 0 clusters.
full_star_km  <-  kmeans(as.matrix(complete_stars[,c(-1,-87)]), 
                         centers = as.matrix(sum_stars[,c(-1,-2)]), iter.max = 20,
                         algorithm = "Lloyd")

# Result of clustering with 25 random clusters
plot(table(star_km$cluster))
table(star_km$cluster)
# Result of clustering with 25 nonrandom clusters drawn from training.
table(full_star_km$cluster) 
plot(table(full_star_km$cluster))
plot(table(stars$class))
barplot(summary(stars$class), horiz = TRUE, las = 1, cex.names = 0.5)



#algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)
View(as.matrix(sum_stars[1:10,c(-1,-2)]))


is.data.frame(complete_stars)
View(sum_stars)
?kmeans

View(sum_stars[,c(-1,-2,-87)])
dim(sum_stars[,c(-1,-2)])
dim(stars[,c(-1,-87)])


# An example of setting the centers ---------------------------
#  Available here:  http://stats.stackexchange.com/questions/72606/k-means-clustering-after-first-iteration

# Set the RNG seed to ensure reproducibility
set.seed(12345)

# Let's create 3 visually distinct clusters
n <- c(1000, 500, 850)
# Jeff: X var defined here for 3 clusters
classifier.1 <- c(rnorm(n[1], 10, 0.9), 
                  rnorm(n[2], 25, 2),
                  rnorm(n[3], 35, 2))
# Jeff:  Y var defined here for 3 clusters.
classifier.2 <- c(rnorm(n[1], 5, 1),
                  rnorm(n[2], 10, 0.4),
                  rnorm(n[3], 2, .9))

col = c("blue", "darkgreen", "darkred")
# Run k-means with 3 clusters and random initial centroids 
# to check the clusters are correctly recognized
km <- kmeans(cbind(classifier.1, classifier.2), 3)
# Plot the data, colored by cluster
plot(classifier.1, classifier.2, pch=20, col=col[km$cluster])

# Mark the final centroids
points(km$centers, pch=20, cex=2, col="orange")

# Now impose some obviously "wrong" starting centroids
start.x <- c(10, 25, 3000)
start.y <- c(10, 10, -10000)
km.2 <- kmeans(cbind(classifier.1, classifier.2), 
               centers=cbind(start.x, start.y))

# Jeff:  How will this preform if we give it the right stuff?
start.x <- c(10, 25, 35)
# It still finds it even if we give pretty bad y values!
start.y <- c(0, 1, 3)
km.2 <- kmeans(cbind(classifier.1, classifier.2), 
               centers=cbind(start.x, start.y))
plot(classifier.1, classifier.2, pch=20, col=col[km.2$cluster])
points(km.2$centers, pch=20, cex=2, col="orange")

View(cbind(start.x, start.y))
?kmeans

# Top 30 Vars. ---------------------------

#Select the top n variables from PCA to use.
n <- 20
n_star_names <- as.character( pca_out[1:n,1] )
#View(stars[as.character( pca_out[1:n,1] ) ])


#Sort out the complete stars.
complete_stars <- stars %>% filter(complete.cases(.)) 

# Create a sorted list of training stars, by type.
sum_stars <- stars %>% group_by(class) %>% summarise_each(funs(mean(., na.rm=TRUE)))

#


top_var_star_km  <-  kmeans(as.matrix(complete_stars[,n_star_names]), 
                         centers = as.matrix(sum_stars[,n_star_names]), iter.max = 200,
                         algorithm = "Lloyd")

#View(top_var_star_km$cluster)
# Result of clustering using the best n variables, with means of clusters drawn from training.
#table(top_var_star_km$cluster) 
plot(table(top_var_star_km$cluster))
plot(table(stars$class))


barplot(table(top_var_star_km$cluster))
barplot(table(stars$class), las = 1, cex.names = 0.5, )
barplot(table(complete_stars$class), las = 1, cex.names = 0.5, )


barplot(cbind(table(top_var_star_km$cluster), table(stars$class)), las = 1, cex.names = 0.5, beside = T, )
#barplot(summary(stars$class), horiz = TRUE, las = 1, cex.names = 0.5)

results <- as.data.frame( cbind(names = star_names, predicted = table(top_var_star_km$cluster), actual = table(stars$class)))
View(results)
barplot(as.numeric(results$actual), horiz = T, las = 1, names.arg = results$names, cex.names= 0.5, main = "Actual")
barplot(as.numeric(results$predicted), horiz = T, las = 1, names.arg = results$names, cex.names= 0.5, main = "Predicted")
barplot(summary(stars$class), horiz = TRUE, las = 1, cex.names = 0.5, main = "Training Stars")
barplot(summary(complete_stars$class), horiz = TRUE, las = 1, cex.names = 0.5, main = "Complete Stars")





# Working with the whole data ---------------------------
# The location of this file will vary for you.  I didn't upload it to github, since it is so large.
# I suppose we could upload it if we want.

all_stars <- read.arff(file = 'Z:/ST599/Project 1/data/AstronomyTestData.txt')









# Multicolinearity Analysis ---------------------------

#Is there multicolinearity?

# install.packages('faraway')
library(faraway)
#Calculate the variance inflation factors (a measure of multicolinearity)
#  (VIFS are a ratio of eigenvalues to the smallest eigenvalue)
complete_stars_noname <- complete_stars[c(-1,-dim(complete_stars)[2])]
vifs <- (vif(complete_stars_noname))
vifs
plot(vifs)
#identify(vifs)
identify(vifs, labels = names(vifs), atpen = T)
identify(vifs, labels = names(vifs))
identify(vifs)
#  Colinearity Suspects in order::  
#  "eclpoly_p_pulse", "eclpoly_p_pulse_initial", "std" "stetson_j", "amplitude"


View(complete_stars)
sorted_vifcolumns <- arrange(complete_stars_noname[,c(1,18,19,83,84)],amplitude)
View(complete_stars_noname[,c(1,18,19,83,84)])

e <- eigen(t(as.matrix(complete_stars_noname))%*%as.matrix(complete_stars_noname))
signif(e$values,3)
#  There is at least one enormous colinearity. 
#  It was at least 2.0e20 before correction.
var_names <-  names(complete_stars)
sort(var_names)


#  Correcting for variance inflation:
# Remove ID, Remove Name
star_low_vif <- complete_stars[c(-1,-dim(complete_stars)[2])]
star_low_vif <- star_low_vif[c(-18,-19)]

e <- eigen(t(as.matrix(star_low_vif))%*%as.matrix(star_low_vif))
signif(e$values,3)

low_vif <- (vif(star_low_vif))
plot(low_vif)
identify(low_vif)
View(star_low_vif[,c(1,81,82)])






# Sharm's Example ---------------------------

#cancerData<-read.table(file="14cancer.xtrain",sep="")
# namesCancer<-read.table(file="namesCancer.txt",sep="",header="FALSE")
#namesCancer<-read.table(file="namesCancer.txt")
#myLabels<-c('breast','prostate','lung','collorectal','lymphoma','bladder','melanoma','uterus','leukemia','renal','pancreas','ovary','meso','cns')
#names(cancerData)<-c(namesCancer)


#dim(cancerData)
#myLabels<-c('breast','prostate','lung','collorectal','lymphoma','bladder','melanoma','uterus','leukemia','renal','pancreas','ovary','meso','cns')

#digit = read.table('Digit_recog_train.txt')
#digit_no = digit[,1]
#digit_pix = digit[,-1]
#digit_no3 = digit_no[digit_no == 3]
#digit_pix3 = digit_pix[digit_no == 3,]

#sel = sample(c(0,1), length(digit_no), replace = T, prob = c(4/5,1/5))
#digit_pix = digit_pix[sel == 1,]
#digit_no = digit_no[sel==1]

#  Example, part 2

#numCenters<-5
#kmeansCancer<-kmeans(t(cancerData), numCenters, iter.max = 20, nstart = 1)

#kmeansCancer$size
#kmeansCancer$centers
#kmeansCancer$tot.withinss
#table(factor(kmeansCancer$cluster), factor(unlist(namesCancer)))
#myLabels

#numCenters = 10
#kmeansDigit = kmeans(digit_pix, numCenters, iter.max = 20, nstart = 1)
#pamDigit = pam(digit_pix, k = numCenters, metric = 'Euclidean')

#kmeansDigit$size
#kmeansDigit$centers
#kmeansDigit$tot.withinss
#table(factor(kmeansDigit$cluster), factor(digit_no))
#pamDigit$medoids
#pamDigit$objective
#table(factor(pamDigit$clustering), factor(digit_no))

#  ---------------------------