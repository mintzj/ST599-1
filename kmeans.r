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

?kmeans
#numCenters<-5
#kmeansCancer<-kmeans(t(cancerData), numCenters, iter.max = 20, nstart = 1)

#  I made 25 centers, there's no reason there need to be 25, it could be any number.
centers <- 25


star_km <- kmeans(complete_stars[c(-1,-dim(complete_stars)[2])], centers, iter.max = 20, nstart = 1)
summary(star_km)
str(star_km)
found_centers <- star_km$centers
#  There are 25 centers, one for each cluster.  The center contains 85 means representing each variable.

# I can make one of these by:  
#  1)  Sorting the data by star type
#  2)  taking the mean for each star type, for each variable.
#  mapply?
grouped_complete_stars <- group_by(complete_stars, class)
sum_stars <- summarise(grouped_complete_stars, amplitude = mean(amplitude))
#sum_stars <- complete_stars %>% group_by(class) %>% summarise_each(funs(mean))
sum_stars <- stars %>% group_by(class) %>% summarise_each(funs(mean(., na.rm=TRUE)))

summarise(grouped_complete_stars, amplitude = mean(amplitude))
df %>% group_by(grp) %>% summarise_each(funs(mean))
#df %>% group_by(grp) %>% summarise_each(funs(mean))

#star_km$size
#star_km$centers
#star_km$tot.withinss
#table(factor(star_km$cluster), factor(unlist(star_names)))


#length(star_names)
star_km$cluster
table(factor(star_km$cluster))
hist(star_km$cluster, 25)

#6 clusters:
centers <- 10
star_km <- kmeans(complete_stars[c(-1,-dim(complete_stars)[2])], centers, iter.max = 20, nstart = 1)
#star_km$cluster
#table(factor(star_km$cluster))
hist(star_km$cluster, centers)

#Is there multicolinearity?
install.packages('faraway')
library(faraway)
#Calculate the variance inflation factors (a measure of multicolinearity)
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