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
#star_km$size
#star_km$centers
#star_km$tot.withinss
#table(factor(star_km$cluster), factor(unlist(star_names)))


#length(star_names)
star_km$cluster
table(factor(star_km$cluster))
hist(star_km$cluster)




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