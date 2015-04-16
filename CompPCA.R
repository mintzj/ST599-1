library(foreign)
library(dplyr)

stars <- read.arff(file = 'AstronomyTrainingData.txt')

# goal: run PCA on 25 unique star classes
# modifying jeff's code:

starcom <- stars %>% filter(complete.cases(.))
starcom.std <- starcom
for (i in (2:86)) {
  starcom.std[,i] <- (starcom[,i] - mean(starcom[,i])) / sd(starcom[,i])
}

# a weird note: some star classes are dramatically reduced
# by removing all NAs. in particular, class 25 goes from
# 57 entries to just 1 (making PCA impossible in theory).
# pressing on:

j=1; for (i in unique(stars$class)) {
  assign(paste("pca.", j, sep=""),
         prcomp(filter(starcom.std, class==i)[,-c(1,87)]))
  j <- j+1
}

# this creates 25 "pca.#" lists. we can analyze any one individually.
# replace each "(I)" with the desired class number:

pca.9.latent.sem <- pca.9$rotation
sort(pca.9.latent.sem[,1],decreasing=FALSE)[1:30]
