library(foreign)
library(dplyr)

stars <- read.arff(file = 'AstronomyTrainingData.txt')

# goal: run PCA on 25 unique star classes
# modifying jeff's code:

starcom <- stars %>% filter(complete.cases(.))
starcom.std <- starcom
for (i in c(2:32,34:41,43:50,52:86)) {
  starcom.std[,i] <- (starcom[,i] - mean(starcom[,i])) / sd(starcom[,i])
}
starcom.std2 <- starcom.std %>% filter(complete.cases(.))

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

summary(pca.9)
pca.9.latent.sem <- pca.9$rotation
sort(pca.9.latent.sem[,1],decreasing=TRUE)[1:30]

signif(sort(pca.9.latent.sem[,1],decreasing=TRUE)[1:30],2)
signif(sort(pca.9.latent.sem[,1],decreasing=FALSE)[1:30],2)

signif(sort(pca.9.latent.sem[,2],decreasing=TRUE)[1:30],2)
signif(sort(pca.9.latent.sem[,2],decreasing=FALSE)[1:30],2)
