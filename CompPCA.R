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

summary(pca.1)
pca.1.latent.sem <- pca.1$rotation
sort(pca.1.latent.sem[,1],decreasing=TRUE)[1:30]

# What if we want to see how often certain variables explain variation?
# We do counts instead:

pca.1.count <- c(names(abs(sort(pca.1$rotation[,1],decreasing=TRUE))[1:30]),
names(abs(sort(pca.1$rotation[,2],decreasing=TRUE))[1:30]),
names(abs(sort(pca.1$rotation[,3],decreasing=TRUE))[1:30]),
names(abs(sort(pca.1$rotation[,4],decreasing=TRUE))[1:30]),
names(abs(sort(pca.1$rotation[,5],decreasing=TRUE))[1:30]))

# To see what our work has found:
sort(table(pca.1.count),decreasing=TRUE)

# Now we create counts for the remaining 24 classes:
# (note: there is surely an easier way to do this,
# but assign() was not cooperating here, and i decided 
# getting teammates the data was more important than elegance)
pca.2.count <- c(names(abs(sort(pca.2$rotation[,1],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.2$rotation[,2],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.2$rotation[,3],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.2$rotation[,4],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.2$rotation[,5],decreasing=TRUE))[1:30]))

pca.3.count <- c(names(abs(sort(pca.3$rotation[,1],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.3$rotation[,2],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.3$rotation[,3],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.3$rotation[,4],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.3$rotation[,5],decreasing=TRUE))[1:30]))

pca.4.count <- c(names(abs(sort(pca.4$rotation[,1],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.4$rotation[,2],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.4$rotation[,3],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.4$rotation[,4],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.4$rotation[,5],decreasing=TRUE))[1:30]))

pca.5.count <- c(names(abs(sort(pca.5$rotation[,1],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.5$rotation[,2],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.5$rotation[,3],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.5$rotation[,4],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.5$rotation[,5],decreasing=TRUE))[1:30]))

pca.6.count <- c(names(abs(sort(pca.6$rotation[,1],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.6$rotation[,2],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.6$rotation[,3],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.6$rotation[,4],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.6$rotation[,5],decreasing=TRUE))[1:30]))

pca.7.count <- c(names(abs(sort(pca.7$rotation[,1],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.7$rotation[,2],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.7$rotation[,3],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.7$rotation[,4],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.7$rotation[,5],decreasing=TRUE))[1:30]))

pca.8.count <- c(names(abs(sort(pca.8$rotation[,1],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.8$rotation[,2],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.8$rotation[,3],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.8$rotation[,4],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.8$rotation[,5],decreasing=TRUE))[1:30]))

pca.9.count <- c(names(abs(sort(pca.9$rotation[,1],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.9$rotation[,2],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.9$rotation[,3],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.9$rotation[,4],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.9$rotation[,5],decreasing=TRUE))[1:30]))

pca.10.count <- c(names(abs(sort(pca.10$rotation[,1],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.10$rotation[,2],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.10$rotation[,3],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.10$rotation[,4],decreasing=TRUE))[1:30]),
                 names(abs(sort(pca.10$rotation[,5],decreasing=TRUE))[1:30]))

pca.11.count <- c(names(abs(sort(pca.11$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.11$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.11$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.11$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.11$rotation[,5],decreasing=TRUE))[1:30]))

pca.12.count <- c(names(abs(sort(pca.12$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.12$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.12$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.12$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.12$rotation[,5],decreasing=TRUE))[1:30]))

pca.13.count <- c(names(abs(sort(pca.13$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.13$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.13$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.13$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.13$rotation[,5],decreasing=TRUE))[1:30]))

pca.14.count <- c(names(abs(sort(pca.14$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.14$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.14$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.14$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.14$rotation[,5],decreasing=TRUE))[1:30]))

pca.15.count <- c(names(abs(sort(pca.15$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.15$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.15$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.15$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.15$rotation[,5],decreasing=TRUE))[1:30]))

pca.16.count <- c(names(abs(sort(pca.16$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.16$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.16$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.16$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.16$rotation[,5],decreasing=TRUE))[1:30]))

pca.17.count <- c(names(abs(sort(pca.17$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.17$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.17$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.17$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.17$rotation[,5],decreasing=TRUE))[1:30]))

pca.18.count <- c(names(abs(sort(pca.18$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.18$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.18$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.18$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.18$rotation[,5],decreasing=TRUE))[1:30]))

pca.19.count <- c(names(abs(sort(pca.19$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.19$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.19$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.19$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.19$rotation[,5],decreasing=TRUE))[1:30]))

# PCA 20 does not have five rotations; I would assume this is due to only having one point.
# We comment PCA 20 while working with NAs removed, as all loadouts are 0:

# pca.20.count <- c(names(abs(sort(pca.20$rotation[,1],decreasing=TRUE))[1:30]),
#                   names(abs(sort(pca.20$rotation[,2],decreasing=TRUE))[1:30]),
#                   names(abs(sort(pca.20$rotation[,3],decreasing=TRUE))[1:30]),
#                   names(abs(sort(pca.20$rotation[,4],decreasing=TRUE))[1:30]),
#                   names(abs(sort(pca.20$rotation[,5],decreasing=TRUE))[1:30]))

pca.21.count <- c(names(abs(sort(pca.21$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.21$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.21$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.21$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.21$rotation[,5],decreasing=TRUE))[1:30]))

pca.22.count <- c(names(abs(sort(pca.22$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.22$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.22$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.22$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.22$rotation[,5],decreasing=TRUE))[1:30]))

pca.23.count <- c(names(abs(sort(pca.23$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.23$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.23$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.23$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.23$rotation[,5],decreasing=TRUE))[1:30]))

pca.24.count <- c(names(abs(sort(pca.24$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.24$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.24$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.24$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.24$rotation[,5],decreasing=TRUE))[1:30]))

pca.25.count <- c(names(abs(sort(pca.25$rotation[,1],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.25$rotation[,2],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.25$rotation[,3],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.25$rotation[,4],decreasing=TRUE))[1:30]),
                  names(abs(sort(pca.25$rotation[,5],decreasing=TRUE))[1:30]))

# Once again, we remove PCA 20's counts:
pca.counts <- c(pca.1.count, pca.2.count, pca.3.count, pca.4.count, pca.5.count,
                pca.6.count, pca.7.count, pca.8.count, pca.9.count, pca.10.count,
                pca.11.count,pca.12.count,pca.13.count,pca.14.count,pca.15.count,
                pca.16.count,pca.17.count,pca.18.count,pca.19.count,
                pca.21.count,pca.22.count,pca.23.count,pca.24.count,pca.25.count)

# And see our results:
sort(table(pca.counts),decreasing=TRUE)