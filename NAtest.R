library(dplyr)
library(ggplot2)

dim(stars)
col.NAs <- numeric(dim(stars)[2])
for (i in 1:dim(stars)[2]) {
  col.NAs[i] <- sum(is.na(stars[,i]))
}

sum(col.NAs[4:9])
# This is all 2419 of the NAs; these columns are the culprits
names(stars)[4:9]
# It is only color; earlier examination of the PCA analysis indicated these WEREN'T
# the main source of variation. Perhaps we could analyze without these?

# We should also find which classes of stars are contributing here. Using dplyr:

stars.NA <- filter(stars, is.na(color_bv_extinction) == 1 |
         is.na(color_diff_bj) == 1 | is.na(color_diff_hk) == 1 |
         is.na(color_diff_jh) == 1 | is.na(color_diff_rj) == 1 |
         is.na(color_diff_vj) == 1)
sum(is.na(stars.NA))

# This gives 2419; this database is a collection of all NAs!)

stars.NA.sum <- data.frame(summarize(group_by(stars.NA,class), count = n()))
stars.sum <- data.frame(summarize(group_by(stars,class), count = n()))
stars.NA.sum
stars.sum
