### Step 9. Kruskal-Wallis Tests followed by Dunn Tests with Bonferroni Correction for Both Subspecific Identities and Heteroblasty ###
## Analyzes pairwise comparisons of measurements for statistical significance. ##

# Install and load packages. #
install.packages("dunn.test")

library(dunn.test)

# Read in data and subset by subspecies identity. #
data <- read.table("master_data.txt", header=TRUE)
sub_data <- data[ which(data$subspecies=='Chenopodium_oahuense_ilioense'),]
sub_data2 <- data[ which(data$subspecies=='Chenopodium_oahuense_oahuense'),]
sub_data3 <- data[ which(data$subspecies=='ILS_Hybrid'),]

## K-W Tests and Dunn Test with Bonferroni Correction for Subspecific Identity Data. ##
kruskal.test(data$LMK_PC1~data$subspecies)
kruskal.test(data$LMK_PC2~data$subspecies)
kruskal.test(data$EFD_PC1~data$subspecies)
kruskal.test(data$EFD_PC2~data$subspecies)

dunn.test(x = data$LMK_PC1, g = data$subspecies, method="bonferroni", list=TRUE)
dunn.test(x = data$LMK_PC2, g = data$subspecies, method="bonferroni", list=TRUE)
dunn.test(x = data$EFD_PC1, g = data$subspecies, method="bonferroni", list=TRUE)
dunn.test(x = data$EFD_PC2, g = data$subspecies, method="bonferroni", list=TRUE)

kruskal.test(data$Petiole_Length_mm~data$subspecies)
kruskal.test(data$Area_mm2~data$subspecies)
kruskal.test(data$AR~data$subspecies)
kruskal.test(data$Circ~data$subspecies)
kruskal.test(data$Thickness~data$subspecies)

dunn.test(x = data$Petiole_Length_mm, g = data$subspecies, method="bonferroni", list=TRUE)
dunn.test(x = data$Area_mm2, g = data$subspecies, method="bonferroni", list=TRUE)
dunn.test(x = data$AR, g = data$subspecies, method="bonferroni", list=TRUE)
dunn.test(x = data$Circ, g = data$subspecies, method="bonferroni", list=TRUE)
dunn.test(x = data$Thickness, g = data$subspecies, method="bonferroni", list=TRUE)

## K-W Tests and Dunn Test with Bonferroni Correction for Heteroblastic Data separated by Subspecies Identity. ##
## Chenopodium oahuense ssp. ilioense ##

kruskal.test(sub_data$LMK_PC1~sub_data$Week)
kruskal.test(sub_data$LMK_PC2~sub_data$Week)
kruskal.test(sub_data$EFD_PC1~sub_data$Week)
kruskal.test(sub_data$EFD_PC2~sub_data$Week)

dunn.test(x = sub_data$LMK_PC1, g = sub_data$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data$LMK_PC2, g = sub_data$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data$EFD_PC1, g = sub_data$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data$EFD_PC2, g = sub_data$Week, method="bonferroni", list=TRUE)

kruskal.test(sub_data$Petiole_Length_mm~sub_data$Week)
kruskal.test(sub_data$Area_mm2~sub_data$Week)
kruskal.test(sub_data$AR~sub_data$Week)
kruskal.test(sub_data$Circ~sub_data$Week)
kruskal.test(sub_data$Thickness~sub_data$Week)

dunn.test(x = sub_data$Petiole_Length_mm, g = sub_data$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data$Area_mm2, g = sub_data$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data$AR, g = sub_data$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data$Circ, g = sub_data$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data$Thickness, g = sub_data$Week, method="bonferroni", list=TRUE)

## Chenopodium oahuense ssp. oahuense ##

kruskal.test(sub_data2$LMK_PC1~sub_data2$Week)
kruskal.test(sub_data2$LMK_PC2~sub_data2$Week)
kruskal.test(sub_data2$EFD_PC1~sub_data2$Week)
kruskal.test(sub_data2$EFD_PC2~sub_data2$Week)

dunn.test(x = sub_data2$LMK_PC1, g = sub_data2$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data2$LMK_PC2, g = sub_data2$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data2$EFD_PC1, g = sub_data2$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data2$EFD_PC2, g = sub_data2$Week, method="bonferroni", list=TRUE)

kruskal.test(sub_data2$Petiole_Length_mm~sub_data2$Week)
kruskal.test(sub_data2$Area_mm2~sub_data2$Week)
kruskal.test(sub_data2$AR~sub_data2$Week)
kruskal.test(sub_data2$Circ~sub_data2$Week)
kruskal.test(sub_data2$Thickness~sub_data2$Week)

dunn.test(x = sub_data2$Petiole_Length_mm, g = sub_data2$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data2$Area_mm2, g = sub_data2$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data2$AR, g = sub_data2$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data2$Circ, g = sub_data2$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data2$Thickness, g = sub_data2$Week, method="bonferroni", list=TRUE)

## ILS/Hybrid ##

kruskal.test(sub_data3$LMK_PC1~sub_data3$Week)
kruskal.test(sub_data3$LMK_PC2~sub_data3$Week)
kruskal.test(sub_data3$EFD_PC1~sub_data3$Week)
kruskal.test(sub_data3$EFD_PC2~sub_data3$Week)

dunn.test(x = sub_data3$LMK_PC1, g = sub_data3$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data3$LMK_PC2, g = sub_data3$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data3$EFD_PC1, g = sub_data3$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data3$EFD_PC2, g = sub_data3$Week, method="bonferroni", list=TRUE)

kruskal.test(sub_data3$Petiole_Length_mm~sub_data3$Week)
kruskal.test(sub_data3$Area_mm2~sub_data3$Week)
kruskal.test(sub_data3$AR~sub_data3$Week)
kruskal.test(sub_data3$Circ~sub_data3$Week)
kruskal.test(sub_data3$Thickness~sub_data3$Week)

dunn.test(x = sub_data3$Petiole_Length_mm, g = sub_data3$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data3$Area_mm2, g = sub_data3$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data3$AR, g = sub_data3$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data3$Circ, g = sub_data3$Week, method="bonferroni", list=TRUE)
dunn.test(x = sub_data3$Thickness, g = sub_data3$Week, method="bonferroni", list=TRUE)