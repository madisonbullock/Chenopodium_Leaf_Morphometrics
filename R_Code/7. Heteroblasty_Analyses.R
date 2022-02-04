### Step 7. Heteroblasty Analyses ###
## This step separates that data set by subspecific class then visualizes the PCs (both landmark and EFD) as well as shape descriptors on scatterplots. Colors of plots are on a gradient to indicate week sample was collected. ##

# Install and load packages. #
install.packages("ggplot2")
library(ggplot2)

# Read in data and subset by subspecies identity. #
data <- read.table("master_data.txt", header=TRUE)
sub_data <- data[ which(data$subspecies=='Chenopodium_oahuense_ilioense'),]
sub_data2 <- data[ which(data$subspecies=='Chenopodium_oahuense_oahuense'),]
sub_data3 <- data[ which(data$subspecies=='ILS_Hybrid'),]

## Chenopodium oahuense ssp. ilioense. ##
# Visualize landmark PCA by heteroblasty. #

p <- ggplot(data=sub_data, aes(LMK_PC1, LMK_PC2, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="LMK_PC2", x = "LMK_PC1", colour = "Week")

p <- ggplot(data=sub_data, aes(LMK_PC3, LMK_PC4, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="LMK_PC4", x = "LMK_PC3", colour = "Week")

# Visualize Elliptical Fourier Descriptor PCA by heteroblasty. #

p <- ggplot(data=sub_data, aes(EFD_PC1, EFD_PC2, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="EFD_PC2", x = "EFD_PC1", colour = "Week")

p <- ggplot(data=sub_data, aes(EFD_PC3, EFD_PC4, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="EFD_PC4", x = "EFD_PC3", colour = "Week")

# Visualize Shape Descriptor Comparions by heteroblasty. #

p <- ggplot(data=sub_data, aes(Area, Thickness, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data, aes(Thickness, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data, aes(Area, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data, aes(Area, AR, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data, aes(AR, Circ, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data, aes(AR, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data, aes(AR, Thickness, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data, aes(Circ, Thickness, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data, aes(Circ, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

## Chenopodium oahuense ssp. oahuense ##
# Visualize landmark PCA by heteroblasty. #

p <- ggplot(data=sub_data2, aes(LMK_PC1, LMK_PC2, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="LMK_PC2", x = "LMK_PC1", colour = "Week")

p <- ggplot(data=sub_data2, aes(LMK_PC3, LMK_PC4, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="LMK_PC4", x = "LMK_PC3", colour = "Week")

# Visualize Elliptical Fourier Descriptor PCA by heteroblasty. #

p <- ggplot(data=sub_data2, aes(EFD_PC1, EFD_PC2, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="EFD_PC2", x = "EFD_PC1", colour = "Week")

p <- ggplot(data=sub_data2, aes(EFD_PC3, EFD_PC4, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="EFD_PC4", x = "EFD_PC3", colour = "Week")

# Visualize Shape Descriptor Comparions by heteroblasty. #

p <- ggplot(data=sub_data2, aes(Area, Thickness, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data2, aes(Thickness, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data2, aes(Area, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data2, aes(Area, AR, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data2, aes(AR, Circ, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data2, aes(AR, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data2, aes(AR, Thickness, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data2, aes(Circ, Thickness, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data2, aes(Circ, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

## ILS/Hybrid ##
# Visualize landmark PCA by heteroblasty. #

p <- ggplot(data=sub_data3, aes(LMK_PC1, LMK_PC2, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="LMK_PC2", x = "LMK_PC1", colour = "Week")

p <- ggplot(data=sub_data3, aes(LMK_PC3, LMK_PC4, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="LMK_PC4", x = "LMK_PC3", colour = "Week")

# Visualize Elliptical Fourier Descriptor PCA by heteroblasty. #

p <- ggplot(data=sub_data3, aes(EFD_PC1, EFD_PC2, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="EFD_PC2", x = "EFD_PC1", colour = "Week")

p <- ggplot(data=sub_data3, aes(EFD_PC3, EFD_PC4, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw() + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18) + labs(y ="EFD_PC4", x = "EFD_PC3", colour = "Week")

# Visualize Shape Descriptor Comparions by heteroblasty. #

p <- ggplot(data=sub_data3, aes(Area, Thickness, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data3, aes(Thickness, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data3, aes(Area, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data3, aes(Area, AR, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data3, aes(AR, Circ, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data3, aes(AR, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data3, aes(AR, Thickness, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data3, aes(Circ, Thickness, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)

p <- ggplot(data=sub_data3, aes(Circ, Petiole_Length, colour=Week))
p + geom_point(size=3, alpha=0.6) + theme_bw()  + scale_colour_gradient2(low="purple3",mid="slategray",high="green", midpoint=18)