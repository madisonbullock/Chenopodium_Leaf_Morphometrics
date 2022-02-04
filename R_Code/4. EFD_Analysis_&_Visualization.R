### Step 4. EFD PC Analysis and Visualization ###
## Calculates and visualizes mean leaf shape per category. Visualizes harmonic and PC contributions to shape variation. Compares PC variance based on class. ##

# Install and load packages. #
install.packages("shapes")
install.packages("spdep")
install.packages("ade4")
install.packages("ggplot2")
install.packages("gridExtra")

library(shapes)
library(spdep)
library(ade4)
library(ggplot2)
library(gridExtra)

# Install and read in older Momocs (v 0.2-6) package. Older version is required for this analysis. #
packageurl <- "https://cran.r-project.org/src/contrib/Archive/Momocs/Momocs_0.2-6.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
library(Momocs)

# Convert the .nef file from SHAPE (http://lbm.ab.a.u-tokyo.ac.jp/~iwata/shape/) to a COE object using the NEF2COE function. This function was written by Ryan Felice, as provided via Vincent Bonhomme. #
NEF2COE<-function (nef.path){
  nef <- readLines(nef.path)
  HARMO.l <- grep(pattern = "HARMO", nef)
  nb.h <- as.numeric(substring(nef[HARMO.l], 8))
  nef <- nef[-(1:HARMO.l)]
  nb.coo <- length(nef)/(nb.h + 1)
  coo.i <- 1:nb.coo
  coo.beg <- (coo.i - 1) * (nb.h + 1) + 1
  coo.end <- coo.beg + nb.h
  res <- matrix(NA, nrow = nb.coo, ncol = nb.h * 4,
                dimnames = list(nef[coo.beg], paste(rep(LETTERS[1:4], each = nb.h),
                                                    1:nb.h, sep = "")))
  for (i in seq(along = coo.i)) {
    nef.i <- nef[(coo.beg[i]+1):coo.end[i]]
    x <- as.numeric(unlist(strsplit(nef.i, " ")))
    x1<-x[!is.na(x)]
    a.i<-x1[seq(1,length(x1),4)]
    b.i<-x1[seq(2,length(x1),4)]
    c.i<-x1[seq(3,length(x1),4)]
    d.i<-x1[seq(4,length(x1),4)]
    res[i, ]<-c(a.i,b.i,c.i,d.i)
  }
  return(Coe(res,method="eFourier"))}

# Create a COE object from the .nef file. #
pass <- NEF2COE("NEF_master_data")

# Read in subspecies identities in the form of a single-column data file of all subspecies labels with the header being "type". #
species <- read.table("just_species.txt", header=TRUE)

 #Assign subspecies identities to the @fac slot in the pass COE object. #
pass@fac <- species

# Calculate mean leaf shapes. #
mean_subspecies <- meanShapes(pass)

# Visualize the mean shape for a subspecies by comparing the subspecies samples against themselves. #
tps.iso(mean_species$Chenopodium_oahuense_ilioense, mean_species$Chenopodium_oahuense_ilioense, cont=FALSE, iso.levels=0, shp.col=c("#18A51C"), shp.border=c("black"), shp.lwd=c(5,5))
tps.iso(mean_species$Chenopodium_oahuense_oahuense, mean_species$Chenopodium_oahuense_oahuense, cont=FALSE, iso.levels=0, shp.col=c("#5E33FF"), shp.border=c("black"), shp.lwd=c(5,5))
tps.iso(mean_species$ILS_Hybrid, mean_species$ILS_Hybrid, cont=FALSE, iso.levels=0, shp.col=c("#0027FF"), shp.border=c("black"), shp.lwd=c(5,5))

# Visualize the harmonic contributions to shape. #
hcontrib(pass, harm.range=1:20, amp.h=c(1,2,4,8,16,32,64))

# Run a PCA on NEF data. #
pass_pca <- pca(pass)

# Visualize the PC contributions to shape. #
PC.contrib(pass_pca, PC.r=1:4, sd=1, cols=NA, borders=c("dodgerblue","gray","orange"),lwd=5)

# Percent contributions of PCs to variance. #
sum_eig <- sum(pass_pca$eig)
percents <- (pass_pca$eig/sum_eig)*100

# Read in data. #
data <- read.table("master_data.txt", header=TRUE)

# Visualize Elliptical Fourier Descriptor PCA by subspecies. #
p1 <- ggplot(data, aes(EFD_PC1, EFD_PC2, colour=subspecies))
plot1 <- p1 + geom_point(size=3, alpha=0.6) + 
  theme_bw() + 
  labs(y ="EFD_PC2", x = "EFD_PC1", colour = "Subspecies") +
  scale_colour_manual(values=c("purple4","green4","blue")) +
  scale_x_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2)) + 
  scale_y_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2))

p2 <- ggplot(data, aes(EFD_PC3, EFD_PC4, colour=subspecies))
plot2 <- p2 + geom_point(size=3, alpha=0.6) + 
  theme_bw() + 
  scale_colour_brewer(type="qual", palette=2) + 
  labs(y ="EFD_PC4", x = "EFD_PC3", colour = "Subspecies") +
  scale_colour_manual(values=c("purple4","green4","blue")) +
  scale_x_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2)) + 
  scale_y_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2))

grid.arrange(plot1, plot2)

# Repeat above steps with other categories. #
# Population. #
population <- read.table("just_population.txt", header=TRUE)

pass@fac <- population

mean_population <- meanShapes(pass)

tps.iso(mean_locales$Big_Island, mean_locales$Big_Island, cont=FALSE, iso.levels=0, shp.col=c("#5E51FF"), shp.border=c("black"), shp.lwd=c(5,5))
tps.iso(mean_locales$Ilio_Point, mean_locales$Ilio_Point, cont=FALSE, iso.levels=0, shp.col=c("#12B6E3"), shp.border=c("black"), shp.lwd=c(5,5))
tps.iso(mean_locales$Maui, mean_locales$Maui, cont=FALSE, iso.levels=0, shp.col=c("#F23421"), shp.border=c("black"), shp.lwd=c(5,5))
tps.iso(mean_locales$Mokio_Point, mean_locales$Mokio_Point, cont=FALSE, iso.levels=0, shp.col=c("#0049FF"), shp.border=c("black"), shp.lwd=c(5,5))
tps.iso(mean_locales$Oahu, mean_locales$Oahu, cont=FALSE, iso.levels=0, shp.col=c("#FFB600"), shp.border=c("black"), shp.lwd=c(5,5))
tps.iso(mean_locales$Puu_Ka_Pele, mean_locales$Puu_Ka_Pele, cont=FALSE, iso.levels=0, shp.col=c("#1DA100"), shp.border=c("black"), shp.lwd=c(5,5))

p1 <- ggplot(data, aes(EFD_PC1, EFD_PC2, colour=population))
plot1 <- p1 + geom_point(size=3, alpha=0.6) + 
  theme_bw() + 
  scale_colour_brewer(type="qual", palette=2) + 
  labs(y ="EFD_PC2", x = "EFD_PC1", colour = "Population") +
  scale_colour_manual(values=c("purple4","cyan4","blue","red","chocolate3","green4","magenta")) +
  scale_x_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2)) + 
  scale_y_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2))

p2 <- ggplot(data, aes(EFD_PC3, EFD_PC4, colour=population))
plot2 <- p2 + geom_point(size=3, alpha=0.6) + 
  theme_bw() + 
  scale_colour_brewer(type="qual", palette=2) + 
  labs(y ="EFD_PC4", x = "EFD_PC3", colour = "Population") +
  scale_colour_manual(values=c("purple4","cyan4","blue","red","chocolate3","green4","magenta")) +
  scale_x_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2)) + 
  scale_y_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2))

grid.arrange(plot1, plot2)

# Island. #
island <- read.table("just_island.txt", header=TRUE)

pass@fac <- island

mean_island <- meanShapes(pass)

tps.iso(mean_island$Big_Island, mean_island$Big_Island, cont=FALSE, iso.levels=0, shp.col=c("#EDD83D"), shp.border=c("black"), shp.lwd=c(5,5))
tps.iso(mean_island$Molokai, mean_island$Molokai, cont=FALSE, iso.levels=0, shp.col=c("#6AB547"), shp.border=c("black"), shp.lwd=c(5,5))
tps.iso(mean_island$Maui, mean_island$Maui, cont=FALSE, iso.levels=0, shp.col=c("#011502"), shp.border=c("black"), shp.lwd=c(5,5))
tps.iso(mean_island$Oahu, mean_island$Oahu, cont=FALSE, iso.levels=0, shp.col=c("#74226C"), shp.border=c("black"), shp.lwd=c(5,5))


p1 <- ggplot(data, aes(EFD_PC1, EFD_PC2, colour=island))
plot1 <- p1 + geom_point(size=3, alpha=0.6) + 
  theme_bw() + 
  scale_colour_brewer(type="qual", palette=2) + 
  labs(y ="EFD_PC2", x = "EFD_PC1", colour = "Island") +
  scale_colour_manual(values=c("purple4","green4","blue","red")) +
  scale_x_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2)) + 
  scale_y_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2))

p2 <- ggplot(data, aes(EFD_PC3, EFD_PC4, colour=island))
plot2 <- p2 + geom_point(size=3, alpha=0.6) + 
  theme_bw() + 
  scale_colour_brewer(type="qual", palette=2) + 
  labs(y ="EFD_PC4", x = "EFD_PC3", colour = "Island") +
  scale_colour_manual(values=c("purple4","green4","blue","red")) +
  scale_x_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2)) + 
  scale_y_continuous(breaks=seq(-0.2, 0.2, by = 0.1), limits = c(-0.2, 0.2))

grid.arrange(plot1, plot2)